###############################################################################
#  Purpose:  Enable identification of common Medical Device Adverse Events by
#            extracting MDR text information, performing NLP, and clustering
#            events with the DBSCAN unsupervised learning algorithm.
#
#  Author:  Aaron Boussina, Hedral Inc.
#
#  Inputs:  openFDA API, user queries
#
#  Output: An Rshiny plotly output with clustered events
#
# Revision History:
# AB 27OCT2020:  N/A, Initial Release.
###############################################################################

# Import Libraries
library(shiny)
library(shinydashboard)
library(lubridate)
library(httr)
library(jsonlite)
library(magrittr)
library(dplyr)
library(tm)
library(conflicted)
library(SnowballC)
library(dbscan)
library(plotly)
library(proxy)
library(stringr)
library(RColorBrewer)

# Determine Default Value for Date Range Input
# One year look-back will be utilized
startDate <- Sys.Date() - years(2)

ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(
    title = "MDR AE Clustering"
  ),

  dashboardSidebar(
    dateRangeInput(
      inputId = "dateRange",
      label = "Enter a Date Range",
      start = startDate
    ),

    textInput(
      inputId = "companyName",
      label = "Enter a Company Name (Optional)",
      placeholder = "Roche"
    ),

    textInput(
      inputId = "deviceName",
      label = "Enter a Product Name (Optional)",
      placeholder = "Accu-check"
    ),

    actionButton(
      inputId = "search",
      label = "Search",
      style = "background-color: rgba(55,200,55, 1); color: white;"
    ),

    # HTML Styling
    tags$hr(),
    tags$h3("Info"),
    tags$p("This program calls the openFDA Medical Device Adverse Events API 
      with the given input arguments and uses Natural Language Processing and
      unsupervised machine learning to cluster similar events."),
    tags$p("Note: Queries are limited to the first 1000 results and top 8 
           clusters.  Events are omitted if they can not be clustered."),
    tags$hr(),
    tags$h3("Stats:"),
    textOutput(outputId = "numEventsOut"),
    textOutput(outputId = "numClustOut")
  ),

  dashboardBody(
    plotlyOutput(outputId = "plot", height = "60em")
  )
)

server <- function(input, output) {

  # Create object for loading bar
  progress <- Progress$new()
  progress$set(message = "Awaiting User Input ...", value = 0)
  output$numEventsOut <- renderText("No results to display.")

  # Upon clicking the Search button, call the openFDA API and process the data
  observeEvent(
    input$search,
    {

      # Set initial info data for display
      output$numEventsOut <- renderText("Events Found: 0")
      output$numClustOut <- renderText("Clusters Generated: 0")

      # Construct a query string using the User Inputs
      companyQuery <- ""
      deviceQuery <- ""

      if (input$companyName != "") {
        companyQuery <- paste0(
          "+AND+device.manufacturer_d_name:",
          input$companyName
        )
      }

      if (input$deviceName != "") {
        deviceQuery <- paste0(
          "+AND+device.brand_name:",
          input$deviceName
        )
      }

      openFDA_query <- paste0(
        "https://api.fda.gov/device/event.json?search=date_received:[",
        input$dateRange[1], "+TO+", input$dateRange[2], "]",
        companyQuery, deviceQuery, "&limit=1000"
      )

      # Call the openFDA API and extract the MDR text
      progress$set(message = "Connecting to openFDA ...", value = 0.1)
      openFDA_response <- GET(openFDA_query)

      if (!http_error(openFDA_response)) {
        openFDA_response_Results <- httr::content(openFDA_response, as = "text") %>%
          fromJSON() %$% results

        numInitialEvents <- openFDA_response_Results %>%
          nrow()

        openFDA_response_txt <- openFDA_response_Results %$% mdr_text %>%
          bind_rows(.id = "dfId") %>%
          select(dfId, mdr_text_key, text_type_code, text)

        # Remove the Additional Manufacturer Narrative from the Response
        # This is a subjective choice to ensure clustering is only based on
        # the event and not common response phrasing.
        textMDR <- subset(
          openFDA_response_txt,
          text_type_code != "Additional Manufacturer Narrative"
        ) %>%
          select(doc_id = mdr_text_key, text)

        numEvents <- textMDR %$% doc_id %>%
          unique() %>%
          length()

        # Create the corpus and perform initial text preprocessing
        progress$set(message = "Performing text processing ...", value = 0.2)
        docsMDR <- Corpus(DataframeSource(textMDR)) %>%
          tm_map(removeNumbers) %>%
          tm_map(removePunctuation) %>%
          tm_map(content_transformer(tolower)) %>%
          tm_map(removeWords, stopwords("english")) %>%
          tm_map(stemDocument) %>%
          tm_map(stripWhitespace)

        # Create the Document Term Matrix with TF-IDF weighting
        dtmMDR <- DocumentTermMatrix(docsMDR,
          control = list(weighting = weightTfIdf)
        ) %>%
          as.matrix() %>%
          proxy::dist(method = "cosine")

        # Use the DBSCAN clustering algorithm to cluster the dtmMDR values
        # Manual hyperparemeter fine-tuning is being used to ensure clusters
        # are still generated for small numbers of adverse events, while being
        # specific enough for large numbers
        progress$set(message = "Running DBSCAN Clustering ...", value = 0.5)

        dbMDR <- dbscan(dtmMDR,
          minPts = 3,
          eps = min(2.5 / log(numEvents), 0.8)
        )

        # Use Principle Component Analysis to visualize on XY plane
        progress$set(
          message = "Performing Principle Component Analysis ...",
          value = 0.7
        )
        pcaMDR <- prcomp(dtmMDR, rank = 2) %$% x %>%
          as.data.frame()
        pcaMDR$cluster <- dbMDR$cluster
        pcaMDR$textMDR <- textMDR$text

        # Remove outliers and limit  the top 8 clusters
        topClusters <- pcaMDR %>%
          count(cluster) %>%
          slice_max(n = 9, n, with_ties = FALSE) %>%
          subset(cluster > 0) %>%
          {
            if (nrow(.) > 0) {
              mutate(., newCluster = as.character(c(1:nrow(.))))
            } else {
              .
            }
          }

        numClust <- topClusters %>%
          nrow()

        pcaMDR <- pcaMDR %>%
          inner_join(topClusters, by = "cluster")

        if (nrow(pcaMDR) > 0) {
          # Generate the output plot
          progress$set(message = "Plotting ...", value = 0.9)
          plotMDR <- plot_ly(pcaMDR,
            x = ~PC1,
            y = ~PC2,
            color = ~newCluster,
            colors = "Set2",
            type = "scatter",
            mode = "markers",
            size = 20,
            text = ~ paste(str_wrap(textMDR))
          )
          output$plot <- renderPlotly({
            plotMDR
          })
        }
        else {
          output$plot <- renderPlotly({})
        }


        progress$set(message = "Done.", value = 1)
        output$numEventsOut <- renderText(
          paste0("Events Found: ", numInitialEvents)
        )
        output$numClustOut <- renderText(
          paste0("Clusters Generated: ", numClust)
        )
      }
      else {
        output$numEventsOut <- renderText("No query results found.")
        output$numClustOut <- renderText("")
        progress$set(message = "Awaiting User Input ...", value = 1)
        output$plot <- renderPlotly({
        })
      }
    }
  )
}

shinyApp(ui, server)
