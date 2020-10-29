# Objective 
The FDA requires medical device & pharmaceutical manufacturers to track and trend post market complaints and nonconformances as part of their Quality Management Systems (see "Guidance for Industry Quality Systems Approach to Pharmaceutical CGMP Regulations" and "Postmarket Surveillance Under Section 522 of the Federal Food, Drug, and Cosmetic Act").  Due to the sheer number of complaints/NCRs (Nonconformance Reports) processed, however, occurence rates can be difficult to accurately calculate and manufacturers often depend on broad symptom codes which may not accurately detect trends.  Therefore, there is potential benefit in using natural language processing (NLP) to cluster similar events based on event description.
</br>
</br>
This Rshiny dashboard allows a user to investigate clusters of MDR (Medical Device Report) Adverse Events present in the MAUDE (Manufacturer and User Facility Device Experience) database.  This enables identification of the most frequent recurring events for a given date range, manufacturer, and/or product.  The application calls the openFDA API with user query arguments and processes the event description text.  Clustering is done with unsupervised machine learning, and the results are displayed graphically in 2D with Principal Component Analysis.
</br>
</br>
You can use the dashboard from your browser here:  https://aboussina.shinyapps.io/DBSCAN-Clustering-for-MDR-Text/, or clone the repo and run app.R within RStudio.
</br>
</br>
![Dashboard Example GIF](https://i.imgur.com/kplT9VJ.gif)
</br>
</br>
<strong>FDA Disclaimer on MDR data: </strong> "Although MDRs are a valuable source of information, this passive surveillance system has limitations, including the potential submission of incomplete, inaccurate, untimely, unverified, or biased data. In addition, the incidence or prevalence of an event cannot be determined from this reporting system alone due to potential under-reporting of events and lack of information about frequency of device use. Because of this, MDRs comprise only one of the FDA's several important postmarket surveillance data sources."
</br>
</br>

# Methodology
This R application perfoms NLP using the Text Mining library (tm).  The workflow is standard and involves the following steps:
<br/>

1.  MDR text is set to lower case and numbers, punctuation, whitespace, and stopwords are removed.  In addition, words are stemmed using Porter's algorithm. <br/>
```R
    docsMDR <- Corpus(DataframeSource(textMDR)) %>%  
      tm_map(removeNumbers) %>%  
      tm_map(removePunctuation) %>%  
      tm_map(content_transformer(tolower)) %>%  
      tm_map(removeWords, stopwords("english")) %>%  
      tm_map(stemDocument) %>%  
      tm_map(stripWhitespace) 
```
<br/>

2.  A Document Term Matrix is created using term frequency-inverse document frequency weighting (TF-IDF) and a distance matrix is created using Cosine dissimilarity.  Other distance methods were tried (e.g. Euclidean) but cosine appeared to generate the most accurate clustering. <br/>
```R
    dtmMDR <- DocumentTermMatrix(docsMDR,  
      control = list(weighting = weightTfIdf)  
    ) %>%  
      as.matrix() %>%  
      proxy::dist(method = "cosine") 
```
<br/>

3.  Clustering is performed with the DBSCAN algorithm.  k-Means was also tested, but produced undesirable results due to its inclusion of outliers (see image below).  Since the use-case for this dashboard is to identify frequent clustering, outliers can be ignored and a density-based algorithm (such as DBSCAN) yields better results.  Determination of the  epsilon neighborhood size hyperparameter (eps) was done through manual tuning.  Use of kNNdist (k-Nearest Neighbor Distance) could have been used to optimize the results, but would have resulted in slower processing and a reduction in dashboard user experience.  As expected, smaller MDR sets needed greater values of eps to ensure clustering was sensitive enough while larger MDR sets needed smaller values of eps to guarentee specificity.  Thus, a heuristic of `eps = min(2.5 / log(numEvents), 0.8)` was used. eps was capped at 0.8 since anything greater resulted in massive overclustering. <br/>
```R
    dbMDR <- dbscan(dtmMDR,  
      minPts = 3,  
      eps = min(2.5 / log(numEvents), 0.8)  
    ) 
```
<br/>
<br/>
Undesired clustering with k-Means:
<br/>
<img src="https://i.imgur.com/jlH2RCk.png" alt="k-Means Undesired Clustering" width="50%">  
<br/>

4.  The identified clusters are mapped onto the original term matrix and the results are visualized in 2D following principal component analysis.  Results are limited to the largest 8 clusters to satisfy the use-case of trend identification. <br/>
```R
    pcaMDR <- prcomp(dtmMDR, rank = 2) %$% x %>%  
      as.data.frame()
```