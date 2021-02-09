# 2. Text analysis

# FOLLOW THIS: https://cran.r-project.org/web/packages/RNewsflow/vignettes/RNewsflow.html

## empty memory (!)
rm(list=ls())

#set wd: 
setwd("C:/Users/lse043/OneDrive - University of Bergen/Documents/0 PhD PROJECT/PAPER 2 'Strategies'/Ghana_RTI_media_advocacy")

# reading dataframe(s) ####

# the merged dataframe
df <- readRDS("dataframe_merged.rds")

# loading all the dataframes (df_gw, text, df)
#load("dataframes.RData")

# making corpus ####

library(quanteda)
corp <- corpus(df, docid_field = 'id', text_field='text')
dtm <- dfm(corp)

# 1. Pre-processing texts and creating the DTM ####

dtm
#Document-feature matrix of: 394 documents, 12,031 features (97.9% sparse) and 7 docvars.

head(docvars(dtm)) 
tail(docvars(dtm)) 


#### pre-processing here ####


# ANALYZING (RNewsflow) #### 
library(RNewsflow)

dtm
head(docvars(dtm), 3)

# 2. Using word statistics to filter and weight the DTM ####

#filter out words that are evenly used over time: 
tdd <- term_day_dist(dtm)
tail(tdd, 3)
head(tdd, 3)
#days.entropy:  tells us whether the occurrence of a word over time is 
#evenly distributed (high entropy) or concentrated (low entropy)
#days.entropy.norm score normalizes the entropy by dividing by the number of days

#to see everything
tdd_df <- data.frame(tdd)
View(tdd)

#By selecting the terms with low entropy scores, the DTM can be filtered 
#by using the selected terms as column values:
#Note: also a good automatic approach for filtering out stopwords, boilerplate words, and word forms such as articles and common verbs
#basically deleting: 

#don't want to do this: (would potentially remove common words, such as rti, bill, etc.)
#select_terms <- tdd$term[tdd$days.entropy.norm <= 0.3]
#dtm <- dtm[,select_terms] # no change, no deletion: change the threshold - from dataframe: highest norm score=0.04160406
#dtm

#weighting: 
#give more weight to rare words than common words
#classic weighting scheme and recommended standard: the term-frequency inverse document frequency (tf.idf)
dtm2 <- quanteda::dfm_tfidf(dtm)


# 3. Calculating document similarities ####
docvars(dtm2)

g <- newsflow_compare(dtm, date_var='date',
                     hour_window = c(0,744),  #window set to a month
                     min_similarity = 0.2)  # in model, set to 0.4

g2 <- newsflow_compare(dtm2, date_var='date',
                      hour_window = c(0,744),  #need to alter window
                      min_similarity = 0.2)  # in model, set to 0.4

#inspecting document (vertices) and document pairs (edges) and attributes: 

V(g)$source_type
E(g)$hourdiff

vertex_sourcetype = V(g)$source_type
edge_hourdiff = E(g)$hourdiff

head(vertex_sourcetype)
head(edge_hourdiff)

vertex_date = V(g)$date
head(vertex_date)

# as dataframe: 

v <- as_data_frame(g2, 'vertices')
e <- as_data_frame(g2, 'edges')

head(v[,c('name','date','source','source_type')],3)
head(e,3) # weight represents the similarity score

# plotting histogram:
hist(E(g)$hourdiff, main='Time distance of document pairs', 
     xlab = 'Time difference in hours', breaks = 150, right=F)

#looks much better (with the weighting above): 
hist(E(g2)$hourdiff, main='Time distance of document pairs', 
     xlab = 'Time difference in hours', breaks = 150, right=F)

# saving with 1000 x 550 



## demo =========================
dtm_rn = rnewsflow_dfm ## copy the demo data --- NB !
dtm_rn

docvars(dtm_rn)

# WIP: What does this "sparse" mean? I had 97.9% sparse - must be good, no?
#Document-feature matrix of: 1,754 documents, 6,968 features (99.6% sparse) and 3 docvars.

head(docvars(dtm_rn), 3) # only the first 3


# 2. Using word statistics to filter and weight the DTM ####
tdd_rn = term_day_dist(dtm_rn)
tail(tdd_rn, 3)

select_terms = tdd_rn$term[tdd_rn$days.entropy.norm <= 0.3]
dtm_rn2 = dtm_rn[,select_terms]

dtm_rn3 = quanteda::dfm_tfidf(dtm_rn2)

# 3. Calculating document similarities #####
g_rn3 = newsflow_compare(dtm_rn3, date_var='date',
                     hour_window = c(0,36), 
                     min_similarity = 0.4)

V(g_rn3)$sourcetype

vertex_sourcetype_rn = V(g_rn3)$sourcetype
edge_hourdiff_rn = E(g_rn3)$hourdiff

head(vertex_sourcetype)

head(edge_hourdiff)

v = as_data_frame(g, 'vertices')
e = as_data_frame(g, 'edges')

head(v[,c('name','date','source','sourcetype')],3)

head(e,3)

hist(E(g)$hourdiff, main='Time distance of document pairs', 
     xlab = 'Time difference in hours', breaks = 150, right=F)

# saving workspace ####

# Save my entire workspace:
save.image(file = "my_work_space.RData")

#To restore:
#load("my_work_space.RData")

