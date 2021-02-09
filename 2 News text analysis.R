# 2. Text analysis

## empty memory (!)
rm(list=ls())

#set wd: 
setwd("C:/Users/lse043/OneDrive - University of Bergen/Documents/0 PhD PROJECT/PAPER 2 'Strategies'/Ghana_RTI_media_advocacy")

#importing text data: 
gw_374 <- readRDS("GhanaWeb_374_(190620).rds")

#inspecting
gw_374[1]


# FOLLOW THIS: https://cran.r-project.org/web/packages/RNewsflow/vignettes/RNewsflow.html

# 0. Making dataframe ####
library (plyr)
df <- ldply (gw_374, data.frame)

View(df)

str(df$title)
str(df$date)
str(df$text)

#unique docnames for corpus function: 
df$id <- 1:nrow(df)

library(quanteda)
corp = corpus(df, docid_field = 'id', text_field='text')
dtm = dfm(corp)

# 1. Pre-processing texts and creating the DTM ####

dtm
head(docvars(dtm)) # 2 docvars: title and date

#### pre-processing here ####

# RNewsflow analysis
library(RNewsflow)

dtm = rnewsflow_dfm ## copy the demo data --- NB !
dtm

# WIP: What does this "sparse" mean? I had 97.9% sparse - must be good, no?
#Document-feature matrix of: 1,754 documents, 6,968 features (99.6% sparse) and 3 docvars.

head(docvars(dtm), 3) # only the first 3


# 2. Using word statistics to filter and weight the DTM ####
tdd = term_day_dist(dtm)
tail(tdd, 3)

select_terms = tdd$term[tdd$days.entropy.norm <= 0.3]
dtm = dtm[,select_terms]

dtm = quanteda::dfm_tfidf(dtm)

# 3. Calculating document similarities #####
g = newsflow_compare(dtm, date_var='date',
                     hour_window = c(0,36), 
                     min_similarity = 0.4)

vertex_sourcetype = V(g)$sourcetype
edge_hourdiff = E(g)$hourdiff

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

