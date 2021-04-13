# 2A. Text analysis: Making news content variation histograms

## empty memory (!)
rm(list=ls())

#set wd: 
setwd("C:/Users/lse043/OneDrive - University of Bergen/Documents/0 PhD PROJECT/PAPER 2 'Strategies'/Ghana_RTI_media_advocacy")

# loading all the dataframes (df_gw, text, df)
load("dataframes.RData")

str(df_gw)
df_gw$year <- format(df_gw$date, format = "%Y")

# Making corpus per year ####
library(quanteda)
corp <- corpus(df_gw, docid_field = 'text_id', text_field='text')

head(docvars(corp))
ndoc(corp)

# Pre-processing texts and creating the DTM ####
#Document-feature matrix of: 394 documents, 12,031 features (97.9% sparse) and 7 docvars.

dtm <- dfm(corp,
           tolower = TRUE,
           stem = TRUE,
           remove = stopwords("english"),
           remove_punct = TRUE,
           remove_numbers = TRUE)

#NB: STOPWORDS! #remove = stopwords("english"),

dtm
#Document-feature matrix of: 374 documents, 7,651 features (97.1% sparse) and 8 docvars

#Not grouping the dfm, that aggregates: 
#Document-feature matrix of: 14 documents, 7,651 features (78.6% sparse) and 5 docvars

# subsetting by year: 
dtm_2020 <- dfm_subset(dtm, year == 2020)
dtm_2019 <- dfm_subset(dtm, year == 2019)
dtm_2018 <- dfm_subset(dtm, year == 2018)
dtm_2017 <- dfm_subset(dtm, year == 2017)
dtm_2016 <- dfm_subset(dtm, year == 2016)
dtm_2015 <- dfm_subset(dtm, year == 2015)
dtm_2014 <- dfm_subset(dtm, year == 2014)
dtm_2013 <- dfm_subset(dtm, year == 2013)
dtm_2012 <- dfm_subset(dtm, year == 2012)
dtm_2011 <- dfm_subset(dtm, year == 2011)
dtm_2010 <- dfm_subset(dtm, year == 2010)
dtm_2009 <- dfm_subset(dtm, year == 2009)
dtm_2008 <- dfm_subset(dtm, year == 2008)
dtm_2007 <- dfm_subset(dtm, year == 2007)
dtm_2006 <- dfm_subset(dtm, year == 2006)
dtm_2005 <- dfm_subset(dtm, year == 2005)

#inspecting
head(docvars(dtm_2010), 3)
tail(docvars(dtm_2010), 3) 


# NB: NOT, but SHOULD: filter and weight the DTM (later) ####
library(RNewsflow)

# Compare with RNewsflow #### 

# calculating document similarities (THE MODEL:)
g <- newsflow_compare(dtm, date_var='date',
                      hour_window = c(0.1,744),  # window set to a month
                      min_similarity = 0.4)  # in model, set to 0.4

#the window from "0.1" removes news stories published the same day 

#g_2020 <- newsflow_compare(dtm_2020, date_var='date', hour_window = c(0.1,744), min_similarity = 0.4)
g_2019 <- newsflow_compare(dtm_2019, date_var='date', hour_window = c(0.1,744), min_similarity = 0.4, only_complete_window = F)
g_2018 <- newsflow_compare(dtm_2018, date_var='date', hour_window = c(0.1,744), min_similarity = 0.4, only_complete_window = F)
g_2017 <- newsflow_compare(dtm_2017, date_var='date', hour_window = c(0.1,744), min_similarity = 0.4, only_complete_window = F)
g_2016 <- newsflow_compare(dtm_2016, date_var='date', hour_window = c(0.1,744), min_similarity = 0.4, only_complete_window = F)
#g_2015 <- newsflow_compare(dtm_2015, date_var='date', hour_window = c(0.1,744), min_similarity = 0.4, only_complete_window = F)
g_2014 <- newsflow_compare(dtm_2014, date_var='date', hour_window = c(0.1,744), min_similarity = 0.4, only_complete_window = F)
g_2013 <- newsflow_compare(dtm_2013, date_var='date', hour_window = c(0.1,744), min_similarity = 0.4, only_complete_window = F)
g_2012 <- newsflow_compare(dtm_2012, date_var='date', hour_window = c(0.1,744), min_similarity = 0.4, only_complete_window = F)
g_2011 <- newsflow_compare(dtm_2011, date_var='date', hour_window = c(0.1,744), min_similarity = 0.4, only_complete_window = F)
g_2010 <- newsflow_compare(dtm_2010, date_var='date', hour_window = c(0.1,744), min_similarity = 0.4, only_complete_window = F)
g_2009 <- newsflow_compare(dtm_2009, date_var='date', hour_window = c(0.1,744), min_similarity = 0.4, only_complete_window = F)
g_2008 <- newsflow_compare(dtm_2008, date_var='date', hour_window = c(0.1,744), min_similarity = 0.4, only_complete_window = F)
#g_2007 <- newsflow_compare(dtm_2007, date_var='date', hour_window = c(0.1,744), min_similarity = 0.4, only_complete_window = F)
#g_2006 <- newsflow_compare(dtm_2006, date_var='date', hour_window = c(0.1,744), min_similarity = 0.4, only_complete_window = F)
#g_2005 <- newsflow_compare(dtm_2005, date_var='date', hour_window = c(0.1,744), min_similarity = 0.4, only_complete_window = F)

# drop 2020 and 2015
# also loose: 2007, 2006, 2005

# plotting histograms:
#all:
par(mfrow=c(4,3))

#post and pre 2016:
par(mfrow=c(2,2))
#par(mfrow=c(2,2),mar = c(2, 2, 2, 2))
#par(mfrow=c(3,3),mar = c(1, 1, 1, 1))
#par(mfrow = c(2, 2),mar = c(.5, .5, 1, .5))
par()

g_2019 <- newsflow_compare(dtm_2019, date_var='date', hour_window = c(0.1,744), min_similarity = 0.3, only_complete_window = F)
g_2018 <- newsflow_compare(dtm_2018, date_var='date', hour_window = c(0.1,744), min_similarity = 0.3, only_complete_window = F)
g_2017 <- newsflow_compare(dtm_2017, date_var='date', hour_window = c(0.1,744), min_similarity = 0.3, only_complete_window = F)
g_2016 <- newsflow_compare(dtm_2016, date_var='date', hour_window = c(0.1,744), min_similarity = 0.3, only_complete_window = F)


#full:
#hist(E(g_2019)$hourdiff, main='2019', xlab = 'Time difference in hours', breaks = 150, right=F)

hist(E(g_2019)$hourdiff, main='2019', xlab = 'Time difference in hours', breaks = 150, right=F)

hist(E(g_2018)$hourdiff, main='2018', xlab = 'Time difference in hours', breaks = 150, right=F)

hist(E(g_2017)$hourdiff, main='2017', xlab = 'Time difference in hours', breaks = 150, right=F)

hist(E(g_2016)$hourdiff, main='2016', xlab = 'Time difference in hours', breaks = 150, right=F)

# saving : 1700 x 800

par(mfrow=c(3,3))

hist(E(g_2014)$hourdiff, main='2014 (N=19)', xlab = 'Time difference in hours', breaks = 150, right=F)

hist(E(g_2013)$hourdiff, main='2013 (N=15)', xlab = 'Time difference in hours', breaks = 150, right=F)

hist(E(g_2012)$hourdiff, main='2012 (N=12)', xlab = 'Time difference in hours', breaks = 150, right=F)

hist(E(g_2011)$hourdiff, main='2011 (N=6)', xlab = 'Time difference in hours', breaks = 150, right=F)

hist(E(g_2010)$hourdiff, main='2010 (N=10)', xlab = 'Time difference in hours', breaks = 150, right=F)

hist(E(g_2009)$hourdiff, main='2009 (N=12)', xlab = 'Time difference in hours', breaks = 150, right=F)

hist(E(g_2008)$hourdiff, main='2008 (N=8)', xlab = 'Time difference in hours', breaks = 150, right=F)

# saving : 1250 x 800

#restore plot frame:
par()


## numbers for methods section ####


# to get numbers for table in methods section: (but not necessary for the following)
#corp_2020 <- corpus_subset(corp, year == 2020)
#corp_2019 <- corpus_subset(corp, year == 2019)
#corp_2018 <- corpus_subset(corp, year == 2018)
#corp_2005 <- corpus_subset(corp, year == 2005)
#ndoc(corp_2005)


#### notes for inspecting E and V ####

#inspecting document (vertices) and document pairs (edges) and attributes: 
V(g)$source_type
E(g)$hourdiff
vertex_sourcetype = V(g)$source_type
edge_hourdiff = E(g)$hourdiff
head(vertex_sourcetype, 20)
head(edge_hourdiff, 20)
vertex_date = V(g)$date
head(vertex_date, 20)
# as dataframe: 
v <- as_data_frame(g, 'vertices')
e <- as_data_frame(g, 'edges')
View(v) # a lot of duplicates 
head(v[,c('name','date','source','source_type')],3)
head(e,3) # weight represents the similarity score

