# 2C. Text analysis: Textreuse with press statements

## empty memory (!)
rm(list=ls())

#set wd: 
setwd("C:/Users/lse043/OneDrive - University of Bergen/Documents/0 PhD PROJECT/PAPER 2 'Strategies'/Ghana_RTI_media_advocacy")

# loading all the dataframes (df_gw, text, df)
load("dataframes.RData")

str(df)

# 1. adding grouping variables ####
library(lubridate)
str(df$date)

# by press statement
library(dplyr)
df$group_ps <- df$date
df$group_ps
str(df$group_ps)

#to get press statement dates
str(text$date)
text$date

# creating grouping variables (v's)
df <- df %>% 
  mutate(v1 = ifelse(group_ps >= "2010-02-15 01:00:00" & group_ps < "2012-05-10 01:00:00", 1, 0),
         v2 = ifelse(group_ps >= "2012-05-10 01:00:00" & group_ps < "2012-05-22 01:00:00", 1, 0),
         v3 = ifelse(group_ps >= "2012-05-22 01:00:00" & group_ps < "2012-07-19 01:00:00", 1, 0),
         v4 = ifelse(group_ps >= "2012-07-19 01:00:00" & group_ps < "2012-07-20 01:00:00", 1, 0),
         v5 = ifelse(group_ps >= "2012-07-20 01:00:00" & group_ps < "2012-10-22 01:00:00", 1, 0),
         v6 = ifelse(group_ps >= "2012-10-22 01:00:00" & group_ps < "2013-08-01 01:00:00", 1, 0),
         v7 = ifelse(group_ps >= "2013-08-01 01:00:00" & group_ps < "2013-02-06 00:00:00", 1, 0),
         v8 = ifelse(group_ps >= "2013-02-06 00:00:00" & group_ps < "2013-04-30 00:00:00", 1, 0),
         v9 = ifelse(group_ps >= "2013-04-30 00:00:00" & group_ps < "2013-05-31 00:00:00", 1, 0),
         v10 = ifelse(group_ps >= "2013-05-31 00:00:00" & group_ps < "2013-09-27 00:00:00", 1, 0),
         v11 = ifelse(group_ps >= "2013-09-27 00:00:00" & group_ps < "2015-09-28 00:00:00", 1, 0),
         v12 = ifelse(group_ps >= "2015-09-28 00:00:00" & group_ps < "2016-04-15 00:00:00", 1, 0),
         v13 = ifelse(group_ps >= "2016-04-15 00:00:00" & group_ps < "2016-07-28 00:00:00", 1, 0),
         v14 = ifelse(group_ps >= "2016-07-28 00:00:00" & group_ps < "2016-08-18 00:00:00", 1, 0),
         v15 = ifelse(group_ps >= "2016-08-18 00:00:00" & group_ps < "2016-09-23 00:00:00", 1, 0),
         v16 = ifelse(group_ps >= "2016-09-23 00:00:00" & group_ps < "2017-02-01 00:00:00", 1, 0),
         v17 = ifelse(group_ps >= "2017-02-01 00:00:00" & group_ps < "2018-09-28 00:00:00", 1, 0),
         v18 = ifelse(group_ps >= "2018-09-28 00:00:00" & group_ps < "2019-03-27 00:00:00", 1, 0),
         v19 = ifelse(group_ps >= "2019-03-27 00:00:00" & group_ps < "2019-04-15 00:00:00", 1, 0),
         v20 = ifelse(group_ps >= "2019-04-15 00:00:00", 1, 0))

df$group_ps <- as.numeric(df$group_ps)
df$group_ps <- 0
str(df$group_ps)
df$group_ps[df$v1 == 1] <- 1
df$group_ps[df$v2 == 1] <- 2
df$group_ps[df$v3 == 1] <- 3
df$group_ps[df$v4 == 1] <- 4
df$group_ps[df$v5 == 1] <- 4 # NB: combining group 4 and 5 
df$group_ps[df$v6 == 1] <- 5
df$group_ps[df$v7 == 1] <- 6
df$group_ps[df$v8 == 1] <- 7
df$group_ps[df$v9 == 1] <- 8
df$group_ps[df$v10 == 1] <- 9
df$group_ps[df$v11 == 1] <- 10
df$group_ps[df$v12 == 1] <- 11
df$group_ps[df$v13 == 1] <- 12
df$group_ps[df$v14 == 1] <- 13
df$group_ps[df$v15 == 1] <- 14
df$group_ps[df$v16 == 1] <- 15
df$group_ps[df$v17 == 1] <- 16
df$group_ps[df$v18 == 1] <- 17
df$group_ps[df$v19 == 1] <- 18
df$group_ps[df$v20 == 1] <- 19

#View(df)

#### calculating time difference ####

df$ps_date <- df$date
str(df$ps_date)
df$ps_date[df$group_ps == 1] <- text$date[1]
df$ps_date[df$group_ps == 2] <- text$date[2]
df$ps_date[df$group_ps == 3] <- text$date[3]
df$ps_date[df$group_ps == 4] <- text$date[4]
df$ps_date[df$group_ps == 5] <- text$date[6] # Note mismatch (4/5)
df$ps_date[df$group_ps == 6] <- text$date[7]
df$ps_date[df$group_ps == 7] <- text$date[8]
df$ps_date[df$group_ps == 8] <- text$date[9]
df$ps_date[df$group_ps == 9] <- text$date[10]
df$ps_date[df$group_ps == 10] <- text$date[11]
df$ps_date[df$group_ps == 11] <- text$date[12]
df$ps_date[df$group_ps == 12] <- text$date[13]
df$ps_date[df$group_ps == 13] <- text$date[14]
df$ps_date[df$group_ps == 14] <- text$date[15]
df$ps_date[df$group_ps == 15] <- text$date[16]
df$ps_date[df$group_ps == 16] <- text$date[17]
df$ps_date[df$group_ps == 17] <- text$date[18]
df$ps_date[df$group_ps == 18] <- text$date[19]
df$ps_date[df$group_ps == 19] <- text$date[20]

# count days variables: 
# time1 = df$date
str(df$date)
# time2 = df$ps_date
str(df$ps_date)
# both: POSIXct[1:394], format: "2020-05-04 12:00:00"

df$disp_days <- as.numeric(difftime(df$date, df$ps_date, tz,
                                    units = c("days")))
df$disp_days


## VARIABLES: ####
# df$group_ps = which groups, numbered
# df$ps_date = date of corresponding ps, in that group
# df$date = date of news story (or ps)
# df$disp_days = number of days from news story to (corresponding) press statement in that group


## make new dataframe (df_sim) ####

df_sim <- subset(df, select=c(text, source, doc_id, date, ps_date, group_ps, disp_days))
df_sim <- subset.data.frame(df_sim, group_ps > 0 )

df_sim_1 <- subset.data.frame(df_sim, group_ps == 1)
df_sim_2 <- subset.data.frame(df_sim, group_ps == 2)
df_sim_3 <- subset.data.frame(df_sim, group_ps == 3)
df_sim_4 <- subset.data.frame(df_sim, group_ps == 4)
df_sim_5 <- subset.data.frame(df_sim, group_ps == 5)
df_sim_6 <- subset.data.frame(df_sim, group_ps == 6)
df_sim_7 <- subset.data.frame(df_sim, group_ps == 7)
df_sim_8 <- subset.data.frame(df_sim, group_ps == 8)
df_sim_9 <- subset.data.frame(df_sim, group_ps == 9)
df_sim_10 <- subset.data.frame(df_sim, group_ps == 10)
df_sim_11 <- subset.data.frame(df_sim, group_ps == 11)
df_sim_12 <- subset.data.frame(df_sim, group_ps == 12)
df_sim_13 <- subset.data.frame(df_sim, group_ps == 13)
df_sim_14 <- subset.data.frame(df_sim, group_ps == 14)
df_sim_15 <- subset.data.frame(df_sim, group_ps == 15)
df_sim_16 <- subset.data.frame(df_sim, group_ps == 16)
df_sim_17 <- subset.data.frame(df_sim, group_ps == 17)
df_sim_18 <- subset.data.frame(df_sim, group_ps == 18)
df_sim_19 <- subset.data.frame(df_sim, group_ps == 19)

# make order variable for the df, for the y variable later...
### .... ###

# make sure the PS er først - order by disp_days


#### Analysis: Textreuse ####
library(textreuse)

View(df_sim_18)

### NB: NO PRE-PROCESSING DONE ###

corpus_18 <- TextReuseCorpus(text = df_sim_18$text, meta = list(id = df_sim_18$doc_id, tokenizer = tokenize_skip_ngrams, lowercase = TRUE, n = 5, k = 0:2, progress = FALSE))


#inspecting
meta(corpus_18)
corpus_18[[1]]
names(corpus_18)

corpus_18[["GhanaWeb-30"]] # NULL
corpus_18["GhanaWeb-30"] # funker ikke.. men jeg har jo teksten, kanskje ikke så farlig?

content(corpus_18[[21]])
content(corpus_18[["doc-21"]]) # verifying: the press statement



# RATIO OF MATCHES and DIRECTIONALITY:
#matches_18 <- as.matrix(pairwise_compare(corpus_18, ratio_of_matches, directional = TRUE))

# directional=TRUE, so that f(a,b) measures a’s borrowing from b, 
# which may not be the same as f(b,a) (e.g., ratio_of_matches).

matches_18 <- pairwise_candidates(pairwise_compare(corpus_18, ratio_of_matches, directional = TRUE), directional = TRUE)

View(matches_18)


# remember to drop one press statement 

# NOTES for subsetting: 
# subset dataframe 
# not press statements
df_news <- subset.data.frame(df, source == "GhanaWeb")
#not news stories before any press statements
df_news_ps <- subset.data.frame(df_news, group_ps > "0")