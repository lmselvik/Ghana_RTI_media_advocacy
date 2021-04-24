# 2C-b. Text analysis: Textreuse with press statements- TRYING WITH RNEWSFLOW !

## empty memory (!)
rm(list=ls())

#set wd: 
setwd("C:/Users/lse043/OneDrive - University of Bergen/Documents/0 PhD PROJECT/PAPER 2 'Strategies'/Ghana_RTI_media_advocacy")

# loading all the dataframes (df_gw, text, df)
load("dataframes.RData")

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

# saving and loading ####

#saveRDS(df_sim, "df_sim.rds")
#saveRDS(df_sim, file = "df_sim") # correct


# Restore it
#df_sim <- readRDS("df_sim.rds")
df_sim <- readRDS("df_sim")

#### 1. Prepare RNewsflow analysis ####

# making corpus ####
library(quanteda)
corp <- corpus(df_sim, docid_field = 'doc_id', text_field='text')

head(docvars(corp))
ndoc(corp)
corp[1]
corp["GhanaWeb-1"]


# pre-processing texts and creating the DTM ####
#Document-feature matrix of: 372 documents, 7,342 features (97.6% sparse) and 5 docvars.

dtm <- dfm(corp,
           tolower = TRUE,
           stem = TRUE,
           remove = stopwords("english"),
           remove_punct = TRUE,
           remove_numbers = TRUE)
dtm

# subsetting dfm by group ####
dtm_19 <- dfm_subset(dtm, group_ps == 19)
dtm_18 <- dfm_subset(dtm, group_ps == 18)
dtm_17 <- dfm_subset(dtm, group_ps == 17)
dtm_16 <- dfm_subset(dtm, group_ps == 16)
dtm_15 <- dfm_subset(dtm, group_ps == 15)
dtm_14 <- dfm_subset(dtm, group_ps == 14)
dtm_13 <- dfm_subset(dtm, group_ps == 13)
dtm_12 <- dfm_subset(dtm, group_ps == 12)
dtm_11 <- dfm_subset(dtm, group_ps == 11)
dtm_10 <- dfm_subset(dtm, group_ps == 10)
dtm_09 <- dfm_subset(dtm, group_ps == 9)
dtm_08 <- dfm_subset(dtm, group_ps == 8)
dtm_07 <- dfm_subset(dtm, group_ps == 7)
dtm_06 <- dfm_subset(dtm, group_ps == 6)
dtm_05 <- dfm_subset(dtm, group_ps == 5)
dtm_04 <- dfm_subset(dtm, group_ps == 4)
dtm_03 <- dfm_subset(dtm, group_ps == 3)
dtm_02 <- dfm_subset(dtm, group_ps == 2)
dtm_01 <- dfm_subset(dtm, group_ps == 1)

dtm_01

dtm_01b <- dfm_subset(dtm, group_ps > 1)
dtm_01b

#inspecting
head(docvars(dtm_10), 3)
tail(docvars(dtm_10), 3) 

# 2. Compare with RNewsflow #### 
library(RNewsflow)

docvars(dtm_18)
# disp_days (to the PS)
# min: 0.500000
# max: 8.458333

# calculating document similarities (THE MODEL:)
g_18 <- newsflow_compare(dtm_18, date_var='date',
                      hour_window = c(0.1,2232),  # window set to 3 months = 31 days x 3 months x 24 hours
                      min_similarity = 0, 
                      only_complete_window = F)

# extracting relevant info: 
#inspecting document (vertices) and document pairs (edges) and attributes: 
# as dataframe: 
v_18 <- as_data_frame(g_18, 'vertices')
e_18 <- as_data_frame(g_18, 'edges')
View(e_18)

# as dataframe: 
e_18 <- as_data_frame(g_18, 'edges')
head(e_18,3) # weight represents the similarity score
str(e_18)
rti_e_18 <- subset.data.frame(e_18, from == "Coalition-19")
View(rti_e_18)

# 19: (delimit to 2020 ?? AND laarge window...) =================

docvars(dtm_19)
# disp_days (to the PS)
# min:  1.5000
# max:  256.5417
# 2020-05-04 // 2019-12-27

g_19 <- newsflow_compare(dtm_19, date_var='date',
                         hour_window = c(0.1,2232),  # window set to 3 months = 31 days x 3 months x 24 hours
                         min_similarity = 0, 
                         only_complete_window = F)

e_19 <- as_data_frame(g_19, 'edges')
head(e_19,3)
View(e_19)
rti_e_19 <- subset.data.frame(e_19, from == "Coalition-20")
View(rti_e_19)


# 17: =================
docvars(dtm_17)
# disp_days (to the PS)
# min: 1.50000
# max: 178.54167
# 2019-03-25

g_17 <- newsflow_compare(dtm_17, date_var='date',
                         hour_window = c(0.1,2232),  # window set to 3 months = 31 days x 3 months x 24 hours
                         min_similarity = 0, 
                         only_complete_window = F)

e_17 <- as_data_frame(g_17, 'edges')
head(e_17,3)
View(e_17)
rti_e_17 <- subset.data.frame(e_17, from == "Coalition-18")
View(rti_e_17)

# 16: =================
docvars(dtm_16)
# disp_days (to the PS)
# min:  1.50000
# max: 545.45833
# 2018-07-31 

g_16 <- newsflow_compare(dtm_16, date_var='date',
                         hour_window = c(0.1,2232),  # window set to 3 months = 31 days x 3 months x 24 hours
                         min_similarity = 0, 
                         only_complete_window = F)

e_16 <- as_data_frame(g_16, 'edges')
rti_e_16 <- subset.data.frame(e_16, from == "Coalition-17")
View(rti_e_16)

# 15: =================
docvars(dtm_15)
# disp_days (to the PS)
# min: 1.50000
# max: 107.54167
# 2017-01-08

g_15 <- newsflow_compare(dtm_15, date_var='date',
                         hour_window = c(0.1,2232),  # window set to 3 months = 31 days x 3 months x 24 hours
                         min_similarity = 0, 
                         only_complete_window = F)

e_15 <- as_data_frame(g_15, 'edges')
rti_e_15 <- subset.data.frame(e_15, from == "Coalition-16")
View(rti_e_15)

# 14: =================
docvars(dtm_14)
# disp_days (to the PS)
# min: -
# max: -
# 0

# 13: =================
docvars(dtm_13)
# disp_days (to the PS)
# min: 1.5
# max:  6.5
# 2016-08-03

g_13 <- newsflow_compare(dtm_13, date_var='date',
                         hour_window = c(0.1,2232),  # window set to 3 months = 31 days x 3 months x 24 hours
                         min_similarity = 0, 
                         only_complete_window = F)

e_13 <- as_data_frame(g_13, 'edges')
rti_e_13 <- subset.data.frame(e_13, from == "Coalition-14")
View(rti_e_13)

# 12: =================
docvars(dtm_12)
# disp_days (to the PS)
# min: 1.5
# max: 94.5
# 2016-07-18

g_12 <- newsflow_compare(dtm_12, date_var='date',
                         hour_window = c(0.1,2232),  # window set to 3 months = 31 days x 3 months x 24 hours
                         min_similarity = 0, 
                         only_complete_window = F)

e_12 <- as_data_frame(g_12, 'edges')
rti_e_12 <- subset.data.frame(e_12, from == "Coalition-13")
View(rti_e_12)

# 11: =================
docvars(dtm_11)
# disp_days (to the PS)
# min: 4.50000
# max: 174.54167
# 2016-03-20

g_11 <- newsflow_compare(dtm_11, date_var='date',
                         hour_window = c(0.1,2232),  # window set to 3 months = 31 days x 3 months x 24 hours
                         min_similarity = 0, 
                         only_complete_window = F)

e_11 <- as_data_frame(g_11, 'edges')
rti_e_11 <- subset.data.frame(e_11, from == "Coalition-12")
View(rti_e_11)

# 10: =================
docvars(dtm_10)
# disp_days (to the PS)
# min: 2.50000
# max: 701.50000
# 2015-08-29

g_10 <- newsflow_compare(dtm_10, date_var='date',
                         hour_window = c(0.1,2232),  # window set to 3 months = 31 days x 3 months x 24 hours
                         min_similarity = 0, 
                         only_complete_window = F)

e_10 <- as_data_frame(g_10, 'edges')
rti_e_10 <- subset.data.frame(e_10, from == "Coalition-11")
View(rti_e_10)

# 9: (NB: This group includes the PS without proper date, 2013-08-01 - "Coalition-7") =================
docvars(dtm_09)
# disp_days (to the PS)
# min: 4.50000
# max: 85.50000
# 2013-08-24

g_9 <- newsflow_compare(dtm_09, date_var='date',
                         hour_window = c(0.1,2232),  # window set to 3 months = 31 days x 3 months x 24 hours
                         min_similarity = 0, 
                         only_complete_window = F)

e_9 <- as_data_frame(g_9, 'edges')
rti_e_9 <- subset.data.frame(e_9, from == "Coalition-10")
View(rti_e_9)

# 8: =================
docvars(dtm_08)
# disp_days (to the PS)
# min: 1.5
# max: ---
# 2013-05-01

g_8 <- newsflow_compare(dtm_08, date_var='date',
                        hour_window = c(0.1,2232),  # window set to 3 months = 31 days x 3 months x 24 hours
                        min_similarity = 0, 
                        only_complete_window = F)

e_8 <- as_data_frame(g_8, 'edges')
rti_e_8 <- subset.data.frame(e_8, from == "Coalition-9")
View(rti_e_8)

# 7: =================
docvars(dtm_07)
# disp_days (to the PS)
# min:  6.5
# max: 32.5
# 2013-03-10

g_7 <- newsflow_compare(dtm_07, date_var='date',
                        hour_window = c(0.1,2232),  # window set to 3 months = 31 days x 3 months x 24 hours
                        min_similarity = 0, 
                        only_complete_window = F)

e_7 <- as_data_frame(g_7, 'edges')
rti_e_7 <- subset.data.frame(e_7, from == "Coalition-8")
View(rti_e_7)


# 6: =================
docvars(dtm_06)
# disp_days (to the PS)
# min: -
# max: -
# 0

# 5: =================
docvars(dtm_05)
# disp_days (to the PS)
# min: -
# max: -
# 0

# 4: (NB: 2 PS, 4 and 5) =================
docvars(dtm_04)
# disp_days (to the PS)
# min: 72.45833
# max: 87.45833
# 2012-10-14

g_4 <- newsflow_compare(dtm_04, date_var='date',
                        hour_window = c(0.1,2232),  # window set to 3 months = 31 days x 3 months x 24 hours
                        min_similarity = 0, 
                        only_complete_window = F)

e_4 <- as_data_frame(g_4, 'edges')
View(e_4)
rti_e_4 <- subset.data.frame(e_4, from == "Coalition-5")
View(rti_e_4)

# 3: =================
docvars(dtm_03)
# disp_days (to the PS)
# min: 0.4583333
# max: 45.4583333
# 2012-07-06

g_3 <- newsflow_compare(dtm_03, date_var='date',
                        hour_window = c(0.1,2232),  # window set to 3 months = 31 days x 3 months x 24 hours
                        min_similarity = 0, 
                        only_complete_window = F)

e_3 <- as_data_frame(g_3, 'edges')
rti_e_3 <- subset.data.frame(e_3, from == "Coalition-3")

# 2: =================
docvars(dtm_02)
# disp_days (to the PS)
# min: 6.458333
# max: 8.458333
# 2012-05-18

g_2 <- newsflow_compare(dtm_02, date_var='date',
                        hour_window = c(0.1,2232),  # window set to 3 months = 31 days x 3 months x 24 hours
                        min_similarity = 0, 
                        only_complete_window = F)

e_2 <- as_data_frame(g_2, 'edges')
rti_e_2 <- subset.data.frame(e_2, from == "Coalition-2")

# 1: =================
docvars(dtm_01)
# disp_days (to the PS)
# min: 10.45833
# max: 806.41667
# 2012-05-01

library(RNewsflow)

g_1 <- newsflow_compare(dtm_01, date_var='date',
                        hour_window = c(0.1,2232),  # window set to 3 months = 31 days x 3 months x 24 hours
                        min_similarity = 0, 
                        only_complete_window = F)

e_1 <- as_data_frame(g_1, 'edges')
rti_e_1 <- subset.data.frame(e_1, from == "Coalition-1")

View(rti_e_1)

####

dtm_01b

g_1b <- newsflow_compare(dtm_01b, date_var='date',
                        hour_window = c(0.1,2232),  # window set to 3 months = 31 days x 3 months x 24 hours
                        min_similarity = 0, 
                        only_complete_window = F)

library(dplyr)
e_1b <- as_data_frame(g_1b, 'edges')
rti_e_1b <- subset.data.frame(e_1b, from == "Coalition-1")

View(rti_e_1b)


# 3. Compiling analysis data ####

# first, combining all rti_e
rti_e <- rbind(rti_e_1, rti_e_2, rti_e_3, rti_e_4, rti_e_7, rti_e_8, rti_e_9, rti_e_10, rti_e_11, rti_e_12, rti_e_13, rti_e_15, rti_e_16, rti_e_17, rti_e_18, rti_e_19)

library(dplyr)
#detach("package:plyr", unload = TRUE)
rti_e <- rti_e %>% 
  rename(doc_id = to)

str(df_sim)
str(rti_e)

df_rti_sim <- left_join(df_sim, rti_e, by = c("doc_id"))

View(df_rti_sim)


## saving dataframe for analysis ####
#saveRDS(df_rti_sim, file = "df_similarity_RNewsflow")

# Restore it
df_rti_sim <- readRDS("df_similarity_RNewsflow")


# new variable: ps_id ####
str(df_rti_sim)

df_rti_sim$ps_date

df_rti_sim$ps_id <- format(df_rti_sim$ps_date, format="%Y-%m-%d")

df_rti_sim$ps_id[df_rti_sim$ps_id == "2019-04-15"] <- 18
df_rti_sim$ps_id[df_rti_sim$ps_id == "2019-03-27"] <- 17
df_rti_sim$ps_id[df_rti_sim$ps_id == "2018-09-28"] <- 16
df_rti_sim$ps_id[df_rti_sim$ps_id == "2017-02-01"] <- 15
df_rti_sim$ps_id[df_rti_sim$ps_id == "2016-09-23"] <- 14
df_rti_sim$ps_id[df_rti_sim$ps_id == "2016-08-18"] <- 13
df_rti_sim$ps_id[df_rti_sim$ps_id == "2016-07-28"] <- 12
df_rti_sim$ps_id[df_rti_sim$ps_id == "2016-04-15"] <- 11
df_rti_sim$ps_id[df_rti_sim$ps_id == "2015-09-28"] <- 10
df_rti_sim$ps_id[df_rti_sim$ps_id == "2013-09-27"] <- 9
df_rti_sim$ps_id[df_rti_sim$ps_id == "2013-05-31"] <- 8
df_rti_sim$ps_id[df_rti_sim$ps_id == "2013-04-30"] <- 7
df_rti_sim$ps_id[df_rti_sim$ps_id == "2013-02-06"] <- 6
df_rti_sim$ps_id[df_rti_sim$ps_id == "2012-10-22"] <- 5
df_rti_sim$ps_id[df_rti_sim$ps_id == "2012-07-19"] <- 4
df_rti_sim$ps_id[df_rti_sim$ps_id == "2012-05-22"] <- 3
df_rti_sim$ps_id[df_rti_sim$ps_id == "2012-05-10"] <- 2
df_rti_sim$ps_id[df_rti_sim$ps_id == "2010-02-15"] <- 1

df_rti_sim$ps_id

# ID as numeric
str(df_rti_sim$ps_id)
df_rti_sim$ps_id <- as.numeric(df_rti_sim$ps_id)

# ordered dataframe by date
str(df_rti_sim$date)
df_rti_sim <- df_rti_sim[order(df_rti_sim$date),]


# no missing obs
df_2 <- na.omit(df_rti_sim)
#View(df_2)
str(df_2)   #178 obs. of  11 variables

# remove the one left with source = RTI Coalition !
df_3 <- filter(df_2, doc_id != "Coalition-7")
str(df_3)   #177 obs. of  11 variables


# 4. Plotting ####
library(ggplot2)
library(dplyr)

str(df_3)

# velg ut hvilke grupper som skal med, jf. methods table with obs. number: 

# keep gr.: 1 (N-16), 3 (N=6), 8 (N=6), 9 (N=28), 10 (N=6), 11 (N=13), 14, 15, 16, 17, 18 WITH NEW ID --- NB: 11 har kun 3 obs i grafen - ta vekk? 13 = 4...
### => old numbering: 1,3,9,10,11,12,15,16,17,18,19

# unsure: 4 (n=3), 12 (N=4)
# not: gr.: 2 (N=2), 5 (N=0), 6 (N=2), 7 (N=1), 13 (N=0)

# ID label variable 

df_3$ps_id_date <- format(df_3$ps_date, format="%Y-%m-%d")
df_3$ps_id_label <- apply(df_3,1 ,function(x) paste0("Press statement ", toString(x["ps_id"]), sep=": ", toString(x["ps_id_date"])))

# Keep only relevant PS groups: 
df_4 <- df_3 %>% 
  filter(ps_id %in% c(1,3,8,9,10,11,14,15,16,17,18))

# drop another two, 10 = only 3 obs in window (and no line) & 17 = short time window --> all in Appendix instead! 
df_5 <- df_3 %>% 
  filter(ps_id %in% c(1,3,8,9,11,14,15,16,18))

# Plot
df_3$ps_id <- as.factor(df_3$ps_id)
df_4$ps_id <- as.factor(df_4$ps_id)
df_5$ps_id <- as.factor(df_5$ps_id)

#nice font: 
library("extrafont")
loadfonts(device = "win")
windowsFonts()
font_import()

# Final PLOT for submission ####
p <- ggplot(df_5, aes(x=disp_days, y=weight, group=ps_id_label)) + 
  geom_point(alpha = 0.5) +  #, ymax = 1 ## no effect
  geom_smooth( , se = F, colour = "#7570B3") +
  theme_bw()+
  ylim(0, 1)+
  xlab("Time since issuing of press statement (in days)")+
  ylab("News stories' similarity with preceding press statement")+
  labs(fill = "")+
  facet_wrap(~ps_id_label)

p + theme(legend.title = element_blank(), text = element_text(size=20, family="serif")) 

# saving: 1700 x 1100 = jpg
# saving: 1100 x 700 = jpg

# APPENDIX PLOT for submission ####
pp <- ggplot(df_3, aes(x=disp_days, y=weight, group=ps_id_label)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth( , se = F, colour = "#7570B3") +
  theme_bw()+
  ylim(0, 1)+
  xlab("Time since issuing of press statement (in days)")+
  ylab("News stories' similarity with preceding press statement")+
  labs(fill = "")+
  facet_wrap(~ps_id_label)


pp + theme(legend.title = element_blank(), text = element_text(size=20, family="serif")) 

# saving: 1450 x 850 = jpg


# INCLUDE IN PLOTS
# WIP: more "air" around the plot and legends
# DONE: title in plot - with date
# DONE: better legend
# WIP/LATER: - CAN I INSERT BACKGROUND COLOUR BASED ON THE DISTANCE TO NEXT PS?

# NO: 
#geom_smooth(method = "lm",color="black", se = F) +
## theme(text = element_text(size=12),legend.position = "bottom")+



# previous notes =========================

# Keep only 3 names
df_3 <- df_2 %>% 
  filter(group_ps %in% c(1, 3, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19))

# Plot
df_2$group_ps <- as.factor(df_2$group_ps)
df_3$group_ps <- as.factor(df_3$group_ps)

# nb: ikke bruk group_ps, men from :-) 

df_2 %>%
  ggplot( aes(x=disp_days, y=weight, group=from, color=from)) +
  geom_line()

df_2 %>%
  ggplot( aes(x=disp_days, y=weight, group=group_ps, color=group_ps)) +
  geom_line(alpha = 0.7) + #alpha = 0.7 : mindre sterke farger
  ylab("fill in")+
  xlab("fill in")+
  xlim(0,100)

# GOOD: will give each group a graph of its own!
p <- ggplot(df_2, aes(x=disp_days, y=weight, group=group_ps, color=group_ps)) + 
  geom_line(alpha = 0.7)+
  facet_grid(group_ps~.)+
  scale_color_viridis_d()+
  ylab("fill in")+
  xlab("fill in")+
  theme_bw()+
  xlim(0,100)
p

# getting closer: NB: df_3 ikke df_2
p4 <- ggplot(df_3, aes(x=disp_days, y=weight, group=group_ps)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm",color="black", se = F) +
  theme_bw()+
  xlab("fill")+
  ylab("fill")+
  labs(fill = "")+
  #theme(text = element_text(size=12),legend.position = "bottom")+
  facet_wrap(~group_ps)

p4 + theme(legend.title = element_blank()) 

p5 <- ggplot(df_2, aes(x=disp_days, y=weight, group=group_ps)) + 
  geom_point(alpha = 0.5) +
  geom_smooth( , se = F) +
  theme_bw()+
  xlab("fill")+
  ylab("fill")+
  labs(fill = "")+
  facet_wrap(~group_ps)

p5 + theme(legend.title = element_blank())  ## but warning !

# something nice here, needs more work: 
p3 <- ggplot(df_2, aes(x=disp_days, y=weight, group=group_ps, color=group_ps)) + 
  scale_color_viridis_d()+
  geom_point(alpha = 0.8) +
  geom_smooth(method = "lm",color="black", se = T) +
  theme_bw()+
  xlab("fill")+
  ylab("fill")+
  labs(fill = "")+
  theme(text = element_text(size=12),legend.position = "bottom")+
  facet_wrap(~group_ps)

p3 + theme(legend.title = element_blank()) 

#### lisa g notes 
# something nice here, but needs more work: 

p3 <- ggplot(df_2, aes(x=disp_days, y=weight, group=group_ps, color=group_ps)) + 
  scale_color_viridis_d()+
  geom_point(alpha = 0.05) +
  geom_smooth(method = "lm",color="black", se = T) +
  theme_bw()+
  xlab("fill")+
  ylab("fill")+
  labs(fill = "")+
  theme(text = element_text(size=12),legend.position = "bottom")+
  facet_wrap(~group_ps)

p3 + theme(legend.title = element_blank()) 




#### lisa g pure notes 
p3 <- ggplot(data=dt3, 
             aes(x = v2x_jucon, y = value, 
                 group = variable,
                 color = variable)) + 
  scale_color_aaas()+
  geom_point(alpha = 0.05) +
  geom_smooth(method = "lm",color="black", se = T) +
  theme_bw()+
  xlab("Judicial constraints")+
  ylab("Expected topic proportion")+
  labs(fill = "")+
  theme(text = element_text(size=12),legend.position = "bottom")+
  facet_wrap(~variable)

p3 + theme(legend.title = element_blank()) 

ggplot(dfDR, aes(date,value, col=variable)) + 
  geom_line(alpha = 0.7)+
  facet_grid(country~.)+
  scale_color_viridis_d()+
  ylab("number of articles")+
  theme_bw()+
  ylim(0,20)



#### notes #####

# mal for changing variable names with dplyr: 
my_data %>% 
  rename(
    sepal_length = Sepal.Length,
    sepal_width = Sepal.Width
  )

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

