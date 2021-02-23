# 1 MAKING DATAFRAME

## empty memory (!)
rm(list=ls())

#set wd: 
setwd("C:/Users/lse043/OneDrive - University of Bergen/Documents/0 PhD PROJECT/PAPER 2 'Strategies'/Ghana_RTI_media_advocacy")

# 1. Importing (old) data ####

#(1) webscraped news stories and (2) press statements

#reading from previous 
#load("C:/Users/lse043/OneDrive - University of Bergen/Documents/0 PhD PROJECT/PAPER 2 'Strategies'/ANALYSIS/1 Getting data/.RData")
# Save the entire workspace anew:
#save.image(file = "old_import.RData")


# reading and saving old data: #### 

# a. NEWS STORIES ####

# Save a single object to a file
saveRDS(val_gw_190620, "GhanaWeb_374_(190620).rds")
# Restore it
gw_374 <- readRDS("GhanaWeb_374_(190620).rds")

# Save multiple objects
save(val_gw_00ish, val_gw_100ish, val_gw_200ish, val_gw_300ish, val_gw_400ish, val_gw_500ish, file = "GhanaWeb_data_per_100.RData")
# To load the data again
load("GhanaWeb_data_per_100.RData")

# b. PRESS STATEMENTS ####

# Save a single object to a file
saveRDS(text, "RTI_20_press_statements.rds")
# Restore it
text <- readRDS("RTI_20_press_statements.rds")

saveRDS(text_corp, "RTI_20_press_statements_corpus.rds")
# Restore it
text_corp <- readRDS("RTI_20_press_statements_corpus.rds")


# 2. Making dataframes ####

#### news stories ####
#importing text data, news stories: 
gw_374 <- readRDS("GhanaWeb_374_(190620).rds")
#inspecting
gw_374[1]

#news stories
library (plyr)
df_gw <- ldply (gw_374, data.frame)

#View(df_gw)
str(df_gw$title)
str(df_gw$date)
str(df_gw$text)

# date 
library(stringr)
#library(lubridate) - not using, maybe more elegant with though
Sys.setlocale("LC_TIME", "English")
df_gw$date
df_gw$date <- df_gw$date %>%
  str_replace_all("General News of ", "") %>%
  str_replace_all("Opinions of ", "") %>%
  str_replace_all("Politics of ", "") %>%
  str_replace_all("Business News of ", "") %>%
  str_replace_all("Diasporian News of ", "") %>%
  str_replace_all("Regional News of ", "") %>%
  str_replace_all("Entertainment of  ", "") %>%
  str_replace_all("Religion of ", " ") %>%
  str_replace_all("xxxxxxxxxxx of ", "") %>%
  str_replace_all("Monday, ", "") %>%
  str_replace_all("Tuesday, ", "") %>%
  str_replace_all("Wednesday, ", "") %>%
  str_replace_all("Thursday, ", "") %>%
  str_replace_all("Friday, ", "") %>%
  str_replace_all("Saturday, ", "") %>%
  str_replace_all("Sunday, ", "")

#remains: 
df_gw$date[61]
df_gw$date[62]
df_gw$date[302]
#61 and 62
df_gw$date <- df_gw$date %>%
  str_replace_all("Entertainment of  ", " ")
#302:
df_gw$date <- df_gw$date %>%
  str_replace_all("Entertainment of ", "")
#346:
df_gw$date <- df_gw$date %>%
  str_replace_all("  ", " ")

#Converting from character to POSIXlt time:
#inserting H and M: 
for (i in 1:length(df_gw$date)){df_gw$date[[i]]<-paste(df_gw$date[[i]]," 12:00",sep="")}
#df_gw$date <- as.POSIXlt.character(df_gw$date, tz = "", "%e %B %Y %H:%M")
df_gw$date <- as.POSIXct(df_gw$date, tz = "", "%e %B %Y %H:%M")
str(df_gw$date)
df_gw$date

str(df_gw)
# adding docvars

#ADDING variables to dataframe: 
df_gw$text_id <- 1:nrow(df_gw)
df_gw$source <- "GhanaWeb"
df_gw$source_type <- "News story"
df_gw$source_owner <- "Media"
df_gw$doc_id <- paste(df_gw$source, df_gw$text_id, sep="-")
df_gw$doc_id

#### press statements ####
#importing text data, press statements: 
text <- readRDS("RTI_20_press_statements.rds")

head(text) #a tibble
# docvars: doc_id, text, date, title, text_id, source, source_type, source_owner
str(text)
text$date <- as.POSIXct(text$date)
text$doc_id <- paste(text$source_owner, text$text_id, sep="-")

# alternatively, but not sure if this is optimal
#text_corp <- readRDS("RTI_20_press_statements_corpus.rds")
#text_corp #Corpus consisting of 20 documents and 6 docvars.
#docvars(text_corp) # date, title, text_id, source, source_type, source_owner

#### merging ####

#NB: I arguably only need date and source (not type and owner) - tidy this later!

df <- rbind(df_gw, text)

str(df)
tail(df, 2)

#unique docnames for corpus function: 
df$id <- 1:nrow(df)

# Saving new data ####
save(df_gw, text, df, file = "dataframes.RData")
# loading: 
load("dataframes.RData")

saveRDS(df, "dataframe_merged.rds")
# Restore it
df <- readRDS("dataframe_merged.rds")

## empty memory (!)
#rm(list=ls())


# 3. adding grouping variables ####
library(lubridate)
# by year
str(df$date)
df$group_year <- format(df$date, format="%Y")
str(df$group_year)
df$group_year

# by press statement
df$group_ps <- df$date


library(dplyr)
#df$group_ps <- as.POSIXct(df$date, format = "%m/%d/%Y %H:%M")
# this is needed in order to use the correct date format
df$group_ps <- df$date
df$group_ps
str(df$group_ps)

#to get press statement dates
str(text$date)
text$date

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

df = df %>% replace(group_ps[v2 == 1], 2)

df <- df %>% 
  mutate(group_ps if v2 == 1, 2)

df = df %>% 

#Compiling the news_disp$ps_group
news_disp$ps_group[news_disp$ps_group_1 == 1] <- 1
news_disp$ps_group[news_disp$ps_group_2 == 1] <- 2
news_disp$ps_group[news_disp$ps_group_3 == 1] <- 3
news_disp$ps_group[news_disp$ps_group_4 == 1] <- 4
news_disp$ps_group[news_disp$ps_group_5 == 1] <- 5
news_disp$ps_group[news_disp$ps_group_6 == 1] <- 6
news_disp$ps_group[news_disp$ps_group_7 == 1] <- 7
news_disp$ps_group[news_disp$ps_group_8 == 1] <- 8
news_disp$ps_group[news_disp$ps_group_9 == 1] <- 9
news_disp$ps_group[news_disp$ps_group_10 == 1] <- 10
news_disp$ps_group[news_disp$ps_group_11 == 1] <- 11
news_disp$ps_group[news_disp$ps_group_12 == 1] <- 12
news_disp$ps_group[news_disp$ps_group_13 == 1] <- 13
news_disp$ps_group[news_disp$ps_group_14 == 1] <- 14
news_disp$ps_group[news_disp$ps_group_15 == 1] <- 15
news_disp$ps_group[news_disp$ps_group_16 == 1] <- 16
news_disp$ps_group[news_disp$ps_group_17 == 1] <- 17
news_disp$ps_group[news_disp$ps_group_18 == 1] <- 18
news_disp$ps_group[news_disp$ps_group_19 == 1] <- 19
news_disp$ps_group[news_disp$ps_group_20 == 1] <- 20
