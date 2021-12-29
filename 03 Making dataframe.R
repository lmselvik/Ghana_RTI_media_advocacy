# Script 3. MAKING DATAFRAME ####

## empty memory (!)
rm(list=ls())

#set wd: 
setwd("C:/Users/lse043/OneDrive - University of Bergen/Documents/0 PhD PROJECT/PAPER 2 'Strategies'/Ghana_RTI_media_advocacy")


# 1. IMPORTING DATA ####

#(1) webscraped news stories and 
#(2) press statements

## a. News stories ####
# reading news stories from web scraping finalised 19 June 2020
gw_374 <- readRDS("Data/GhanaWeb_374_(190620).rds")

## b. Press statements ####
# loading texts read from jpg and pdf files
text <- readRDS("Data/RTI_20_press_statements.rds")


# 2. PREPARING DATAFRAMES ####

## a. News stories ####

# inspecting
gw_374[1]

library (plyr)
df_gw <- ldply (gw_374, data.frame)

#View(df_gw)
str(df_gw)

# date 
library(stringr)
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

# remaining: 
df_gw$date[61]
df_gw$date[62]
df_gw$date[302]
df_gw$date[346]

# manual cleaning: 
#61 and 62
df_gw$date <- df_gw$date %>%
  str_replace_all("Entertainment of  ", " ")
#302:
df_gw$date <- df_gw$date %>%
  str_replace_all("Entertainment of ", "")
#346:
df_gw$date <- df_gw$date %>%
  str_replace_all("  ", " ")

# converting from character to POSIXlt time:
# inserting H and M: 
df_gw$date
for (i in 1:length(df_gw$date)){df_gw$date[[i]]<-paste(df_gw$date[[i]]," 12:00",sep="")}
df_gw$date <- as.POSIXct(df_gw$date, tz = "", "%e %B %Y %H:%M")
str(df_gw$date)


# adding variables to dataframe: 
df_gw$text_id <- 1:nrow(df_gw)
df_gw$source <- "GhanaWeb"
df_gw$doc_id <- paste(df_gw$source, df_gw$text_id, sep="-")
df_gw$doc_id

str(df_gw)

## b. Press statements ####

head(text) #a tibble
str(text)
text$date <- as.POSIXct(text$date)
text$doc_id <- paste(text$source_owner, text$text_id, sep="-")

# selecting variables
library(dplyr)
text <-
  text %>%
  select(doc_id, text, date, title, text_id, source)


# 3. FINAL DATAFRAME ####

# merging: 
df <- rbind(df_gw, text)
str(df)

## Grouping news stories by press statements ####
library(lubridate)
library(dplyr)

# dates in df
str(df$date)

# press statement dates
text$date   # using these dates to make groups

# making grouping variables
df <- df %>% 
  mutate(v0 = ifelse(date < "2010-02-15 01:00:00", 1, 0),
         v1 = ifelse(date >= "2010-02-15 01:00:00" & date < "2012-05-10 01:00:00", 1, 0),
         v2 = ifelse(date >= "2012-05-10 01:00:00" & date < "2012-05-22 01:00:00", 1, 0),
         v3 = ifelse(date >= "2012-05-22 01:00:00" & date < "2012-07-19 01:00:00", 1, 0),
         v4 = ifelse(date >= "2012-07-19 01:00:00" & date < "2012-10-22 01:00:00", 1, 0),
         v5 = ifelse(date >= "2012-10-22 01:00:00" & date < "2013-02-06 00:00:00", 1, 0),
         v6 = ifelse(date >= "2013-02-06 00:00:00" & date < "2013-04-30 00:00:00", 1, 0),
         v7 = ifelse(date >= "2013-04-30 00:00:00" & date < "2013-05-31 00:00:00", 1, 0),
         v8 = ifelse(date >= "2013-05-31 00:00:00" & date < "2013-09-27 00:00:00", 1, 0),
         v9 = ifelse(date >= "2013-09-27 00:00:00" & date < "2015-09-28 00:00:00", 1, 0),
         v10 = ifelse(date >= "2015-09-28 00:00:00" & date < "2016-04-15 00:00:00", 1, 0),
         v11 = ifelse(date >= "2016-04-15 00:00:00" & date < "2016-07-28 00:00:00", 1, 0),
         v12 = ifelse(date >= "2016-07-28 00:00:00" & date < "2016-08-18 00:00:00", 1, 0),
         v13 = ifelse(date >= "2016-08-18 00:00:00" & date < "2016-09-23 00:00:00", 1, 0),
         v14 = ifelse(date >= "2016-09-23 00:00:00" & date < "2017-02-01 00:00:00", 1, 0),
         v15 = ifelse(date >= "2017-02-01 00:00:00" & date < "2018-09-28 00:00:00", 1, 0),
         v16 = ifelse(date >= "2018-09-28 00:00:00" & date < "2019-03-27 00:00:00", 1, 0),
         v17 = ifelse(date >= "2019-03-27 00:00:00" & date < "2019-04-15 00:00:00", 1, 0),
         v18 = ifelse(date >= "2019-04-15 00:00:00", 1, 0))


# grouping variable, group_ps
df$group_ps <- 0   # all news stories before 2010-02-15 will remain 0 - not included in analysis

df$group_ps[df$v1 == 1] <- 1
df$group_ps[df$v2 == 1] <- 2
df$group_ps[df$v3 == 1] <- 3
df$group_ps[df$v4 == 1] <- 4
df$group_ps[df$v5 == 1] <- 5
df$group_ps[df$v6 == 1] <- 6
df$group_ps[df$v7 == 1] <- 7
df$group_ps[df$v8 == 1] <- 8
df$group_ps[df$v9 == 1] <- 9
df$group_ps[df$v10 == 1] <- 10
df$group_ps[df$v11 == 1] <- 11
df$group_ps[df$v12 == 1] <- 12
df$group_ps[df$v13 == 1] <- 13
df$group_ps[df$v14 == 1] <- 14
df$group_ps[df$v15 == 1] <- 15
df$group_ps[df$v16 == 1] <- 16
df$group_ps[df$v17 == 1] <- 17
df$group_ps[df$v18 == 1] <- 18

#View(df)
str(df)

# inserting the corresponding date for press statement group
# ps_date:
df$ps_date <- df$date
df$ps_date[df$group_ps == 1] <- as.POSIXct(text$date[1])
df$ps_date[df$group_ps == 2] <- as.POSIXct(text$date[2])
df$ps_date[df$group_ps == 3] <- as.POSIXct(text$date[3])
df$ps_date[df$group_ps == 4] <- as.POSIXct(text$date[4])
df$ps_date[df$group_ps == 5] <- as.POSIXct(text$date[6]) # Note jump: "2012-10-22 01:00:00 CEST" (see Appendix B and C)
df$ps_date[df$group_ps == 6] <- as.POSIXct(text$date[8]) # Note jump: "2013-02-06 CET" (see Appendix B and C)
df$ps_date[df$group_ps == 7] <- as.POSIXct(text$date[9])
df$ps_date[df$group_ps == 8] <- as.POSIXct(text$date[10])
df$ps_date[df$group_ps == 9] <- as.POSIXct(text$date[11])
df$ps_date[df$group_ps == 10] <- as.POSIXct(text$date[12])
df$ps_date[df$group_ps == 11] <- as.POSIXct(text$date[13])
df$ps_date[df$group_ps == 12] <- as.POSIXct(text$date[14])
df$ps_date[df$group_ps == 13] <- as.POSIXct(text$date[15])
df$ps_date[df$group_ps == 14] <- as.POSIXct(text$date[16])
df$ps_date[df$group_ps == 15] <- as.POSIXct(text$date[17])
df$ps_date[df$group_ps == 16] <- as.POSIXct(text$date[18])
df$ps_date[df$group_ps == 17] <- as.POSIXct(text$date[19])
df$ps_date[df$group_ps == 18] <- as.POSIXct(text$date[20])

str(df)


## Calculating time differences ####

# time1 - time of items:
str(df$date)
# time2 - time of press statement:
str(df$ps_date)

df$disp_days <- as.numeric(difftime(df$date, df$ps_date, tz,
                                          units = c("days")))

df$disp_days



## selecting variables for final dataframe 
library(dplyr)
df_final <-
  df %>%
  select(title, date, text, source, doc_id, group_ps, ps_date, disp_days,
  ) %>%
  rename(ps_id = group_ps)


# 4. SAVING DATAFRAMES ####

# saving dataframes
## save(df_gw, text, df, file = "Data/dataframes.RData")
# loading: 
#load("Data/dataframes.RData")

# Saving final data frame ####
##saveRDS(df_final, "Data/dataframe_final.rds")
# Restore it
#df_final <- readRDS("Data/dataframe_final.rds")

# THE END  -----------------------------
