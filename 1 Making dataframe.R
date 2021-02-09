# 1 MAKING DATAFRAME

## empty memory (!)
rm(list=ls())

#set wd: 
setwd("C:/Users/lse043/OneDrive - University of Bergen/Documents/0 PhD PROJECT/PAPER 2 'Strategies'/Ghana_RTI_media_advocacy")

# 1. Importing data ####

#(1) webscraped news stories and (2) press statements

#reading from previous 
load("C:/Users/lse043/OneDrive - University of Bergen/Documents/0 PhD PROJECT/PAPER 2 'Strategies'/ANALYSIS/1 Getting data/.RData")
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

# saving ####
save(df_gw, text, df, file = "dataframes.RData")
# loading: 
#load("dataframes.RData")

saveRDS(df, "dataframe_merged.rds")
# Restore it
#df <- readRDS("dataframe_merged.rds")

## empty memory (!)
rm(list=ls())



