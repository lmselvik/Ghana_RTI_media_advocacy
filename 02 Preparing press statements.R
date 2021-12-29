# Script 2. PREPARING PRESS STATEMENTS ####

## empty memory (!)
rm(list=ls())

#set wd: 
setwd("C:/Users/lse043/OneDrive - University of Bergen/Documents/0 PhD PROJECT/PAPER 2 'Strategies'/Ghana_RTI_media_advocacy")


# 1. PERPARING DATAFRAME ####

library(quanteda)
library(readtext)

# making dataframe 
dir <- "C:/Users/lse043/OneDrive - University of Bergen/Documents/0 PhD PROJECT/PAPER 2 'Strategies'/Ghana_RTI_media_advocacy/text data/"
docinfo <- c("date")
#text <- readtext(paste0(dir, "press_statements/*.txt")) #this works!
(text <- readtext(paste0(dir, "press_statements/*.txt"),
                  docvarsfrom = "filenames",
                  docvarnames = c("date")))

library(dplyr)
glimpse(text)
#Rows: 20
#Columns: 2
#$ doc_id 
#$ text   
#$ date


## Adding variables   ####

text$title <- "Title in text"
text$text_id <- 1:20
text$source <- "RTI Coalition"
text$source_type <- "Press statement"
text$source_owner <- "Coalition"
text <- cbind(text)

glimpse(text)

# formatting date
text$date

# first, inserting fake date where only year appear: 
text$date[7] <- "2013-08-01"

# then, formatting
text$date <- strptime(text$date, "%Y-%m-%d", tz = "")

# adding hours & minutes
text$date_2 <- text$date

for (i in 1:length(text$date_2)){text$date_2[[i]]<-paste(text$date_2[[i]]," 01:00",sep="")}
text$date_2 <- strptime(text$date_2, "%Y-%m-%d %H:%M", tz = "")

text$date <- text$date_2

str(text)
text <- select(text,
               doc_id, text, date, title, text_id, source, source_type, source_owner)


text <- as_tibble(text)
glimpse(text)



# 2. SAVING ####

# Saving data frame
##saveRDS(text, "Data/RTI_20_press_statements.rds")


# THE END  -----------------------------
