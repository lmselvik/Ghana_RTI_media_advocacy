# 2B. Text analysis: Making news dispersion plot 

## empty memory (!)
rm(list=ls())

#set wd: 
setwd("C:/Users/lse043/OneDrive - University of Bergen/Documents/0 PhD PROJECT/PAPER 2 'Strategies'/Ghana_RTI_media_advocacy")

# loading all the dataframes (df_gw, text, df)
load("dataframes.RData")

str(df_gw)
str(df)

# adding grouping variables ####
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

#### making new dataframe for analysis ####

# subset dataframe 
# not press statements
df_news <- subset.data.frame(df, source == "GhanaWeb")
#not news stories before any press statements
df_news_ps <- subset.data.frame(df_news, group_ps > "0")

# saving and loading ####
# save
#saveRDS(df_news_ps, file = "df_plot_news dispersion")
# load
#df_news_ps <- readRDS("df_plot_news dispersion")


min(df_news_ps$disp_days) #[1] 0.4583333

#inspecting group 1: 
df_news_ps_1 <- subset.data.frame(df_news_ps, group_ps == "1")
max(df_news_ps_1$disp_days) #[1] 806.4167
min(df_news_ps_1$disp_days) #[1] 10.45833

#inspecting group 17: 
df_news_ps_17 <- subset.data.frame(df_news_ps, group_ps == "17")
max(df_news_ps_17$disp_days) #[1] 178.5417
min(df_news_ps_17$disp_days) #[1] 1.5


str(df_news_ps)

# counting dates / obs. 

# selecting variables:
#library(plyr)
#library(dplyr)

myvars <- c("date", "doc_id")
dates <- df_news_ps[myvars]

dates$date <- as.character(dates$date)
str(dates)

count <- as.matrix(count(dates$date))
count_df <- as.data.frame(count)
glimpse(count_df)


#### GRAPHS ####
# See: https://www.r-graph-gallery.com/279-plotting-time-series-with-ggplot2.html
library(ggplot2)
library(dplyr)
library(viridis)
require("ggrepel")


# using: 
#df_news_ps

df_disp <- subset(df_news_ps, select=c(date, ps_date, group_ps, disp_days))

# dataframe: df_disp
# x, date variable, Date format = news_disp$date --> date
# y value, numeric = news_disp$disp_days --> disp_date 

str(df_disp)


# making new variable for label 
df_disp$group_ps
df_disp$ps_date
df_disp$ps_date2 <- format(df_disp$ps_date, format="%Y-%m-%d")
df_disp$group <- apply(df_disp,1 ,function(x) paste0(toString(x["group_ps"]), sep=": ", toString(x["ps_date2"])))


# basic scatterplot, without color
p <- ggplot(df_disp, aes(x=date, y=disp_days)) +
  geom_point()
p

# A basic scatterplot with color depending on Variable --- FIRST DRAFT SUBMISSION
p <- ggplot(df_disp, aes(x=disp_days, y=ps_date, color=disp_days)) + 
  geom_point(size=1)
p


#more interesting, shows whether a press statement leads to related a news story
#ALSO, should remove observations that are too far off... 

df_disp_365 <- subset(df_disp, disp_days < 365,
                      select=c(date, group_ps, ps_date, disp_days, group))

p <- ggplot(df_disp_365, aes(x=disp_days, y=ps_date, color=disp_days)) + 
  geom_point(size=1)
p

#should try to indicate 1) the size of coverage and 2) the press statement dates

#nice font: 
library("extrafont")
loadfonts(device = "win")
windowsFonts()
font_import()

#testing:
pp <- ggplot(df_disp_365, aes(x=disp_days, y=factor(group_ps), color=disp_days)) + 
  geom_point(size=1) +
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5)) +
  labs(title=" ", x="Time: days passed since press statement", y = "Issue of press statement") + 
  theme(text = element_text(size=15, family="serif"))
pp

## final plot ## 

pp <- ggplot(df_disp_365, aes(x=disp_days, y=factor(group), color=disp_days)) + 
  geom_point(size=2) +
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5)) +
  labs(title=" ", x="Days passed since press statement", y = "Issuing of press statement", color = "Distance in time") + 
  theme(text = element_text(size=25, family="serif"))
pp


#saving: 1600 x 700 = jpg 


# THE END  -----------------------------

#decreseing and increasing dot size
#see: https://www.data-to-viz.com/caveat/overplotting.html

## testing ####

# trying advance: 

install.packages("hrbrthemes")
library(hrbrthemes)
#hrbrthemes.loadfonts
#loadfonts(hrbrthemes)

#using the grouping variable:  #trying... 
pp <- ggplot(df_disp, aes(x=disp_days, y=ps_date, color=group_ps)) +
  #geom_point( size=2, alpha=0.1) +
  scale_color_viridis_d() +
  theme_ipsum()
pp

#using the grouping variable:  #this did not really work... 
df_disp_365 %>%
  ggplot( aes(x=disp_days, y=ps_group_date, color=ps_group_date)) +
  geom_point( size=2, alpha=0.1) +
  scale_color_viridis_d(discrete=TRUE) +
  theme_ipsum()

df_disp_365 %>%
  ggplot( aes(x=disp_days, y=ps_group_date, color=ps_group_date)) +
  geom_point( size=2, alpha=0.1) +
  scale_color_viridis_d() +
  theme_ipsum()

df_disp_365 %>%
  ggplot( aes(x=disp_days, y=ps_group_date, color=ps_group_date)) +
  geom_point( size=2, alpha=0.1) +
  scale_color_viridis_d()



#### random notes: ####

# counting observations for each group ####
#I count the occurence of each couple of values.

#df_disp$ps_group_count <- 0
#df_disp$ps_group_count   # this makes a value for each obs - wrong

ps_group_count <- 1:20
ps_group_count

df_disp$ps_group == 1

count(
  df_disp,
  df_disp$ps_group == 1,
  wt = NULL,
  sort = FALSE,
  name = NULL,
  .drop = FALSE
)
#  df_disp$ps_group == 1   n
#1                 FALSE 336
#2                  TRUE  16

ps_group_count[1] = 16
ps_group_count[1]
ps_group_count

count(
  df_disp,
  df_disp$ps_group == 5,
  wt = NULL,
  sort = FALSE,
  name = NULL,
  .drop = FALSE
)

ps_group_count[4] = 0

df_disp$ps_group
