# Script 4. PLOTTING TEXTUAL DATA (FIGURE 1)

## empty memory (!)
rm(list=ls())

#set wd: 
setwd("C:/Users/lse043/OneDrive - University of Bergen/Documents/0 PhD PROJECT/PAPER 2 'Strategies'/Ghana_RTI_media_advocacy")

# dataframe:
df_final <- readRDS("Data/dataframe_final.rds")
str(df_final)


# 1. SUBSETTING DATAFRAME ####

# not including press statements
df_news <- subset.data.frame(df_final, source == "GhanaWeb")

# not including news stories before any press statements
df_news_ps <- subset.data.frame(df_news, ps_id > "0")

# not including news stories after 31-dec-2019: 
df_news_ps <- subset(df_news_ps, df_news_ps$date < "2020-01-01")


# inspecting the news stories per press statement groups

# range of PS groups 
min(df_news_ps$disp_days) #[1] 0.4583333

#inspecting per group: 
# group 1
df_news_ps_1 <- subset.data.frame(df_news_ps, ps_id == "1")
max(df_news_ps_1$disp_days) #[1] 806.4167
min(df_news_ps_1$disp_days) #[1] 10.45833

# group 18 
df_news_ps_18 <- subset.data.frame(df_news_ps, ps_id == "18")
max(df_news_ps_18$disp_days) #[1] 256.5417
min(df_news_ps_18$disp_days) #[1] 1.5


# 2. COUNTING FREQUENCY OF NEWS STORIES   ####
str(df_news_ps)

# counting news stories per date: 
myvars <- c("date", "doc_id")
dates <- df_news_ps[myvars]
dates$date <- as.character(dates$date)
str(dates)

detach("package:dplyr", unload = TRUE)
count <- as.matrix(count(dates$date))
count_df <- as.data.frame(count)   # makes freq variable

library(dplyr)
glimpse(count_df)

# change labels and format
count_df <- rename(count_df, c('date'='x'))
count_df$date <- as.POSIXct(count_df$date)
str(count_df$date)

# 3. MERGING AND SAVING ####

# merging 
df_news_ps <- left_join(df_news_ps, count_df, by = c("date"))
glimpse(df_news_ps)

# saving 
##saveRDS(df_news_ps, file = "Data/df_plot_news_dispersion")


# 4. PLOTTING FIGURE 1   ####

# loading dataframe
df_news_ps <- readRDS("Data/df_plot_news_dispersion")

# packages: 
library(ggplot2)
library(dplyr)
library(viridis)
require("ggrepel")


# using: df_news_ps
glimpse(df_news_ps)

# making new variable for label
glimpse(df_news_ps)
df_news_ps$ps_date2 <- format(df_news_ps$ps_date, format="%Y-%m-%d")
df_news_ps$group <- apply(df_news_ps,1 ,function(x) paste0(toString(x["ps_id"]), sep=": ", toString(x["ps_date2"])))

# subsetting
df_disp <- subset(df_news_ps, select=c(date, ps_date, ps_id, disp_days, group, freq))
str(df_disp)

# preparing variables
df_disp$freq <- as.factor(df_disp$freq)

# limiting to one year after issuing
df_disp_365 <- subset(df_disp, disp_days < 365,
                      select=c(date, ps_id, ps_date, disp_days, group, freq))


# final plot (Figure 1): 
p <- ggplot(df_disp_365, aes(x=disp_days, y=factor(group), color=freq, size=freq)) + 
  geom_point() +
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5)) +
  labs(title=" ", x="Days passed since press statement", y = "Issuing of press statement", color = "Amount", size = "Amount") + 
  theme(text = element_text(size=25))

# blues: 
p + scale_color_manual(values=c("#7fcdbb", "#41b6c4", "#1d91c0", "#225ea8", "#253494", "#081d58")) + scale_size_manual(values=c(4,5,6,7,8,9))


# THE END  -----------------------------
