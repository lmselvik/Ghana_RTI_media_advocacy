# Script 5. ANALYSIS and PLOTTED RESULTS (FIGURE 2 & APPENDIX D)

## empty memory (!)
rm(list=ls())

#set wd: 
setwd("C:/Users/lse043/OneDrive - University of Bergen/Documents/0 PhD PROJECT/PAPER 2 'Strategies'/Ghana_RTI_media_advocacy")

# dataframe:
df_final <- readRDS("Data/dataframe_final.rds")

# Variables: 
str(df_final)
# df$date = date of news story (or ps)
# df$ps_id = which groups, numbered
# df$ps_date = date of corresponding ps, in that group
# df$disp_days = number of days from news story to (corresponding) press statement in that group


# 1. PREPARING CORPUS #### 
df_sim <- subset(df_final, select=c(text, source, doc_id, date, ps_date, ps_id, disp_days))
df_sim <- subset.data.frame(df_sim, ps_id > 0 )

# making corpus
library(quanteda)
corp <- corpus(df_sim, docid_field = 'doc_id', text_field='text')

head(docvars(corp))
ndoc(corp)
corp[1]
corp["GhanaWeb-1"]

# pre-processing texts and creating the DTM
dtm <- dfm(corp,
           tolower = TRUE,
           stem = TRUE,
           remove = stopwords("english"),
           remove_punct = TRUE,
           remove_numbers = TRUE)
dtm
#Document-feature matrix of: 372 documents, 7,342 features (97.6% sparse) and 5 docvars.

# subsetting dfm by group
dtm_18 <- dfm_subset(dtm, ps_id == 18)
dtm_17 <- dfm_subset(dtm, ps_id == 17)
dtm_16 <- dfm_subset(dtm, ps_id == 16)
dtm_15 <- dfm_subset(dtm, ps_id == 15)
dtm_14 <- dfm_subset(dtm, ps_id == 14)
dtm_13 <- dfm_subset(dtm, ps_id == 13)
dtm_12 <- dfm_subset(dtm, ps_id == 12)
dtm_11 <- dfm_subset(dtm, ps_id == 11)
dtm_10 <- dfm_subset(dtm, ps_id == 10)
dtm_09 <- dfm_subset(dtm, ps_id == 9)
dtm_08 <- dfm_subset(dtm, ps_id == 8)
dtm_07 <- dfm_subset(dtm, ps_id == 7)
dtm_06 <- dfm_subset(dtm, ps_id == 6)
dtm_05 <- dfm_subset(dtm, ps_id == 5)
dtm_04 <- dfm_subset(dtm, ps_id == 4)
dtm_03 <- dfm_subset(dtm, ps_id == 3)
dtm_02 <- dfm_subset(dtm, ps_id == 2)
dtm_01 <- dfm_subset(dtm, ps_id == 1)

#inspecting
head(docvars(dtm_10), 3)
tail(docvars(dtm_10), 3) 


# 2. COMPARE AND CALCULATE TEXTUAL SIMILARITIES #### 
library(RNewsflow)
library(dplyr)

# calculating document similarities
# and extracting relevant info:
# inspecting document (vertices) and document pairs (edges) and attributes
# for edges, i.e. document pairs: 'weight' represents the similarity score

# 1: =================
docvars(dtm_01)
# disp_days (to the PS)
# min: 10.45833
# max: 806.41667
# 2010-02-15
g_1 <- newsflow_compare(dtm_01, date_var='date',
                        hour_window = c(0.1,2232),  # window set to 3 months = 31 days x 3 months x 24 hours
                        min_similarity = 0, 
                        only_complete_window = F)

e_1 <- as_data_frame(g_1, 'edges')
rti_e_1 <- subset.data.frame(e_1, from == "Coalition-1")
#View(rti_e_1)

# 2: =================
docvars(dtm_02)
# disp_days (to the PS)
# min: 6.458333
# max: 8.458333
# 2012-05-10
g_2 <- newsflow_compare(dtm_02, date_var='date',
                        hour_window = c(0.1,2232),  # window set to 3 months = 31 days x 3 months x 24 hours
                        min_similarity = 0, 
                        only_complete_window = F)

e_2 <- as_data_frame(g_2, 'edges')
rti_e_2 <- subset.data.frame(e_2, from == "Coalition-2")

# 3: =================
docvars(dtm_03)
# disp_days (to the PS)
# min: 0.4583333
# max: 45.4583333
# 2012-05-22
g_3 <- newsflow_compare(dtm_03, date_var='date',
                        hour_window = c(0.1,2232),  # window set to 3 months = 31 days x 3 months x 24 hours
                        min_similarity = 0, 
                        only_complete_window = F)

e_3 <- as_data_frame(g_3, 'edges')
rti_e_3 <- subset.data.frame(e_3, from == "Coalition-3")

# 4: =================
docvars(dtm_04)
# disp_days (to the PS)
# min: 72.45833
# max: 87.45833
# 2012-07-20
g_4 <- newsflow_compare(dtm_04, date_var='date',
                        hour_window = c(0.1,2232),  # window set to 3 months = 31 days x 3 months x 24 hours
                        min_similarity = 0, 
                        only_complete_window = F)

e_4 <- as_data_frame(g_4, 'edges')
rti_e_4 <- subset.data.frame(e_4, from == "Coalition-5")   # Note: only from press statement 5, not 4

# 5: =================
docvars(dtm_05)
# disp_days (to the PS)    # Note: No following press statements
# min: -
# max: -
# 2012-10-22

# 6: =================
docvars(dtm_06)
# disp_days (to the PS)
# min: 6.5 
# max: 32.5
# 2013-02-06
g_6 <- newsflow_compare(dtm_06, date_var='date',
                        hour_window = c(0.1,2232),  # window set to 3 months = 31 days x 3 months x 24 hours
                        min_similarity = 0, 
                        only_complete_window = F)

e_6 <- as_data_frame(g_6, 'edges')
rti_e_6 <- subset.data.frame(e_6, from == "Coalition-8")   # NB: noe rart med nummereringen her... se table i appendix B - sjekk df_final...

# 7: =================
docvars(dtm_07)
# disp_days (to the PS)
# min: 1.5
# max: -
# 2013-04-30

g_7 <- newsflow_compare(dtm_07, date_var='date',
                        hour_window = c(0.1,2232),  # window set to 3 months = 31 days x 3 months x 24 hours
                        min_similarity = 0, 
                        only_complete_window = F)

e_7 <- as_data_frame(g_7, 'edges')
rti_e_7 <- subset.data.frame(e_7, from == "Coalition-9")

# 8: =================
docvars(dtm_08)
# disp_days (to the PS)
# min: 4.5
# max: 85.5
# 2013-05-31
g_8 <- newsflow_compare(dtm_08, date_var='date',
                        hour_window = c(0.1,2232),  # window set to 3 months = 31 days x 3 months x 24 hours
                        min_similarity = 0, 
                        only_complete_window = F)

e_8 <- as_data_frame(g_8, 'edges')
rti_e_8 <- subset.data.frame(e_8, from == "Coalition-10")


# 9: (NB: This group includes the PS without proper date, 2013-08-01 - "Coalition-7") ================= NOPE: Not any more
docvars(dtm_09)
# disp_days (to the PS)
# min: 2.5
# max: 701.5
# 2013-09-27
g_9 <- newsflow_compare(dtm_09, date_var='date',
                        hour_window = c(0.1,2232),  # window set to 3 months = 31 days x 3 months x 24 hours
                        min_similarity = 0, 
                        only_complete_window = F)

e_9 <- as_data_frame(g_9, 'edges')
rti_e_9 <- subset.data.frame(e_9, from == "Coalition-11")

# 10: =================
docvars(dtm_10)
# disp_days (to the PS)
# min: 4.5
# max: 174.5
# 2015-09-28
g_10 <- newsflow_compare(dtm_10, date_var='date',
                         hour_window = c(0.1,2232),  # window set to 3 months = 31 days x 3 months x 24 hours
                         min_similarity = 0, 
                         only_complete_window = F)

e_10 <- as_data_frame(g_10, 'edges')
rti_e_10 <- subset.data.frame(e_10, from == "Coalition-12")

# 11: =================
docvars(dtm_11)
# disp_days (to the PS)
# min: 1.5
# max: 94.5
# 2016-04-15
g_11 <- newsflow_compare(dtm_11, date_var='date',
                         hour_window = c(0.1,2232),  # window set to 3 months = 31 days x 3 months x 24 hours
                         min_similarity = 0, 
                         only_complete_window = F)

e_11 <- as_data_frame(g_11, 'edges')
rti_e_11 <- subset.data.frame(e_11, from == "Coalition-13")

# 12: =================
docvars(dtm_12)
# disp_days (to the PS)
# min: 1.5
# max: 6.5
# 2016-07-28 
g_12 <- newsflow_compare(dtm_12, date_var='date',
                         hour_window = c(0.1,2232),  # window set to 3 months = 31 days x 3 months x 24 hours
                         min_similarity = 0, 
                         only_complete_window = F)

e_12 <- as_data_frame(g_12, 'edges')
rti_e_12 <- subset.data.frame(e_12, from == "Coalition-14")

# 13: =================
docvars(dtm_13)
# disp_days (to the PS)
# min: 1.5
# max:  6.5
# 2016-08-18    # ------------- no news stories 

# 14: =================
docvars(dtm_14)
# disp_days (to the PS)
# min: 1.5
# max: 107.5
# 2016-09-23
g_14 <- newsflow_compare(dtm_14, date_var='date',
                         hour_window = c(0.1,2232),  # window set to 3 months = 31 days x 3 months x 24 hours
                         min_similarity = 0, 
                         only_complete_window = F)

e_14 <- as_data_frame(g_14, 'edges')
rti_e_14 <- subset.data.frame(e_14, from == "Coalition-16")

# 15: =================
docvars(dtm_15)
# disp_days (to the PS)
# min: 1.5
# max: 545.4
# 2017-02-01
g_15 <- newsflow_compare(dtm_15, date_var='date',
                         hour_window = c(0.1,2232),  # window set to 3 months = 31 days x 3 months x 24 hours
                         min_similarity = 0, 
                         only_complete_window = F)

e_15 <- as_data_frame(g_15, 'edges')
rti_e_15 <- subset.data.frame(e_15, from == "Coalition-17")

# 16: =================
docvars(dtm_16)
# disp_days (to the PS)
# min: 1.5
# max: 178.5
# 2018-09-28
g_16 <- newsflow_compare(dtm_16, date_var='date',
                         hour_window = c(0.1,2232),  # window set to 3 months = 31 days x 3 months x 24 hours
                         min_similarity = 0, 
                         only_complete_window = F)

e_16 <- as_data_frame(g_16, 'edges')
rti_e_16 <- subset.data.frame(e_16, from == "Coalition-18")

# 17: =================
docvars(dtm_17)
# disp_days (to the PS)
# min: 0.5
# max: 8.4
# 2019-03-27 
g_17 <- newsflow_compare(dtm_17, date_var='date',
                         hour_window = c(0.1,2232),  # window set to 3 months = 31 days x 3 months x 24 hours
                         min_similarity = 0, 
                         only_complete_window = F)

e_17 <- as_data_frame(g_17, 'edges')
rti_e_17 <- subset.data.frame(e_17, from == "Coalition-19")

# 18: =================
docvars(dtm_18)
# disp_days (to the PS)
# min: 1.5
# max: 385.5 / 256.5
# 2019-04-15
g_18 <- newsflow_compare(dtm_18, date_var='date',
                         hour_window = c(0.1,2232),  # window set to 3 months = 31 days x 3 months x 24 hours
                         min_similarity = 0, 
                         only_complete_window = F)

e_18 <- as_data_frame(g_18, 'edges')
rti_e_18 <- subset.data.frame(e_18, from == "Coalition-20")


# 3. COMPILING SIMILARITY SCORES  ####

# combining all rti_e objects
rti_e <- rbind(rti_e_1, rti_e_2, rti_e_3, rti_e_4, rti_e_6, rti_e_7, rti_e_8, rti_e_9, rti_e_10, rti_e_11, rti_e_12, rti_e_14, rti_e_15, rti_e_16, rti_e_17, rti_e_18)

library(dplyr)
rti_e <- rti_e %>% 
  rename(doc_id = to)

df_rti_sim <- left_join(df_sim, rti_e, by = c("doc_id"))


## Saving dataframe for analysis ####
##saveRDS(df_rti_sim, file = "Data/df_similarity_scores")

# load:
#df_rti_sim_copy <- readRDS("Data/df_similarity_scores")


## preparing to plot:

# ID as numeric
df_rti_sim$ps_id <- as.numeric(df_rti_sim$ps_id)

# ordered dataframe by date
df_rti_sim <- df_rti_sim[order(df_rti_sim$date),]

# removing missing obs
df_2 <- na.omit(df_rti_sim)
#View(df_2)
# note: remove the one left with source = RTI Coalition
df_3 <- filter(df_2, doc_id != "Coalition-7")
str(df_3)   #177 obs. of  11 variables


# 4. PLOTTING  ####
library(ggplot2)
library(dplyr)

# nice ID label variable 
df_3$ps_id_date <- format(df_3$ps_date, format="%Y-%m-%d")
df_3$ps_id_label <- apply(df_3,1 ,function(x) paste0("Press statement ", toString(x["ps_id"]), sep=": ", toString(x["ps_id_date"])))


## Plotting Figure 2   ####

# only relevant press statement groups:

df_fig2 <- df_3 %>% 
  filter(ps_id %in% c(1,3,8,9,11,14,15,16,18))

# plot
df_fig2$ps_id <- as.factor(df_fig2$ps_id)

pp <- ggplot(df_fig2, aes(x=disp_days, y=weight, group=ps_id_label)) + 
  geom_point(alpha = 0.5) +  #, ymax = 1 ## no effect
  geom_smooth( , se = F, colour = "#7570B3") +
  theme_bw()+
  ylim(0, 1)+
  xlab("Time since issuing of press statement (in days)")+
  ylab("News stories' similarity with preceding press statement")+
  labs(fill = "")+
  facet_wrap(~ps_id_label)

pp + theme(legend.title = element_blank(), text = element_text(size=20)) 


## Plotting Figure A1, Appendix D   ####
# with all press statement groups

# plot
df_3$ps_id <- as.factor(df_3$ps_id)

ppp <- ggplot(df_3, aes(x=disp_days, y=weight, group=ps_id_label)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth( , se = F, colour = "#7570B3") +
  theme_bw()+
  ylim(0, 1)+
  xlab("Time since issuing of press statement (in days)")+
  ylab("News stories' similarity with preceding press statement")+
  labs(fill = "")+
  facet_wrap(~ps_id_label)

ppp + theme(legend.title = element_blank(), text = element_text(size=20)) 

# THE END  -----------------------------
