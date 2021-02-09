# 1. Saving data: webscraped news stories

#set wd: 
setwd("C:/Users/lse043/OneDrive - University of Bergen/Documents/0 PhD PROJECT/PAPER 2 'Strategies'/Ghana_RTI_media_advocacy")

#reading from previous 
load("C:/Users/lse043/OneDrive - University of Bergen/Documents/0 PhD PROJECT/PAPER 2 'Strategies'/ANALYSIS/1 Getting data/.RData")
# Save the entire workspace anew:
#save.image(file = "old_import.RData")


#saving: #### 

# Save a single object to a file
saveRDS(val_gw_190620, "GhanaWeb_374_(190620).rds")
# Restore it
gw_374 <- readRDS("GhanaWeb_374_(190620).rds")


# Save multiple objects
save(val_gw_00ish, val_gw_100ish, val_gw_200ish, val_gw_300ish, val_gw_400ish, val_gw_500ish, file = "GhanaWeb_data_per_100.RData")
# To load the data again
load("GhanaWeb_data_per_100.RData")


## empty memory (!)
rm(list=ls())



