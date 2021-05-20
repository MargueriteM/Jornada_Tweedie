#############################################################################################
# This code is to read lists of noon photo images for Phenocam and format for PhenoAnalyzer #
#     written by: M. Mauritz, 20 May 2021                                                   #
#############################################################################################

# add libraries
library(lubridate)
library(dplyr)
library(ggplot2)

# set working directory to SEL shared file with file lists
setwd("/Volumes/SEL_Data_Archive/Research Data/Desert/Jornada/Bahada/Phenocam/TwrPhenocam/lists")

# list multiple files
# noonfiles <- list.files(path="~/Desktop/TweedieLab/Projects/Jornada/Data/Tower/Climate/RawFiles", full.names=TRUE) 


noon2021 <- read.table("2021_noon_cam1.txt", sep=",", header=TRUE)


# put timestamp into POSIXct format (date/time format for R)
noon2021 <- noon2021 %>% # 'pipe command' which allows sequential exectution
  mutate(timestamp2 = ymd_hms(timestamp)) 


# save only filepath and timestamp2 for PhenoAnalyzer to read lists
noon2021 %>%
  select(full.path, timestamp2)%>%
write.table("2021_noon_cam1_PhenoAnalyzer.txt", sep=",", row.names=FALSE, quote=FALSE)


