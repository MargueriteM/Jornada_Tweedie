#############################################################################################
# This code is to read lists of noon photo images for Phenocam and format for PhenoAnalyzer #
#     written by: M. Mauritz, 20 May 2021                                                   #
#############################################################################################

# add libraries
library(lubridate)
library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)

# set working directory to SEL shared file with file lists

setwd("/Volumes/SEL_Data_Archive/Research Data/Desert/Jornada/Bahada/Phenocam/TwrPhenocam/lists")

# list multiple files
 # create a string for the data path
 data_path <- "/Volumes/SEL_Data_Archive/Research Data/Desert/Jornada/Bahada/Phenocam/TwrPhenocam/lists"
 
 # list all the files with file name only, not full path
 noonfilenames <- list.files(path="/Volumes/SEL_Data_Archive/Research Data/Desert/Jornada/Bahada/Phenocam/TwrPhenocam/lists",
                             full.names=FALSE, pattern="noon")

 # use purrr:map to read all lists and combine with file names
 # https://clauswilke.com/blog/2016/06/13/reading-and-combining-many-tidy-data-files-in-r/
 noonfiles2 <- data_frame(filename = noonfilenames) %>% # create a data frame
   # holding the file names
   mutate(file_contents = map(filename, ~read.csv(file.path(data_path,.), header=TRUE))) # a new data column
 
######## ALTERNATIVE TO noonfiles2 ########
## currently does not work due to brightness factor/numeric problem and headers in middle of files
# noonfiles <- list.files(path="/Volumes/SEL_Data_Archive/Research Data/Desert/Jornada/Bahada/Phenocam/TwrPhenocam/lists",
#                        full.names=TRUE, pattern="noon") %>%
#   map_dfr(read.csv, .id="source")
##########################################
 
#### brightness is a factor in some files, check where the issue is
## check str of all noon lists
# noonfiles2[1,2] %>%
  # map(str)
## go to specific lists that have brightness as a factor
# check2<- unnest(noonfiles2[1,2], cols=c(file_contents))
# check3<- unnest(noonfiles2[13,2], cols=c(file_contents))
####
 
# unnest only the files which have brightness as a number. 2010 and 2013 files for cam 1 have an issue
 noon <- noonfiles2 %>%
   filter(!filename %in% c("2010_noon_cam1.txt", "2013_noon_cam1.txt"))%>%
   unnest(., cols=c(file_contents)) 

# put timestamp into POSIXct format (date/time format for R)
noon <- noon %>% # 'pipe command' which allows sequential exectution
  mutate(timestamp2 = ymd_hms(timestamp)) 


# save only filepath and timestamp2 for PhenoAnalyzer to read lists
setwd("~/Desktop/OneDrive - University of Texas at El Paso/CZ_Drylands/Jornada_REU/noon_lists_test")

test <- noon %>%
tidyr::separate(filename,c("filename2","txt"),".")
group_by(filename)%>%
select(full.path, timestamp2)%>%
group_walk(~write.table(.x, file= paste(.y$filename,
                                          "PhenoAnalyzer.txt",sep="_"),
                          sep =',', dec='.', row.names=FALSE, col.names=FALSE,quote=FALSE)) 


write.table("2021_noon_cam1_PhenoAnalyzer.txt", sep=",",
            row.names=FALSE, col.names=FALSE quote=FALSE)

# save data as txt by filename using group_walk, use filename in the saved name
# https://luisdva.github.io/rstats/export-iteratively/
# https://community.rstudio.com/t/map-write-csv/33292/2
# https://stackoverflow.com/questions/41233173/how-can-i-write-dplyr-groups-to-separate-files


ffp_clim %>%
  group_by(dn_sn) %>%
  select(Year,Month,mDay,Hour,Minutes,zm,DISPLACEMENT_HEIGHT,
         ROUGHNESS_LENGTH,WS,MO_LENGTH,V_SIGMA,USTAR,WD) %>%
  group_walk(~write.table(.x, file= paste("ffp_clim_online_",.y$dn_sn,
                                          ".csv",sep=""),
                          sep =',', dec='.', row.names=FALSE, na="-999", quote=FALSE))


