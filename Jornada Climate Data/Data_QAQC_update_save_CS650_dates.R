###############################################
# Get the most recent data to QA/QC and save  #
#          Soil CS650 probe Data              #
#      written by: M. Mauritz                 #
#            date: 7 December 2019            #
###############################################

# This code will:
# 1. allow data to be checked
# 2. run standard range filters determined from 2010-2019 and input from Ameriflux
# 3. allow year-specific data removal based on visual checks
# 4. save data with date/time in file name of SoilSensor_CS650/year/QAQC folder on server
# 5. save a full year of data to SoilSensor_CS650/Combined with only year in filename
# 6. save a csv file of this code as Data_QAQC_Code_yyyy.csv (or with date/time) to SoilSensor_CS650/year/QAQC folder to record data filter steps

# Notes on CS650 12cm Installation at US-Jo1 for Dryland CZ Project
# 27 Mar 2021

# Probes installed (Lin Ma, Mark Engle, Nohemi Garay Valenzuela, Christian L Leach)

# Installation start~11:05am 
# Installation and testing end: ~5pm

# All read 1.9-2.2% VWC at same location

# SDI, SN, Depth (cm) of sensors, Location relative to Caliche
# SDI 1 SN 46381 Depth 100.5cm Caliche inside/below
# SDI 2 SN 46371 Depth 42.5cm Caliche above
# SDI 3 SN 46374 Depth 25.5 cm Caliche above
# SDI 4 SN 46416 Depth 17.5 cm Caliche above
# SDI 5 SN 46414 Depth 11.5 cm Caliche above

# Data logging starts 2021-05-04 12:44:00 

# load libraries
library(tidyverse)
library(lubridate)
library(ggplot2)

year_file <- 2022

# Based on data checks, no data form Met and CS650 from 16 Dec 17:30 to 17 Jan 2022

# set working directory to server
setwd(paste("/Volumes/SEL_Data_Archive/Research Data/Desert/Jornada/Bahada/Tower/SoilSensor_CS650/",year_file,"/Raw_Data/ASCII",sep=""))

# load headers of file and data
cs650names <- colnames(read.table(paste("dataL1_Soil_",year_file,".csv",sep=""), sep=",", skip=1,heade=TRUE))
cs650wide <- read.table(paste("dataL1_Soil_",year_file,".csv",sep=""), sep=",", skip=4, col.names = cs650names, na.strings="-9999")

# convert to long format
cs650 <- cs650wide %>%
  pivot_longer(-c(TIMESTAMP,RECORD),names_to="IDcol") %>%
  separate(IDcol, c(NA,"metric","probe",NA), sep="_")

# format date/time and create depth labels for probes
cs650 <- cs650 %>%
  mutate(date_time = ymd_hms(TIMESTAMP)) %>%
  mutate(probe_depth = case_when(probe %in% "1" ~ 100.5,
                                 probe %in% "2" ~ 42.5,
                                 probe %in% "3" ~ 25.5,
                                 probe %in% "4" ~ 17.5,
                                 probe %in% "5" ~ 11.5))


# check file start and end date
startdate.check <- (min(cs650$date_time))
enddate.check <- (max(cs650$date_time))


# graph different metrics: "EC"  "P"   "PA"  "T"   "VR"  "VWC"
# graph all metrics
cs650 %>%
 # filter( !is.na(value) & value>0) %>%
  ggplot(., aes(date_time, value, colour=factor(probe_depth)))+
  geom_line()+
  facet_grid(metric~., scales="free_y")

# graph VWC
 cs650 %>%
  filter(metric %in% c("VWC") & !is.na(value)) %>%
  ggplot(., aes(date_time, value, colour=factor(probe_depth)))+
  geom_line()+
  labs(y = "VWC")+
  theme(legend.position="bottom")


# graph T and VWC
cs650 %>%
  filter(metric %in% c("T","VWC") & !is.na(value)) %>%
  ggplot(., aes(date_time, value, colour=factor(probe_depth)))+
  geom_line()+
  labs(title = "Soil Temperature")+
  facet_grid(metric~., scales="free_y")+
  theme(legend.position="bottom")

# 2021
# Data starts 2021-05-04 12:44:00 Temperature blipped to 0 in all probes on 2021-05-05
# remove this row

# cs650 <- cs650 %>%
#  filter(!(as.Date(date_time) == as.Date("2021-05-05") & value == 0))

# graph T and VWC
cs650 %>%
  filter(metric %in% c("T","VWC") & date_time > ymd_hms("2021-05-04 12:43:00")) %>%
  ggplot(., aes(date_time, value, colour=factor(probe_depth)))+
  geom_line()+
  labs(title = "Soil Moisture and Temperature", colour="Probe Depth (cm)")+
  facet_grid(metric~., scales="free_y")+
  theme_bw()+
  theme(legend.position="bottom")

# graph all metrics
cs650 %>%
  #filter( !is.na(value) & value>0) %>%
  ggplot(., aes(date_time, value, colour=factor(probe_depth)))+
  geom_line()+
  facet_grid(metric~., scales="free_y")

# 2022 data looks good until "2022-06-01 11:30:00 UTC"

# prepare for saving for L2 tables and combination with other data

# save to QAQC folder on data archive
startdate <- (min(cs650$date_time))
enddate <- (max(cs650$date_time))

# make data wide again and return to original names
cs650.save <- cs650 %>%
  mutate(probeID = paste("CS650",metric,probe,sep="_")) %>%
  select(date_time, probeID,value)%>%
  spread(probeID,value)%>%
  rename(TIMESTAMP=date_time)

# add information on the data logger, column names, units in the same format as the raw datafile
# climate.save <- rbind(climate.loggerinfo,climate.colnames, climate.save)

# # save in QAQC folder with start and end date in the file name
qaqc.path<- paste("/Volumes/SEL_Data_Archive/Research Data/Desert/Jornada/Bahada/Tower/SoilSensor_CS650/",year_file,"/QAQC/", sep="")
setwd(qaqc.path)


# write.table(cs650.save,
#             paste("dataL2_Soil_",year(startdate),sprintf("%02d",(month(startdate))),sprintf("%02d",(day(startdate))),
#                   sprintf("%02d",(hour(startdate))),sprintf("%02d",(minute(startdate))),
#                   sprintf("%02d",(second(startdate))),
#                   "_",
#                   year(enddate),sprintf("%02d",(month(enddate))),sprintf("%02d",(day(enddate))),
#                   sprintf("%02d",(hour(enddate))),sprintf("%02d",(minute(enddate))),
#                   sprintf("%02d",(second(enddate))), ".csv",sep=""),
#             sep=",", dec=".", row.names=FALSE)

# save file with only year in name to QAQC file along with date range info in seperate file
write.table(cs650.save,
            paste("dataL2_Soil_",year_file, ".csv",sep=""),
            sep=",", dec=".", row.names=FALSE)



# save file range to QAQC folder on data archive
startdate <- (min(cs650$date_time))
enddate <- (max(cs650$date_time))

# save a text file that says date that code was run (system time), start and end date of data
run.info <- data.frame(info=c("Data_start","Data_end","Date_processed"),
                       date_time=c(startdate,enddate,ymd_hms(Sys.time(),tz="UTC")))

write.table(run.info, "dataL2_Soil_DateRange.csv",
            sep=",", dec=".", row.names=FALSE)


# Save data to Combined folder with only year name
difftime(startdate,enddate)

setwd("/Volumes/SEL_Data_Archive/Research Data/Desert/Jornada/Bahada/Tower/SoilSensor_CS650/Combined")

write.table(cs650.save,
            paste("dataL2_Soil_",year_file, ".csv",sep=""),
            sep=",", dec=".", row.names=FALSE)

write.table(run.info,
            paste("dataL2_Soil_DateRange_",year_file,".csv",sep=""),
            sep=",", dec=".", row.names=FALSE)


# save the R script that went along with creating the file to have a record of QA/QC
# use rstudioapi to get the path of the current script and then copy it to the 
# server location

# http://theautomatic.net/2018/07/11/manipulate-files-r/ 
# file.copy("source_file.txt", "destination_folder")

# file.rename(from = rstudioapi::getActiveDocumentContext()$path,
#            # to = file.path("/Volumes/SEL_Data_Archive/Research Data/Desert/Jornada/Bahada/Tower/TowerClimate_met/2019/QAQC/",
#            to = file.path("~/Desktop",                
#            paste("Data_QAQC_update_save_Climate_",year(startdate),sprintf("%02d",(month(startdate))),sprintf("%02d",(day(startdate))),
#                                  sprintf("%02d",(hour(startdate))),sprintf("%02d",(minute(startdate))),
#                                 sprintf("%02d",(second(startdate))),
#                                 "_",
#                                  year(enddate),sprintf("%02d",(month(enddate))),sprintf("%02d",(day(enddate))),
#                                  sprintf("%02d",(hour(enddate))),sprintf("%02d",(minute(enddate))),
#                                 sprintf("%02d",(second(enddate))), ".csv",sep="")))

file.copy(from = rstudioapi::getActiveDocumentContext()$path,
          to = file.path(qaqc.path,
                         #to = file.path("~/Desktop",                
                         paste("Data_QAQC_Code_",year_file, ".csv",sep="")))



