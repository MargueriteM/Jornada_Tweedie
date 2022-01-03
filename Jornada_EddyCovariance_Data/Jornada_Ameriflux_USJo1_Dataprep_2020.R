############################################
# Combine filtered EC data and full Biomet #
#  Ameriflux submission: No gapfill or U*  #
#           written by: M. Mauritz         #
#             August 2019                  #
############################################

# 20201227: update with PPFD_1_1_1 removed in 2010 & 2011, LWS rescaled to 0-100, SWC_1_1_1 amd 3_3_1 baseline jumps removed
# 20200427: update with timestamp corrected biomet and flux data 

library(data.table)
library(lubridate)
library(ggplot2)
library(bit64)

# import filtered flux data file from Eddy Pro as data table
setwd("/Volumes/SEL_Data_Archive/Research Data/Desert/Jornada/Bahada/Tower/EddyCovariance_ts/2020/EddyPro_Out/")

flux_filter_sd <- fread("JER_flux_2020_EddyPro_Output_filtered_SD.csv",sep=",", dec=".",
            header = TRUE)


# rename columns (only up to 2019 that had a timestamp/daylight savings issue)
# TIMESTAMP_START = TIMESTAMP_START_correct
# TIMESTAMP_END = TIMESTAMP_END_correct
# setnames(flux_filter_sd,c("TIMESTAMP_START_correct","TIMESTAMP_END_correct"),
#         c("TIMESTAMP_START","TIMESTAMP_END"))

# don't need for 2020: date_time already = TIMESTAMP_END and fread imports as POSIXct
# convert date to POSIXct and get a year, day, hour column
# if this step doesn't work, make sure bit64 library is loaded otherwise the timestamps import in a non-sensical format
# flux_filter_sd[,date_time := NULL]
# flux_filter_sd[,':=' (date_time = parse_date_time(TIMESTAMP_END,"YmdHM",tz="UTC"))]

# find duplicated rows based on timestamp 
flux <- (flux_filter_sd[(duplicated(flux_filter_sd, by=c("date_time")))])


# remove dataL1_ts_20191231_0000.csv row. That's the last timestamp of 2019 and shouldn't be in 2020
flux <- flux_filter_sd[!(FILENAME_HF == "dataL1_ts_20191231_0000.csv")]

# import biomet2 which contains all sensors as individual datastreams
setwd("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/MetDataFiles_EP/Biomet2_20201227")

biomet_all <- fread("Biomet_USJo1_wide_2020_.csv", sep=",",dec=".", header=TRUE)

biomet_all[,':=' (date_time = parse_date_time(date_time,"Ymd HMS",tz="UTC"))]

# graph
ggplot(biomet_all, aes(date_time,P_RAIN_1_1_1))+
  geom_line()

# merge the flux data and biomet2 data
# first remove the biomet columns from the flux data
exclude_cols <- c("TA_1_1_1", "RH_1_1_1", "PA_1_1_1", "WD_1_1_1", "MWS_1_1_1", "PPFD_IN_1_1_1", "PPFD_OUT_1_1_1", 
                  "P_RAIN_1_1_1", "SWC_1_1_1", "TS_1_1_1", "G_1_1_1", "G_1_2_1", "G_2_1_1", "G_2_2_1",
                  "LW_IN_1_1_1", "LW_OUT_1_1_1", "SW_OUT_1_1_1", "SW_IN_1_1_1", "NETRAD_1_1_1") 

flux <- flux[,!c(exclude_cols),with=FALSE]

flux.biomet <- merge(flux,biomet_all, by="date_time", all.x=TRUE)


# format all columns to be in the same order: 
names_all <- colnames(flux.biomet[,!c("TIMESTAMP_START","TIMESTAMP_END"),with=FALSE])
names_output <- c("TIMESTAMP_START","TIMESTAMP_END",names_all)

setcolorder(flux.biomet,names_output)

# graph to check
ggplot(flux.biomet, aes(date_time, LE))+geom_line()

# save to upload to ameriflux, save to server: 
# <SITE_ID>_<RESOLUTION>_<TS-START>_<TS-END>_<OPTIONAL>.csv
setwd("/Volumes/SEL_Data_Archive/Research Data/Desert/Jornada/Bahada/Tower/Ameriflux_USJo1")

# save file in Ameriflux format
write.table(flux.biomet[,!c("date_time"),with=FALSE], paste("USJo1_HH",min(flux.biomet$TIMESTAMP_END),max(flux.biomet$TIMESTAMP_END),
                               ".csv",sep="_"), sep=',', dec='.', row.names=FALSE)


# # save by years
# for (i in 2010:2019){
#   # subset each year
#   dat.save <- flux.biomet[year(date_time)==i,]
#   
#   write.table (dat.save[,!c("date_time"),with=FALSE],
#              file= paste("US-Jo1_HH",min(dat.save$TIMESTAMP_END),max(dat.save$TIMESTAMP_END),
#                          "20200514submit.csv",sep="_"),
#              sep =',', dec='.', row.names=FALSE, na="-9999", quote=FALSE)
# }


