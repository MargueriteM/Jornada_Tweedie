############################################
# Combine filtered EC data and full Biomet #
#  Ameriflux submission: No gapfill or U*  #
#           written by: M. Mauritz         #
#             August 2019                  #
############################################

library(data.table)
library(lubridate)
library(ggplot2)
library(bit64)

# import filtered flux data file from Eddy Pro as data table
# filtered in: Jornada_EddyPro_Output_Fluxnext_2010_2019.R
setwd("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_EddyPro_filtered")

# import data that was filtered by 3SD filter
load("JER_flux_2010_2019_EddyPro_Output_filtered_SD_20200212.Rdata")

# convert date to POSIXct and get a year, day, hour column
# if this step doesn't work, make sure bit64 library is loaded otherwise the timestamps importa in a non-sensical format
flux_filter_sd[,':=' (date_time = parse_date_time(TIMESTAMP_START,"YmdHM",tz="UTC"))]

# there's duplicated data in 2012 DOY 138
flux <- (flux_filter_sd[!(duplicated(flux_filter_sd, by=c("date_time")))])


# import biomet2 which contains all sensors as individual datastreams
setwd("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/MetDataFiles_EP")

biomet2 <- fread(file="Biomet_USJo1_wide_2010_2019_20200214.csv", sep=",", dec=".", na.strings=c("NA","NaN"),
                 header=TRUE)

biomet2[,':=' (date_time = parse_date_time(date_time,"Ymd HMS",tz="UTC"))][,TIMESTAMP]

# merge the flux data and biomet2 data
# first remove the biomet columns from the flux data
exclude_cols <- c("TA_1_1_1", "RH_1_1_1", "PA_1_1_1", "WD_1_1_1", "MWS_1_1_1", "PPFD_IN_1_1_1", "PPFD_OUT_1_1_1", 
                  "P_RAIN_1_1_1", "SWC_1_1_1", "TS_1_1_1", "G_1_1_1", "G_1_2_1", "G_2_1_1", "G_2_2_1",
                  "LW_IN_1_1_1", "LW_OUT_1_1_1", "SW_OUT_1_1_1", "SW_IN_1_1_1", "NETRAD_1_1_1", "date",
                  "month","year") 

flux <- flux[,!c(exclude_cols),with=FALSE]

flux.biomet <- merge(flux,biomet2, by="date_time", all.x=TRUE)

# save to upload to ameriflux: 
# <SITE_ID>_<RESOLUTION>_<TS-START>_<TS-END>_<OPTIONAL>.csv
setwd("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/Ameriflux")

write.table(flux.biomet, paste("USJo1_HH",min(flux.biomet$TIMESTAMP_START),max(flux.biomet$TIMESTAMP_START),
                               "20200214submit.csv",sep="_"), sep=',', dec='.', row.names=FALSE)


