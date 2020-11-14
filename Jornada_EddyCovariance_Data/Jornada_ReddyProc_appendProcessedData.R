############################################
#  Process filtered EC data in ReddyProc   #
#   for appending regular processing steps #
#           written by: M. Mauritz         #
#             November 2020                #
############################################

library(REddyProc)
library(data.table)
library(lubridate)
library(gridExtra)
library(dplyr)
library(ggplot2)
library(gtable)
library(grid)
library(zoo)
library(bit64)

# import filtered flux data file from Eddy Pro as data table
# filtered in: Jornada_EddyPro_Output_Fluxnext_appendProcessedData.R
setwd("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_EddyPro_filtered")
 load("JER_flux_20200131_EddyPro_Output_filtered_SD_TIMEcorr_20201114.Rdata")

# convert date to POSIXct and get a year, day, hour column
# if this step doesn't work, make sure bit64 library is loaded otherwise the timestamps importa in a non-sensical format
flux2020_filter_sd[,':='(Year=year(date_time),DoY=yday(date_time),
        hours = hour(date_time), mins = minute(date_time))]

# make sure there's no duplicated data
flux_filter <- (flux2020_filter_sd[!(duplicated(flux2020_filter_sd, by=c("date_time")))])

# exclude FC, LE, H data where FC_SSITC_TEST==1 because that data should only be used for budgets, not gap-filling

ggplot(flux_filter, aes(date_time,FC,colour=factor(FC_SSITC_TEST)))+
  geom_point()


ggplot(flux_filter, aes(date_time,LE,colour=factor(LE_SSITC_TEST)))+
  geom_point()

ggplot(flux_filter, aes(date_time,H,colour=factor(H_SSITC_TEST)))+
  geom_point()


# remove 1s
edata <- copy(flux_filter)

edata[FC_SSITC_TEST==1, FC := NA]
edata[LE_SSITC_TEST==1, LE := NA]
edata[H_SSITC_TEST==1, H := NA]

# format data columns for ReddyProc
# Year	DoY	Hour	NEE	LE	H	Rg	Tair	Tsoil	rH	VPD	Ustar 

edata[mins==0, Hour := hours+0.0]
edata[mins==30, Hour := hours+0.5]

edata <- edata[,.(Year,
                 DoY,
                 Hour,
                 FC,
                 LE,
                 H,
                 SW_IN_1_1_1,
                 TA_1_1_1,
                 RH_1_1_1,
                 USTAR)]

ggplot(edata, aes(DoY,FC))+
  geom_line()+
  facet_grid(Year~.)

setnames(edata,c("FC","SW_IN_1_1_1","TA_1_1_1","RH_1_1_1","USTAR"),
         c("NEE","Rg","Tair","rH","Ustar"))

# make all Rg<0 equal to 0 becuase ReddyProc won't accept values <0
 edata[Rg<0, Rg:=0]

 # remove 2019 because that belongs to the following year
 # edata <- edata[Year!=2019,]
 
 
 # create a grid of full dates and times
 filled <- expand.grid(date=seq(as.Date("2020-01-01"),as.Date("2020-01-31"), "days"),
                       Hour=seq(0,23.5, by=0.5))
 filled$Year <- year(filled$date)
 filled$DoY <- yday(filled$date)
 
 filled$date <- NULL
 
 edata <- merge(edata,filled,by=c("Year","DoY","Hour"), all=TRUE)

  # if FC in the first row is NA, remove it
  edata <- edata[!(Year==2020 & DoY == 1 & Hour == 0.0)]
 
 
 # online tool says hours must be between 0.5 and 24.0 
 # therefore add 0.5 to each hour
 edata[,Hour := Hour+0.5]
 
  # check that all days have 48 points
 daylength <- edata[,list(daylength=length(Hour)),by="Year,DoY"]
 
 # convert edata to data frame for ReddyProc
 edata <- as.data.frame(edata)
 
# calculate VPD from rH and Tair in hPa (mbar), at > 10 hPa the light response curve parameters change
edata$VPD <- fCalcVPDfromRHandTair(edata$rH, edata$Tair)


# online tool says missing values must be -9999, convert all NA to -9999
edata[is.na(edata)]=-9999

# export data for online tool of ReddyProc,
# with timesstamp corrected
# write.table(edata, file="~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/ReddyProc/20200427/JER_ReddyProc_Input_20200131_20201114.txt", sep=" ", dec=".",row.names=FALSE)

