# Rscript to process a smaller section of EC data and then append it to already-processed data. 
# This code follows steps laid out in Jornada_EddyPro_Output_Fluxnet_2010_2019_20200212.R
# for examples use the demofileflux_add.Rdata and skip steps with flux_filter_sd

# The data get filtered by unlikely values and a 3-day running mean standard deviation of 3
# filter columns are coded:
# filter_fc = 1 to remove based on unreasonable value = 2 based on runing mean smoothing, and = 0 to keep

# load libraries
library(ggplot2) # library for making figures in ggplot package
library(plotly)
library(lubridate) # library for easier date manipulation 
library(data.table) # library for data table which is more efficient with large data sets
library(reader)
library(tidyr)
library(stringr) # allows string manipulation
library(lsr) # contains quantileCut function
library(gridExtra)
library(viridis)
library(zoo)
#############
# IMPORT DATA
#############

# Process Closed Path Data
# load the previously processed data: 
flux_filter_sd <- fread("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Bahada/Tower/ts_data_2/2023/EddyPro_Out/ClosedPath/JER_flux_2023_EddyPro_Output_filtered_SD_JuneDec_Closed.csv",
                        sep=",", header=TRUE, na.strings=c("-9999","NA"),fill=TRUE)

flux_filter_sd[,':=' (date_time = parse_date_time(TIMESTAMP_END,"YmdHM",tz="UTC"))][
  ,':='(date=as.Date.POSIXct(date_time),month=month(date_time),year=year(date_time))]


# read the data in fluxnet format that you want to append to other data
# or read demofile instead
# closed path from 2023
 flux_add <- fread("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Bahada/Tower/ts_data_2/2024/EddyPro_Out/ClosedPath/eddypro_JER_2024_JanMay_Closed_fluxnet_2024-06-11T182817_exp.csv",
                  sep=",", header=TRUE, na.strings=c("-9999"),fill=TRUE)

 flux_add[,':=' (date_time = parse_date_time(TIMESTAMP_END,"YmdHM",tz="UTC"))][
  ,':='(date=as.Date.POSIXct(date_time),month=month(date_time),year=year(date_time))]

# remove duplicate data
flux_add <- (flux_add[!(duplicated(flux_add, by=c("TIMESTAMP_START")))])

# make sure the data are ordered:
flux_add <- flux_add[order(TIMESTAMP_START),]


# make some plots
# graph some met variables
ggplot(flux_add,aes(date_time,P_RAIN_1_1_1))+geom_line()
# a bug in eddy pro means that when data are insufficient, biomet alignment goes wrong
ggplot(flux_add[!(FILENAME_HF %in% ("not_enough_data")),],aes(date_time,P_RAIN_1_1_1))+geom_line()

# 2023: biomet processing is OK. See 2022 code to sub biomet data in flux files with biomet input to EddyPro
# 2-24: one rain event is missing from Ameriflux,
# but final posting includes merge with biomet 2 so it oesn't matter much at this stage
# load biomet data just to check

biomet2024.names <- colnames(fread("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Bahada/Tower/EddyCovariance_ts/EddyPro_Biomet/Biomet_EddyPro_2024.csv",
                                   header=TRUE))


biomet2024 <- fread("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Bahada/Tower/EddyCovariance_ts/EddyPro_Biomet/Biomet_EddyPro_2024.csv",
                    skip=2, header=FALSE, col.names=biomet2024.names, na.strings=c("-9999"))


biomet2024 <- biomet2024[,':='
                         (date_time=ymd_hm(paste(timestamp_1,timestamp_2,timestamp_3,timestamp_4,timestamp_5, sep=" ")))]

ggplot(biomet2024[date_time>as.Date("2023-07-05")], aes(date_time,P_rain_1_1_1))+
  geom_line()


# create a column for filtering CO2 flux
# look at Fc  by month for each year
ggplot(flux_add,
       aes(DOY_START,FC,colour=factor(FC_SSITC_TEST)))+
  geom_point()+
  #ylim(c(-30,30))+
  facet_grid(year~.,scales="free_y")

# graph AGC (is actually signal strength in closed path)
ggplot(flux_add,
       aes(date_time,CUSTOM_AGC_MEAN))+
  geom_point(aes(colour=factor(FC_SSITC_TEST)))+
  geom_line()+
  #ylim(c(25,100))+
  facet_grid(year~.,scales="free_y")

# filter_fc = 1 to remove and = 0 to keep
flux_add[,filter_fc := 0L]
# get rid of QC code >2 and NA
flux_add[FC_SSITC_TEST>1 | is.na(FC_SSITC_TEST), filter_fc := 1L]

# graph AGC with fc filter
ggplot(flux_add[filter_fc!=1],
       aes(date_time,CUSTOM_AGC_MEAN))+
  geom_point(aes(colour=factor(FC_SSITC_TEST)))+
  geom_line()+
  #ylim(c(25,100))+
  facet_grid(year~.,scales="free_y")


# Keep AGC>50 (AGC column is signal strength where 100 is good)
flux_add[(CUSTOM_AGC_MEAN<50),
         filter_fc := 1L]


# fluxes outside 30 umol/m2/s are unrealistic
flux_add[FC<(-30)|FC>30, filter_fc := 1L]


# look at the fluxes by month for each year
ggplot(flux_add[filter_fc !=1,],
                 aes(date_time,FC))+
  geom_point(aes(colour=factor(FC_SSITC_TEST)))+
  geom_line()+
  geom_hline(yintercept=c(-5,5))+
  #ylim(c(-5,5))+
  facet_grid(year~.,scales="free_y")


###### H ######
# look at H  by month for each year
ggplot(flux_add,
       aes(DOY_START,H,colour=factor(H_SSITC_TEST)))+
  geom_point()+
  geom_line()+
  #ylim(c(-30,30))+
  facet_grid(year~.,scales="free_y")

# remove QC >1 and AGC less 48 or more than 65 (see note for FC)
flux_add[,filter_H := 0L]

# get rid of QC code >2 and NA
flux_add[H_SSITC_TEST>1 | is.na(H_SSITC_TEST), filter_h := 1L]

# Keep AGC>50 (AGC column is signal strength where 100 is good)
flux_add[(CUSTOM_AGC_MEAN<50),
         filter_H := 1L]


# remove H < -120 and > 560, these are not typical values.
flux_add[H < (-120) | H > 560, filter_H := 1L]

# look at all filtered H
ggplot(flux_add[filter_H!=1,],
       aes(DOY_START,H))+
  geom_line()+
  #ylim(c(-30,30))+
  facet_grid(year~.,scales="free_y")


##### LE ####
# look at LE  by month for each year
ggplot(flux_add,
       aes(DOY_START,LE,colour=factor(LE_SSITC_TEST)))+
  geom_point()+
  #ylim(c(-30,30))+
  facet_grid(year~.,scales="free_y")

# remove QC >1 and AGC more than 65 and less than 48 (see note with FC)
flux_add[,filter_LE:= 0L]
# get rid of QC code >2 and NA
flux_add[LE_SSITC_TEST>1 | is.na(LE_SSITC_TEST), filter_LE := 1L]

# Keep AGC>50 (AGC column is signal strength where 100 is good)

flux_add[(CUSTOM_AGC_MEAN<50),
         filter_LE := 1L]


# Remove unreasonable values

# remove LE < (-50) this is a generally outlying value
flux_add[LE < (-50), filter_LE := 1L]
# remove LE >1000 this is a generally outlying value
flux_add[LE >1000, filter_LE := 1L]

# look at LE  by month for each year
ggplot(flux_add[filter_LE!=1,],
       aes(DOY_START,LE,colour=factor(LE_SSITC_TEST)))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept=c(-50,200))

# See fluxes during rain events. For Closed path, don're remove the fluxes during rain
ggplot()+
  geom_line(data=flux_add[filter_fc !=1], aes(DOY_START,FC),alpha=0.5)+
  geom_point(data=flux_add[filter_fc !=1 & P_RAIN_1_1_1>0], aes(DOY_START,FC), colour="red",size=0.25)+
  facet_grid(year~.)

# this graph shows additional data removed due to rain
ggplot()+
  geom_line(data=flux_add[filter_fc !=1], aes(DOY_START,FC),alpha=0.5)+
  geom_point(data=flux_add[filter_fc !=1&P_RAIN_1_1_1>0], aes(DOY_START,FC), colour="red",size=0.25)+
  facet_grid(year~.)

# For closed path, don't remove fluxes during rain
# flux_add[P_RAIN_1_1_1>0, ':=' (filter_fc = 1L, filter_LE = 1L, filter_H = 1L)]

# Before the rolling mean, add two weeks of prior data

 flux_add <- rbind(flux_filter_sd[date_time > as.Date("2023-12-15") & date_time < as.Date("2024-01-01") ],
                  flux_add, fill=TRUE)

# make sure the data are ordered:
 flux_add <- flux_add[order(date_time),]

# remove duplicates
 flux_add <- (flux_add[!(duplicated(flux_add, by=c("date_time")))])


# APPLY ROLLING MEANS APPROACH TO FILTER Fc, LE, H
# use rollmeanr from zoo
# 3 days = (24/0.5)*3 = 144
# exclude the level 1 filtered data that removes QC code==2, bad AGC, and values outside +/-30 range
flux_add[filter_fc !=1, ':=' (FC_rollmean3 = rollapply(FC, width=(24/0.5)*3, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                          FC_rollsd3 = rollapply(FC, width=(24/0.5)*3, fill=0, FUN=sd, na.rm=TRUE, align="right"),
                          FC_rollmean5 = rollapply(FC, width=(24/0.5)*5, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                          FC_rollsd5 = rollapply(FC, width=(24/0.5)*5, fill=0, FUN=sd, na.rm=TRUE, align="right"),
                          FC_rollmean7 = rollapply(FC, width=(24/0.5)*7, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                          FC_rollsd7 = rollapply(FC, width=(24/0.5)*7, fill=0, FUN=sd, na.rm=TRUE, align="right"))]

# also calculate a 3 day moving mean and SD for daytime and nighttime
flux_add[filter_fc !=1, ':=' (FC_rollmean3_daynight = rollapply(FC, width=(24/0.5)*3, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                          FC_rollsd3_daynight = rollapply(FC, width=(24/0.5)*3, fill=0, FUN=sd, na.rm=TRUE, align="right")),
     by=NIGHT]


threshold <- 3

# graph the 3 and 5 day SD ribbons around measured flux_add
ggplot(flux_add[filter_fc!=1,])+
  geom_line(aes(yday(date_time), FC))+
  #geom_line(aes(DOY_START, FC_rollmean), colour="green")+
  geom_ribbon(aes(x=yday(date_time), ymin=FC_rollmean3-threshold*FC_rollsd3, ymax=FC_rollmean3+threshold*FC_rollsd3), alpha=0.5)+
  geom_ribbon(aes(x=yday(date_time), ymin=FC_rollmean7-threshold*FC_rollsd7, ymax=FC_rollmean7+threshold*FC_rollsd7), colour="blue",alpha=0.3)+
  
  facet_grid(year(date_time)~.)

# graph the 3 day SD ribbons around measured flux_add by day/night
ggplot(flux_add[filter_fc!=1,])+
  geom_line(aes(DOY_START, FC))+
  #geom_line(aes(DOY_START, FC_rollmean), colour="green")+
  geom_ribbon(aes(x=DOY_START, ymin=FC_rollmean3_daynight-threshold*FC_rollsd3_daynight,
                  ymax=FC_rollmean3_daynight+threshold*FC_rollsd3_daynight, fill=factor(NIGHT)), alpha=0.5)+
  facet_grid(NIGHT~.)

# mark any Fc>3*FCrollsd3 (3 day moving SD) for removal
# For 2023 ,the first 3 days look OK so keep these
flux_add[,filter_fc_roll := 0L]
flux_add[filter_fc==1, filter_fc_roll := 1L]

flux_add[FC>FC_rollmean3+threshold*FC_rollsd3|FC<FC_rollmean3-threshold*FC_rollsd3, filter_fc_roll := 2L]

# by Day/Night mark any Fc>3*FCrollsd3 (3 day moving SD) for removal
flux_add[,filter_fc_roll_daynight := 0L]
flux_add[filter_fc==1, filter_fc_roll_daynight := 1L]
flux_add[FC>FC_rollmean3_daynight+threshold*FC_rollsd3_daynight|
       FC<FC_rollmean3_daynight-threshold*FC_rollsd3_daynight, filter_fc_roll_daynight := 2L]

# view the marked fluxes in ~25 day chunks
ggplot(flux_add[filter_fc_roll!=1&DOY_START>=1&DOY_START<=180,], aes(DOY_START, FC))+
  geom_line()+
  geom_point(aes(colour=factor(filter_fc_roll)), size=0.5)

# view the marked fluxes in ~25 day chunks for day/night
ggplot(flux_add[filter_fc_roll_daynight!=1&DOY_START>=1&DOY_START<=180,], aes(DOY_START, FC))+
  geom_line()+
  geom_point(aes(colour=factor(filter_fc_roll_daynight)), size=0.5)

# graph with the fluxes from the 3SD 3day day/night filter removed
ggplot(flux_add[filter_fc_roll_daynight==0,], aes(DOY_START, FC))+
  geom_line()

# SD filter for LE
# COMPARE ROLLING MEANS APPROACH TO FILTER Fc, LE, H
# use rollmeanr from zoo
# 3 days = (24/0.5)*3 = 144
# exclude the level 1 filtered data that removes QC code==2, bad AGC, and values outside +/-30 range
flux_add[filter_LE !=1, ':=' (LE_rollmean3 = rollapply(LE, width=(24/0.5)*3, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                          LE_rollsd3 = rollapply(LE, width=(24/0.5)*3, fill=0, FUN=sd, na.rm=TRUE, align="right"),
                          LE_rollmean5 = rollapply(LE, width=(24/0.5)*5, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                          LE_rollsd5 = rollapply(LE, width=(24/0.5)*5, fill=0, FUN=sd, na.rm=TRUE, align="right"),
                          LE_rollmean7 = rollapply(LE, width=(24/0.5)*7, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                          LE_rollsd7 = rollapply(LE, width=(24/0.5)*7, fill=0, FUN=sd, na.rm=TRUE, align="right"))]

# also calculate a 3 day moving mean and SD for daytime and nighttime
flux_add[filter_LE !=1, ':=' (LE_rollmean3_daynight = rollapply(LE, width=(24/0.5)*3, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                          LE_rollsd3_daynight = rollapply(LE, width=(24/0.5)*3, fill=0, FUN=sd, na.rm=TRUE, align="right")),
     by=NIGHT]


# threshold <- 3

# graph the 3 and 5 day SD ribbons around measured flux_add
ggplot(flux_add[filter_LE!=1,])+
  geom_line(aes(DOY_START, LE))+
  #geom_line(aes(DOY_START, LE_rollmean), colour="green")+
  geom_ribbon(aes(x=DOY_START, ymin=LE_rollmean3-threshold*LE_rollsd3, ymax=LE_rollmean3+threshold*LE_rollsd3), alpha=0.5)+
  geom_ribbon(aes(x=DOY_START, ymin=LE_rollmean7-threshold*LE_rollsd7, ymax=LE_rollmean7+threshold*LE_rollsd7), colour="blue",alpha=0.3)

# graph the 3 day SD ribbons around measured flux_add by day/night
ggplot(flux_add[filter_LE!=1,])+
  geom_line(aes(DOY_START, LE))+
  #geom_line(aes(DOY_START, LE_rollmean), colour="green")+
  geom_ribbon(aes(x=DOY_START, ymin=LE_rollmean3_daynight-threshold*LE_rollsd3_daynight,
                  ymax=LE_rollmean3_daynight+threshold*LE_rollsd3_daynight, fill=factor(NIGHT)), alpha=0.5)+
  facet_grid(NIGHT~.)

# mark any LE>3*LErollsd3 (3 day moving SD) for removal
flux_add[,filter_le_roll := 0L]
flux_add[filter_LE==1, filter_le_roll := 1L]
flux_add[LE>LE_rollmean3+threshold*LE_rollsd3|LE<LE_rollmean3-threshold*LE_rollsd3, filter_le_roll := 2L]

# by Day/Night mark any LE>3*LErollsd3 (3 day moving SD) for removal
flux_add[,filter_le_roll_daynight := 0L]
flux_add[filter_LE==1, filter_le_roll_daynight := 1L]
flux_add[LE>LE_rollmean3_daynight+threshold*LE_rollsd3_daynight|
       LE<LE_rollmean3_daynight-threshold*LE_rollsd3_daynight, filter_le_roll_daynight := 2L]

# view the marked flux_addes in ~25 day chunks
ggplot(flux_add[filter_le_roll!=1&DOY_START>=1&DOY_START<=180,], aes(DOY_START, LE, colour=factor(filter_le_roll)))+
  geom_point()

# view the marked flux_addes in ~25 day chunks for day/night
ggplot(flux_add[filter_le_roll_daynight!=1&DOY_START>=1&DOY_START<=180,], aes(DOY_START, LE))+
  geom_line()+
  geom_point(aes(colour=factor(filter_le_roll_daynight)), size=0.5)

# graph with the flux_addes from the 3SD 3day day/night filter removed
ggplot(flux_add[filter_le_roll_daynight==0,], aes(DOY_START, LE))+
  geom_line()

# SD filter for H
# COMPARE ROLLING MEANS APPROACH TO FILTER Fc, LE, H
# use rollmeanr from zoo
# 3 days = (24/0.5)*3 = 144
# exclude the level 1 filtered data that removes QC code==2, bad AGC, and values outside +/-30 range
flux_add[filter_H !=1, ':=' (H_rollmean3 = rollapply(H, width=(24/0.5)*3, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                         H_rollsd3 = rollapply(H, width=(24/0.5)*3, fill=0, FUN=sd, na.rm=TRUE, align="right"),
                         H_rollmean5 = rollapply(H, width=(24/0.5)*5, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                         H_rollsd5 = rollapply(H, width=(24/0.5)*5, fill=0, FUN=sd, na.rm=TRUE, align="right"),
                         H_rollmean7 = rollapply(H, width=(24/0.5)*7, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                         H_rollsd7 = rollapply(H, width=(24/0.5)*7, fill=0, FUN=sd, na.rm=TRUE, align="right"))]

# also calculate a 3 day moving mean and SD for daytime and nighttime
flux_add[filter_H !=1, ':=' (H_rollmean3_daynight = rollapply(H, width=(24/0.5)*3, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                         H_rollsd3_daynight = rollapply(H, width=(24/0.5)*3, fill=0, FUN=sd, na.rm=TRUE, align="right")),
     by=NIGHT]


# threshold <- 3

# graph the 3 and 5 day SD ribbons around measured flux_add
ggplot(flux_add[filter_H!=1,])+
  geom_line(aes(DOY_START, H))+
  #geom_line(aes(DOY_START, H_rollmean), colour="green")+
  geom_ribbon(aes(x=DOY_START, ymin=H_rollmean3-threshold*H_rollsd3, ymax=H_rollmean3+threshold*H_rollsd3), alpha=0.5)+
  geom_ribbon(aes(x=DOY_START, ymin=H_rollmean7-threshold*H_rollsd7, ymax=H_rollmean7+threshold*H_rollsd7), colour="blue",alpha=0.3)

# graph the 3 day SD ribbons around measured flux_add by day/night
ggplot(flux_add[filter_H!=1,])+
  geom_line(aes(DOY_START, H))+
  #geom_line(aes(DOY_START, H_rollmean), colour="green")+
  geom_ribbon(aes(x=DOY_START, ymin=H_rollmean3_daynight-threshold*H_rollsd3_daynight,
                  ymax=H_rollmean3_daynight+threshold*H_rollsd3_daynight, fill=factor(NIGHT)), alpha=0.5)+
  facet_grid(NIGHT~.)


# mark any H>3*Hrollsd3 (3 day moving SD) for removal
flux_add[,filter_h_roll := 0L]
flux_add[filter_H==1, filter_h_roll := 1L]
flux_add[H>H_rollmean3+threshold*H_rollsd3|H<H_rollmean3-threshold*H_rollsd3, filter_h_roll := 2L]


# by Day/Night mark any H>3*Hrollsd3 (3 day moving SD) for removal
flux_add[,filter_h_roll_daynight := 0L]
flux_add[filter_H==1, filter_h_roll_daynight := 1L]
flux_add[H>H_rollmean3_daynight+threshold*H_rollsd3_daynight|
       H<H_rollmean3_daynight-threshold*H_rollsd3_daynight, filter_h_roll_daynight := 2L]


# view the marked flux_addes in ~25 day chunks
ggplot(flux_add[filter_h_roll!=1&DOY_START>=1&DOY_START<=180,], aes(DOY_START, H))+
  geom_line()+
  geom_point(aes(colour=factor(filter_h_roll)), size=0.5)

# view the marked flux_addes in ~25 day chunks for day/night
ggplot(flux_add[filter_h_roll_daynight!=1&DOY_START>=1&DOY_START<=180,], aes(DOY_START, H))+
  geom_line()+
  geom_point(aes(colour=factor(filter_h_roll_daynight)), size=0.5)

# graph with the fluxes from the 3SD 3day day/night filter removed
ggplot(flux_add[filter_h_roll_daynight==0,], aes(DOY_START, H))+
  geom_line()


#########
########### Filtering DONE ###############
########### 

# Prior to saving, filter data and remove unnecessary columns
# c("date_time_orig","TIMESTAMP_START","TIMESTAMP_END","DOY_START","DOY_END","time_diff","SW_IN_POT_AF") 
# date_time_orig, time_diff and SW_IN_POT_AF probably won't be in files after 2019 because they were used for the timestamp corrections needed prior to 2019
flux_add_filter_sd <- copy(flux_add[date_time>=as.Date("2024-01-01"),
                                    !(c("date","month","year","DOY_START","DOY_END")),with=FALSE])
flux_add_filter_sd[filter_fc_roll_daynight!=0, FC := NA]
flux_add_filter_sd[filter_h_roll_daynight!=0, H := NA]
flux_add_filter_sd[filter_le_roll_daynight!=0, LE := NA]

# check the time period is right: 
ggplot(flux_add_filter_sd,aes(date_time, FC))+geom_point()
summary(flux_add_filter_sd$date_time)

# append new data to previous
 flux_filter_sd_all <- rbind(flux_filter_sd[date_time<as.Date("2024-01-01"),], flux_add_filter_sd,fill=TRUE)

# remove potential date duplicates
 flux_filter_sd_all <- (flux_filter_sd_all[!(duplicated(flux_filter_sd_all, by=c("date_time")))])

# count and plot number of records per day, should not be >48
 rec.day <- flux_filter_sd_all[!is.na(FC),.(records = length(FC)), by=as.Date(date_time)]

 ggplot(rec.day, aes(as.Date, records))+
   geom_point()+
   geom_hline(yintercept=48, colour="green")
# 
# # graph all
 ggplot(flux_filter_sd_all,aes(yday(date_time), FC))+
   geom_line(size=0.6)+
   facet_grid(year(date_time)~.)

# include TIMESTAMP_START and TIMESTAMP_END 
# save 2021 and 2022 data with biomet data from input biomet2021/2022, not EddyPro output
# save to server
setwd("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Bahada/Tower/ts_data_2/2024/EddyPro_Out/ClosedPath")

#write.table(flux_add_filter_sd,
#            file="JER_flux_2022_EddyPro_Output_filtered_SD_JanSep.csv",sep=",", dec=".",
#            row.names=FALSE)

write.table(flux_add_filter_sd,
            file="JER_flux_2024_EddyPro_Output_filtered_SD_JanMay_Closed.csv",sep=",", dec=".",
            row.names=FALSE)


