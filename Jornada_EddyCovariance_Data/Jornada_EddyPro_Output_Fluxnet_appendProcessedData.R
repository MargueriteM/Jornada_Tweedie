# Rscript to process a smaller section of EC data and then append it to already-processed data. 
# This code follows steps laid out in Jornada_EddyPro_Output_Fluxnet_2010_2019_20200212.R
# for examples use the demofileFlux2020.Rdata and skip steps with flux_filter_sd

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

# load the previously processed data: 
# 20200427: corrected timestamps!
 load("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_EddyPro_filtered/JER_flux_2010_2019_EddyPro_Output_filtered_SD_TIMEcorr_20200427.Rdata")


# read the data in fluxnet format that you want to append to other data
# or read demofile instead
flux2020 <- fread("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_2020/eddypro_JER_2020_Jan_fluxnet_2020-11-13T180934_adv.csv",
                  sep=",", header=TRUE, na.strings=c("-9999"))

# remove duplicate data
flux2020 <- (flux2020[!(duplicated(flux2020, by=c("TIMESTAMP_START")))])

# make sure the data are ordered:
flux2020 <- flux2020[order(TIMESTAMP_START),]

# format date
flux2020[,':=' (date_time = parse_date_time(TIMESTAMP_END,"YmdHM",tz="UTC"))][
  ,':='(date=as.Date.POSIXct(date_time),month=month(date_time),year=year(date_time))]

# make some plots
# graph some met variables
ggplot(flux2020,aes(date_time,P_RAIN_1_1_1))+geom_line()

# create a column for filtering CO2 flux
# filter_fc = 1 to remove and = 0 to keep
flux2020[,filter_fc := 0L]
# get rid of QC code >2 and low signal strength
flux2020[FC_SSITC_TEST>1 | CUSTOM_AGC_MEAN>50, filter_fc := 1L]
# fluxes outside 30 umol/m2/s are unrealistic
flux2020[FC<(-30)|FC>30, filter_fc := 1L]


# look at the fluxes by month for each year
ggplot(flux2020[filter_fc !=1&month_orig==1,],
                 aes(DOY_START,FC))+
  geom_point(aes(colour=factor(FC_SSITC_TEST)))+
  geom_line()+
  geom_hline(yintercept=c(-5,5))+
  #ylim(c(-5,5))+
  facet_grid(year_orig~.,scales="free_y")


###### H ######
# look at H  by month for each year
ggplot(flux2020[month_orig==1&DOY_START<31,],
       aes(DOY_START,H,colour=factor(H_SSITC_TEST)))+
  geom_point()+
  #ylim(c(-30,30))+
  facet_grid(year_orig~.,scales="free_y")

# remove QC >1 and AGC > 50
flux2020[,filter_H := 0L]
# get rid of QC code >2 and low signal strength
flux2020[H_SSITC_TEST>1 | CUSTOM_AGC_MEAN>50, filter_H := 1L]
# remove H < -120 and > 560, these are not typical values.
flux2020[H < (-120) | H > 560, filter_H := 1L]
# in 2015 March there's one H > 400 that's an outlier
flux2020[year==2015 & month==3 & H>400, filter_H := 1L]
# in 2018 Jan there's one H>400 that's an outlier
flux2020[year==2018 & month==1 & H>400, filter_H := 1L]


# look at all filtered H
ggplot(flux2020[filter_H!=1&month==1&DOY_START<31,],
       aes(DOY_START,H))+
  geom_line()+
  #ylim(c(-30,30))+
  facet_grid(year~.,scales="free_y")


##### LE ####
# look at LE  by month for each year
ggplot(flux2020[month==1&DOY_START<31,],
       aes(DOY_START,LE,colour=factor(LE_SSITC_TEST)))+
  geom_point()+
  #ylim(c(-30,30))+
  facet_grid(year~.,scales="free_y")

# remove QC >1 and AGC > 50
flux2020[,filter_LE:= 0L]
# get rid of QC code >2 and low signal strength
flux2020[LE_SSITC_TEST>1 | CUSTOM_AGC_MEAN>50, filter_LE := 1L]

# Remove unreasonable values

# remove LE < (-50) this is a generally outlying value
flux2020[LE < (-50), filter_LE := 1L]
# remove LE >1000 this is a generally outlying value
flux2020[LE >1000, filter_LE := 1L]


# look at LE  by month for each year
ggplot(flux2020[filter_LE!=1&month==1&DOY_START<31,],
       aes(DOY_START,LE,colour=factor(LE_SSITC_TEST)))+
  geom_point()+
  geom_hline(yintercept=c(-50,200))+
  facet_grid(year~.,scales="free_y")

# Before the SD filter remove all fluxes that occur at the moment of a rain event
ggplot()+
  geom_line(data=flux2020[filter_fc !=1], aes(DOY_START,FC),alpha=0.5)+
  geom_point(data=flux2020[filter_fc !=1&P_RAIN_1_1_1>0], aes(DOY_START,FC), colour="red",size=0.25)+
  facet_grid(year~.)

flux2020[P_RAIN_1_1_1>0, ':=' (filter_fc = 1L, filter_LE = 1L, filter_H = 1L)]

# Before the rolling mean, add two weeks of prior data

flux2020 <- rbind(flux_filter_sd[date_time > as.Date("2019-12-15")],
                  flux2020, fill=TRUE)

# make sure the data are ordered:
flux2020 <- flux2020[order(date_time),]

# APPLY ROLLING MEANS APPROACH TO FILTER Fc, LE, H
# use rollmeanr from zoo
# 3 days = (24/0.5)*3 = 144
# exclude the level 1 filtered data that removes QC code==2, bad AGC, and values outside +/-30 range
flux2020[filter_fc !=1, ':=' (FC_rollmean3 = rollapply(FC, width=(24/0.5)*3, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                          FC_rollsd3 = rollapply(FC, width=(24/0.5)*3, fill=0, FUN=sd, na.rm=TRUE, align="right"),
                          FC_rollmean5 = rollapply(FC, width=(24/0.5)*5, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                          FC_rollsd5 = rollapply(FC, width=(24/0.5)*5, fill=0, FUN=sd, na.rm=TRUE, align="right"),
                          FC_rollmean7 = rollapply(FC, width=(24/0.5)*7, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                          FC_rollsd7 = rollapply(FC, width=(24/0.5)*7, fill=0, FUN=sd, na.rm=TRUE, align="right"))]

# also calculate a 3 day moving mean and SD for daytime and nighttime
flux2020[filter_fc !=1, ':=' (FC_rollmean3_daynight = rollapply(FC, width=(24/0.5)*3, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                          FC_rollsd3_daynight = rollapply(FC, width=(24/0.5)*3, fill=0, FUN=sd, na.rm=TRUE, align="right")),
     by=NIGHT]


threshold <- 3

# graph the 3 and 5 day SD ribbons around measured flux2020
ggplot(flux2020[filter_fc!=1,])+
  geom_line(aes(DOY_START, FC))+
  #geom_line(aes(DOY_START, FC_rollmean), colour="green")+
  geom_ribbon(aes(x=DOY_START, ymin=FC_rollmean3-threshold*FC_rollsd3, ymax=FC_rollmean3+threshold*FC_rollsd3), alpha=0.5)+
  geom_ribbon(aes(x=DOY_START, ymin=FC_rollmean7-threshold*FC_rollsd7, ymax=FC_rollmean7+threshold*FC_rollsd7), colour="blue",alpha=0.3)+
  
  facet_grid(year~.)

# graph the 3 day SD ribbons around measured flux2020 by day/night
ggplot(flux2020[filter_fc!=1,])+
  geom_line(aes(DOY_START, FC))+
  #geom_line(aes(DOY_START, FC_rollmean), colour="green")+
  geom_ribbon(aes(x=DOY_START, ymin=FC_rollmean3_daynight-threshold*FC_rollsd3_daynight,
                  ymax=FC_rollmean3_daynight+threshold*FC_rollsd3_daynight, fill=factor(NIGHT)), alpha=0.5)+
  facet_grid(NIGHT~.)

# mark any Fc>3*FCrollsd3 (3 day moving SD) for removal
# For Jan 2020 added 2 weeks of data prior from 2019 so the running mean can carry over. 
flux2020[,filter_fc_roll := 0L]
flux2020[filter_fc==1, filter_fc_roll := 1L]

flux2020[FC>FC_rollmean3+threshold*FC_rollsd3|FC<FC_rollmean3-threshold*FC_rollsd3, filter_fc_roll := 2L]


# by Day/Night mark any Fc>3*FCrollsd3 (3 day moving SD) for removal
flux2020[,filter_fc_roll_daynight := 0L]
flux2020[filter_fc==1, filter_fc_roll_daynight := 1L]
flux2020[FC>FC_rollmean3_daynight+threshold*FC_rollsd3_daynight|
       FC<FC_rollmean3_daynight-threshold*FC_rollsd3_daynight, filter_fc_roll_daynight := 2L]

# view the marked fluxes in ~25 day chunks
ggplot(flux2020[filter_fc_roll!=1&DOY_START>=1&DOY_START<=60,], aes(DOY_START, FC, colour=factor(filter_fc_roll)))+
  geom_point()+
  facet_grid(year_orig~.)

# view the marked fluxes in ~25 day chunks for day/night
ggplot(flux2020[filter_fc_roll_daynight!=1&DOY_START>=1&DOY_START<=31,], aes(DOY_START, FC, colour=factor(filter_fc_roll_daynight)))+
  geom_point()+
  facet_grid(year_orig~.)

# graph with the fluxes from the 3SD 3day day/night filter removed
ggplot(flux2020[filter_fc_roll_daynight==0,], aes(DOY_START, FC))+
  geom_line()+
  facet_grid(year_orig~.)


# SD filter for LE
# COMPARE ROLLING MEANS APPROACH TO FILTER Fc, LE, H
# use rollmeanr from zoo
# 3 days = (24/0.5)*3 = 144
# exclude the level 1 filtered data that removes QC code==2, bad AGC, and values outside +/-30 range
flux2020[filter_LE !=1, ':=' (LE_rollmean3 = rollapply(LE, width=(24/0.5)*3, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                          LE_rollsd3 = rollapply(LE, width=(24/0.5)*3, fill=0, FUN=sd, na.rm=TRUE, align="right"),
                          LE_rollmean5 = rollapply(LE, width=(24/0.5)*5, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                          LE_rollsd5 = rollapply(LE, width=(24/0.5)*5, fill=0, FUN=sd, na.rm=TRUE, align="right"),
                          LE_rollmean7 = rollapply(LE, width=(24/0.5)*7, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                          LE_rollsd7 = rollapply(LE, width=(24/0.5)*7, fill=0, FUN=sd, na.rm=TRUE, align="right"))]

# also calculate a 3 day moving mean and SD for daytime and nighttime
flux2020[filter_LE !=1, ':=' (LE_rollmean3_daynight = rollapply(LE, width=(24/0.5)*3, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                          LE_rollsd3_daynight = rollapply(LE, width=(24/0.5)*3, fill=0, FUN=sd, na.rm=TRUE, align="right")),
     by=NIGHT]


# threshold <- 3

# graph the 3 and 5 day SD ribbons around measured flux2020
ggplot(flux2020[filter_LE!=1,])+
  geom_line(aes(DOY_START, LE))+
  #geom_line(aes(DOY_START, LE_rollmean), colour="green")+
  geom_ribbon(aes(x=DOY_START, ymin=LE_rollmean3-threshold*LE_rollsd3, ymax=LE_rollmean3+threshold*LE_rollsd3), alpha=0.5)+
  geom_ribbon(aes(x=DOY_START, ymin=LE_rollmean7-threshold*LE_rollsd7, ymax=LE_rollmean7+threshold*LE_rollsd7), colour="blue",alpha=0.3)+
  
  facet_grid(year_orig~.)

# graph the 3 day SD ribbons around measured flux2020 by day/night
ggplot(flux2020[filter_LE!=1,])+
  geom_line(aes(DOY_START, LE))+
  #geom_line(aes(DOY_START, LE_rollmean), colour="green")+
  geom_ribbon(aes(x=DOY_START, ymin=LE_rollmean3_daynight-threshold*LE_rollsd3_daynight,
                  ymax=LE_rollmean3_daynight+threshold*LE_rollsd3_daynight, fill=factor(NIGHT)), alpha=0.5)+
  facet_grid(NIGHT~.)

# mark any LE>3*LErollsd3 (3 day moving SD) for removal
flux2020[,filter_le_roll := 0L]
flux2020[filter_LE==1, filter_le_roll := 1L]
flux2020[LE>LE_rollmean3+threshold*LE_rollsd3|LE<LE_rollmean3-threshold*LE_rollsd3, filter_le_roll := 2L]

# by Day/Night mark any LE>3*LErollsd3 (3 day moving SD) for removal
flux2020[,filter_le_roll_daynight := 0L]
flux2020[filter_LE==1, filter_le_roll_daynight := 1L]
flux2020[LE>LE_rollmean3_daynight+threshold*LE_rollsd3_daynight|
       LE<LE_rollmean3_daynight-threshold*LE_rollsd3_daynight, filter_le_roll_daynight := 2L]


# view the marked flux2020es in ~25 day chunks
ggplot(flux2020[filter_le_roll!=1&DOY_START>=1&DOY_START<=50,], aes(DOY_START, LE, colour=factor(filter_le_roll)))+
  geom_point()+
  facet_grid(year_orig~.)

# view the marked flux2020es in ~25 day chunks for day/night
ggplot(flux2020[filter_le_roll_daynight!=1&DOY_START>=1&DOY_START<=100,], aes(DOY_START, LE, colour=factor(filter_le_roll_daynight)))+
  geom_point()+
  facet_grid(year_orig~.)

# graph with the flux2020es from the 3SD 3day day/night filter removed
ggplot(flux2020[filter_le_roll_daynight==0,], aes(DOY_START, LE))+
  geom_line()+
  facet_grid(year_orig~.)

# graph fluxes with the 3SD 3day day/night filter removed and with my manual filter
# HERE THIS DOESN'T REALLY APPLY, I HAVE DONE LESS MANUAL FILTERING AND AM RELYING ON THE RUNNING MEAN SMOOTH
# this comparison was for previous years. It's nice to have the code handy, if needed
ggplot()+
  geom_line(data=flux2020[filter_LE==0], aes(DOY_START, LE, colour="manual"), colour="blue")+
  geom_line(data=flux2020[filter_le_roll_daynight==0], aes(DOY_START, LE, colour="SD day/night"), colour="green")+
  facet_grid(year_orig~.)


# SD filter for H
# COMPARE ROLLING MEANS APPROACH TO FILTER Fc, LE, H
# use rollmeanr from zoo
# 3 days = (24/0.5)*3 = 144
# exclude the level 1 filtered data that removes QC code==2, bad AGC, and values outside +/-30 range
flux2020[filter_H !=1, ':=' (H_rollmean3 = rollapply(H, width=(24/0.5)*3, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                         H_rollsd3 = rollapply(H, width=(24/0.5)*3, fill=0, FUN=sd, na.rm=TRUE, align="right"),
                         H_rollmean5 = rollapply(H, width=(24/0.5)*5, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                         H_rollsd5 = rollapply(H, width=(24/0.5)*5, fill=0, FUN=sd, na.rm=TRUE, align="right"),
                         H_rollmean7 = rollapply(H, width=(24/0.5)*7, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                         H_rollsd7 = rollapply(H, width=(24/0.5)*7, fill=0, FUN=sd, na.rm=TRUE, align="right"))]

# also calculate a 3 day moving mean and SD for daytime and nighttime
flux2020[filter_H !=1, ':=' (H_rollmean3_daynight = rollapply(H, width=(24/0.5)*3, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                         H_rollsd3_daynight = rollapply(H, width=(24/0.5)*3, fill=0, FUN=sd, na.rm=TRUE, align="right")),
     by=NIGHT]


# threshold <- 3

# graph the 3 and 5 day SD ribbons around measured flux2020
ggplot(flux2020[filter_H!=1,])+
  geom_line(aes(DOY_START, H))+
  #geom_line(aes(DOY_START, H_rollmean), colour="green")+
  geom_ribbon(aes(x=DOY_START, ymin=H_rollmean3-threshold*H_rollsd3, ymax=H_rollmean3+threshold*H_rollsd3), alpha=0.5)+
  geom_ribbon(aes(x=DOY_START, ymin=H_rollmean7-threshold*H_rollsd7, ymax=H_rollmean7+threshold*H_rollsd7), colour="blue",alpha=0.3)+
  
  facet_grid(year~.)

# graph the 3 day SD ribbons around measured flux2020 by day/night
ggplot(flux2020[filter_H!=1,])+
  geom_line(aes(DOY_START, H))+
  #geom_line(aes(DOY_START, H_rollmean), colour="green")+
  geom_ribbon(aes(x=DOY_START, ymin=H_rollmean3_daynight-threshold*H_rollsd3_daynight,
                  ymax=H_rollmean3_daynight+threshold*H_rollsd3_daynight, fill=factor(NIGHT)), alpha=0.5)+
  facet_grid(NIGHT~.)

# mark any H>3*Hrollsd3 (3 day moving SD) for removal
flux2020[,filter_h_roll := 0L]
flux2020[filter_H==1, filter_h_roll := 1L]
flux2020[H>H_rollmean3+threshold*H_rollsd3|H<H_rollmean3-threshold*H_rollsd3, filter_h_roll := 2L]

# by Day/Night mark any H>3*Hrollsd3 (3 day moving SD) for removal
flux2020[,filter_h_roll_daynight := 0L]
flux2020[filter_H==1, filter_h_roll_daynight := 1L]
flux2020[H>H_rollmean3_daynight+threshold*H_rollsd3_daynight|
       H<H_rollmean3_daynight-threshold*H_rollsd3_daynight, filter_h_roll_daynight := 2L]


# view the marked flux2020es in ~25 day chunks
ggplot(flux2020[filter_h_roll!=1&DOY_START>=1&DOY_START<=50,], aes(DOY_START, H, colour=factor(filter_h_roll)))+
  geom_point()+
  facet_grid(year_orig~.)

# view the marked flux2020es in ~25 day chunks for day/night
ggplot(flux2020[filter_h_roll_daynight!=1&DOY_START>=1&DOY_START<=100,], aes(DOY_START, H, colour=factor(filter_h_roll_daynight)))+
  geom_point()+
  facet_grid(year~.)

# graph with the fluxes from the 3SD 3day day/night filter removed
ggplot(flux2020[filter_h_roll_daynight==0,], aes(DOY_START, H))+
  geom_line()+
  facet_grid(year~.)


#########
########### Filtering DONE ###############
########### 

# Prior to saving, filter data and remove unnecessary columns
# c("date_time_orig","TIMESTAMP_START","TIMESTAMP_END","DOY_START","DOY_END","time_diff","SW_IN_POT_AF") 
# date_time_orig, time_diff and SW_IN_POT_AF probably won't be in files after 2019 because they were used for the timestamp corrections needed prior to 2019
flux2020_filter_sd <- copy(flux2020[date_time>=as.Date("2020-01-01"),!(c("date","month","year","TIMESTAMP_START","TIMESTAMP_END","DOY_START","DOY_END")),with=FALSE])
flux2020_filter_sd[filter_fc_roll_daynight!=0, FC := NA]
flux2020_filter_sd[filter_h_roll_daynight!=0, H := NA]
flux2020_filter_sd[filter_le_roll_daynight!=0, LE := NA]

# check the time period is right: 
ggplot(flux2020_filter_sd,aes(date_time, FC))+geom_point()
summary(flux2020_filter_sd$date_time)

# append new data to previous
flux_filter_sd_all <- rbind(flux_filter_sd, flux2020_filter_sd)
 
# save filtered data with SD filter
setwd("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_EddyPro_filtered")

# 20201114
# save(flux_filter_sd_all,
# file="JER_flux_2010_20200131_EddyPro_Output_filtered_SD_TIMEcorr_20201114.Rdata")

# save only the 2020 flux data as Rdata for a demo file
# save(flux2020,
# file="/Users/memauritz/Desktop/R/R_programs/Tweedie/Jornada/Jornada_Tweedie/Jornada_EddyCovariance_Data/demofileFlux2020.Rdata")



