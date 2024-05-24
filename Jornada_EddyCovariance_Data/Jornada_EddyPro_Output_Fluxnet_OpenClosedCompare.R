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

# read the data in fluxnet format 
# open path
flux_open <- fread("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Bahada/Tower/EddyCovariance_ClosedPath/EddyPro_Out/20231013_OpenPathCompare/eddypro_JER_2023_0811_1010_OpenPathCompare_fluxnet_2023-10-13T154144_adv.csv",
              sep=",", header=TRUE, na.strings=c("-9999"),fill=TRUE)

# remove duplicate data
flux_open <- (flux_open[!(duplicated(flux_open, by=c("TIMESTAMP_START")))])

# make sure the data are ordered:
flux_open <- flux_open[order(TIMESTAMP_START),]

# format date
flux_open[,':=' (date_time = parse_date_time(TIMESTAMP_END,"YmdHM",tz="UTC"))][
  ,':='(date=as.Date.POSIXct(date_time),month=month(date_time),year=year(date_time))]

# closed path
flux_closed <- fread("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Bahada/Tower/EddyCovariance_ClosedPath/EddyPro_Out/20231018_ClosedPathCompare2_umolmol/eddypro_JER_2023_0811_1010_ClosedPathCompare2_fluxnet_2023-10-18T121739_adv.csv",
                   sep=",", header=TRUE, na.strings=c("-9999"),fill=TRUE)

# remove duplicate data
flux_closed <- (flux_closed[!(duplicated(flux_closed, by=c("TIMESTAMP_START")))])

# make sure the data are ordered:
flux_closed <- flux_closed[order(TIMESTAMP_START),]

# format date
flux_closed[,':=' (date_time = parse_date_time(TIMESTAMP_END,"YmdHM",tz="UTC"))][
  ,':='(date=as.Date.POSIXct(date_time),month=month(date_time),year=year(date_time))]

# column numbers differ, find difference
c(setdiff(colnames(flux_open), colnames(flux_closed)), setdiff(colnames(flux_closed), colnames(flux_open)))

# change column names and merge
setnames(flux_open, 5:477, paste0(names(flux_open)[5:477], '_open'))
setnames(flux_closed, 5:474, paste0(names(flux_closed)[5:474], '_closed'))

# merge open and closed
keycols = c("TIMESTAMP_START","TIMESTAMP_END", "DOY_START", "DOY_END", "date_time","date","year","month")

setkeyv(flux_open, keycols)
setkeyv(flux_closed, keycols)

flux <- flux_open[flux_closed,]  

# make some plots
# graph some met variables
ggplot(flux[!(FILENAME_HF_open %in% ("not_enough_data")),],
       aes(date_time,H2O_open))+
  geom_line()+
  ylim(c(0,40))

ggplot(flux[!(FILENAME_HF_closed %in% ("not_enough_data")),],
       aes(date_time,H2O_closed))+
  geom_line()+
  ylim(c(0,40))


ggplot(flux[!(FILENAME_HF_open %in% ("not_enough_data")),],
       aes(date_time,FC_open))+
  geom_line()

ggplot(flux[!(FILENAME_HF_closed %in% ("not_enough_data")),],
       aes(date_time,FC_closed))+
  geom_line()



# create a column for filtering CO2 flux
# filter_fc = 1 to remove and = 0 to keep
flux[,filter_fc_open := 0L]
# get rid of QC code >2 and low signal strength (AGC bounces between 50-56)
flux[FC_SSITC_TEST_open>1 | CUSTOM_AGC_MEAN_open>56, filter_fc_open := 1L]
# fluxes outside 30 umol/m2/s are unrealistic
flux[FC_open<(-30)|FC_open>30, filter_fc_open := 1L]


# filter_fc = 1 to remove and = 0 to keep
flux[,filter_fc_closed := 0L]
# get rid of QC code >2 and low signal strength
flux[FC_SSITC_TEST_closed>1 , filter_fc_closed := 1L]
# fluxes outside 30 umol/m2/s are unrealistic
flux[FC_closed<(-30)|FC_closed>30, filter_fc_closed := 1L]


# look at the fluxes by month for each year
ggplot(flux[filter_fc_open !=1,],
                 aes(date_time,FC_open))+
  geom_point(aes(colour=factor(FC_SSITC_TEST_open)))+
  geom_line()+
  geom_hline(yintercept=c(-5,5))

ggplot(flux[filter_fc_closed !=1,],
       aes(date_time,FC_closed))+
  geom_point(aes(colour=factor(FC_SSITC_TEST_closed)))+
  geom_line()+
  geom_hline(yintercept=c(-5,5))

# graph FC open vs closed
ggplot(flux[filter_fc_closed !=1 & filter_fc_open !=1,],
       aes(FC_closed,FC_open))+
  geom_point()+
  geom_abline(intercept = 0, slope=1)

# look at covariance


###### H ######
# look at H  by month for each year
ggplot(flux_add,
       aes(DOY_START,H,colour=factor(H_SSITC_TEST)))+
  geom_point()+
  geom_line()

# remove QC >1 and AGC > 50
flux_add[,filter_H := 0L]
# get rid of QC code >2 and low signal strength
flux_add[H_SSITC_TEST>1 | CUSTOM_AGC_MEAN>50, filter_H := 1L]
# remove H < -120 and > 560, these are not typical values.
flux_add[H < (-120) | H > 560, filter_H := 1L]


# look at all filtered H
ggplot(flux_add[filter_H!=1,],
       aes(DOY_START,H,colour=factor(H_SSITC_TEST)))+
  geom_point()+
  geom_line()


##### LE ####
# look at LE  by month for each year
ggplot(flux_add,
       aes(DOY_START,LE,colour=factor(LE_SSITC_TEST)))+
  geom_point()

# remove QC >1 and AGC > 50
flux_add[,filter_LE:= 0L]
# get rid of QC code >2 and low signal strength
flux_add[LE_SSITC_TEST>1 | CUSTOM_AGC_MEAN>50, filter_LE := 1L]

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

# Before the SD filter remove all fluxes that occur at the moment of a rain event
# first look at all fluxes with rain events marked
# FC
f.fc <- ggplot()+
  geom_line(data=flux_add[filter_fc !=1], aes(DOY_START,FC),alpha=0.5)+
  geom_point(data=flux_add[filter_fc !=1&P_RAIN_1_1_1>0], aes(DOY_START,FC), colour="red",size=0.25)

#LE
f.le <- ggplot()+
  geom_line(data=flux_add[filter_LE !=1], aes(DOY_START,LE),alpha=0.5)+
  geom_point(data=flux_add[filter_LE !=1&P_RAIN_1_1_1>0], aes(DOY_START,LE), colour="red",size=0.25)

#H
f.h <- ggplot()+
  geom_line(data=flux_add[filter_H !=1], aes(DOY_START,H),alpha=0.5)+
  geom_point(data=flux_add[filter_H !=1&P_RAIN_1_1_1>0], aes(DOY_START,H), colour="red",size=0.25)

grid.arrange(f.fc, f.le, f.h)

# remove FC, LE, H DURING rain
flux_add[P_RAIN_1_1_1>0, ':=' (filter_fc = 1L, filter_LE = 1L, filter_H = 1L)]

# Before the rolling mean, add two weeks of prior data
# flux_filter_sd has TIMESTAMP_START_correct and TIMESTAMP_END_correct
# change to TIMESTAMP_START and TIMESTAMP_END to match newly processed data in flux_add
setnames(flux_filter_sd, c("TIMESTAMP_START_correct", "TIMESTAMP_END_correct"),
         c("TIMESTAMP_START", "TIMESTAMP_END"))

# and set as integer64
flux_filter_sd[,':=' (TIMESTAMP_START = bit64::as.integer64(TIMESTAMP_START),
                      TIMESTAMP_END = bit64::as.integer64(TIMESTAMP_END))]

flux_add <- rbind(flux_filter_sd[date_time > as.Date("2019-12-15")],
                  flux_add, fill=TRUE)

# make sure the data are ordered:
flux_add <- flux_add[order(date_time),]

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

# graph the 3 (grey) and 5 (blue) day SD ribbons around measured flux_add
ggplot(flux_add[filter_fc!=1,])+
  geom_line(aes(DOY_START, FC))+
  #geom_line(aes(DOY_START, FC_rollmean), colour="green")+
  geom_ribbon(aes(x=DOY_START, ymin=FC_rollmean3-threshold*FC_rollsd3, ymax=FC_rollmean3+threshold*FC_rollsd3), alpha=0.5)+
  geom_ribbon(aes(x=DOY_START, ymin=FC_rollmean5-threshold*FC_rollsd5, ymax=FC_rollmean7+threshold*FC_rollsd5), colour="blue",alpha=0.3)

# graph the 3 day SD ribbons around measured flux_add by day/night
ggplot(flux_add[filter_fc!=1,])+
  geom_line(aes(DOY_START, FC))+
  #geom_line(aes(DOY_START, FC_rollmean), colour="green")+
  geom_ribbon(aes(x=DOY_START, ymin=FC_rollmean3_daynight-threshold*FC_rollsd3_daynight,
                  ymax=FC_rollmean3_daynight+threshold*FC_rollsd3_daynight, fill=factor(NIGHT)), alpha=0.5)+
  facet_grid(NIGHT~.)

# mark any Fc>3*FCrollsd3 (3 day moving SD) for removal
# For Jan 2020 added 2 weeks of data prior from 2019 so the running mean can carry over. 
flux_add[,filter_fc_roll := 0L]
flux_add[filter_fc==1, filter_fc_roll := 1L]

flux_add[FC>FC_rollmean3+threshold*FC_rollsd3|FC<FC_rollmean3-threshold*FC_rollsd3, filter_fc_roll := 2L]


# by Day/Night mark any Fc>3*FCrollsd3 (3 day moving SD) for removal
flux_add[,filter_fc_roll_daynight := 0L]
flux_add[filter_fc==1, filter_fc_roll_daynight := 1L]
flux_add[FC>FC_rollmean3_daynight+threshold*FC_rollsd3_daynight|
       FC<FC_rollmean3_daynight-threshold*FC_rollsd3_daynight, filter_fc_roll_daynight := 2L]

# view the marked fluxes in ~25 day chunks
ggplot(flux_add[filter_fc_roll!=1&DOY_START>=300&DOY_START<=365,], aes(DOY_START, FC, colour=factor(filter_fc_roll)))+
  geom_point()+
  facet_grid(year_orig~.)

# view the marked fluxes in ~25 day chunks for day/night
ggplot(flux_add[filter_fc_roll_daynight!=1&DOY_START>=1&DOY_START<=31,], aes(DOY_START, FC, colour=factor(filter_fc_roll_daynight)))+
  geom_point()+
  facet_grid(year_orig~.)

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
ggplot(flux_add[filter_le_roll!=1&DOY_START>=1&DOY_START<=50,], aes(DOY_START, LE, colour=factor(filter_le_roll)))+
  geom_point()+
  facet_grid(year_orig~.)

# view the marked flux_addes in ~25 day chunks for day/night
ggplot(flux_add[filter_le_roll_daynight!=1&DOY_START>=1&DOY_START<=100,], aes(DOY_START, LE, colour=factor(filter_le_roll_daynight)))+
  geom_point()+
  facet_grid(year_orig~.)

# graph with the flux_addes from the 3SD 3day day/night filter removed
ggplot(flux_add[filter_le_roll_daynight==0,], aes(DOY_START, LE))+
  geom_line()

# graph fluxes with the 3SD 3day day/night filter removed and with my manual filter
# HERE THIS DOESN'T REALLY APPLY, I HAVE DONE LESS MANUAL FILTERING AND AM RELYING ON THE RUNNING MEAN SMOOTH
# this comparison was for previous years (2010-2019). It's nice to have the code handy, if needed
ggplot()+
  geom_line(data=flux_add[filter_LE==0], aes(DOY_START, LE, colour="manual"), colour="blue")+
  geom_line(data=flux_add[filter_le_roll_daynight==0], aes(DOY_START, LE, colour="SD day/night"), colour="green")


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
ggplot(flux_add[filter_h_roll!=1&DOY_START>=1&DOY_START<=50,], aes(DOY_START, H, colour=factor(filter_h_roll)))+
  geom_point()+
  facet_grid(year_orig~.)

# view the marked flux_addes in ~25 day chunks for day/night
ggplot(flux_add[filter_h_roll_daynight!=1&DOY_START>=1&DOY_START<=100,], aes(DOY_START, H, colour=factor(filter_h_roll_daynight)))+
  geom_point()+
  facet_grid(year~.)

# graph with the fluxes from the 3SD 3day day/night filter removed
ggplot(flux_add[filter_h_roll_daynight==0,], aes(DOY_START, H))+
  geom_line()


#########
########### Filtering DONE ###############
########### 

# Prior to saving, filter data and remove unnecessary columns
# c("date_time_orig","TIMESTAMP_START","TIMESTAMP_END","DOY_START","DOY_END","time_diff","SW_IN_POT_AF") 
# date_time_orig, time_diff and SW_IN_POT_AF probably won't be in files after 2019 because they were used for the timestamp corrections needed prior to 2019
flux_add_filter_sd <- copy(flux_add[date_time>=as.Date("2020-01-01"),
                                    !(c("date","month","year","DOY_START","DOY_END")),with=FALSE])
flux_add_filter_sd[filter_fc_roll_daynight!=0, FC := NA]
flux_add_filter_sd[filter_h_roll_daynight!=0, H := NA]
flux_add_filter_sd[filter_le_roll_daynight!=0, LE := NA]

# check the time period is right: 
ggplot(flux_add_filter_sd,aes(date_time, FC))+geom_point()
summary(flux_add_filter_sd$date_time)

# append new data to previous
flux_filter_sd_all <- rbind(flux_filter_sd, flux_add_filter_sd,fill=TRUE)
 
# make sure there are no duplicates
flux_filter_sd_all <- (flux_filter_sd_all[!(duplicated(flux_filter_sd_all, by=c("date_time")))])


# 20211229 (redo on 20220103 with TIMESTAMP_START and TIMESTAMP_END included)
# save 2020 data with biomet data from input biomet2020, not EddyPro output
# save to server
setwd("/Volumes/SEL_Data_Archive/Research Data/Desert/Jornada/Bahada/Tower/EddyCovariance_ts/2020/EddyPro_Out/")
 
write.table(flux_add_filter_sd,
 file="JER_flux_2020_EddyPro_Output_filtered_SD.csv",sep=",", dec=".",
 row.names=FALSE)

 # save filtered data with SD filter for all years 2010-current
 setwd("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_EddyPro_filtered")
 
 # 20220913 save with entire year of 2020
 # 20211229 (redo on 20220103 with TIMESTAMP_START and TIMESTAMP_END included)
 save(flux_filter_sd_all,
 file="JER_flux_2010_EddyPro_Output_filtered_SD_20220913.Rdata")
 
 
# 20201114
# save(flux_filter_sd_all,
# file="JER_flux_2010_20200131_EddyPro_Output_filtered_SD_TIMEcorr_20201114.Rdata")

# save only 2020 data for ReddyProc 
# save(flux_add_filter_sd,
# file="JER_flux_20200131_EddyPro_Output_filtered_SD_TIMEcorr_20201114.Rdata")

# save only the 2020 flux data as Rdata for a demo file
# save(flux_add,
# file="/Users/memauritz/Desktop/R/R_programs/Tweedie/Jornada/Jornada_Tweedie/Jornada_EddyCovariance_Data/demofileflux_add.Rdata")

# Save data for Hayden with similar columns as previous data supplied. (as in: Jornada_ReddyProc_2010_2019_OnlineToolOut)
# This is not gap-filled because it's only1 month of data

# save data for Hayden. Could not run through ReddyProc because it's only 1 month. 
flux.hayden <- copy(flux_add_filter_sd)

flux.hayden[,':=' (Year = year(date_time),
                   DoY = yday(date_time),
                   hours = hour(date_time),
                   mins = minute(date_time),
                   LE_orig = LE,
                   H_orig = H, 
                   Rg_orig = SW_IN_1_1_1,
                   Tair_orig = TA_1_1_1,
                   rH_orig = RH_1_1_1,
                   VPD = VPD_EP)]

# Create an Hour variable that matches ReddyProc format
flux.hayden[mins==0, Hour := hours+0.0]
flux.hayden[mins==30, Hour := hours+0.5]

# save the data
write.table(flux.hayden[,.(Year,DoY,Hour,LE_orig,H_orig,Rg_orig,Tair_orig,rH_orig,VPD,USTAR)],
            file="~/Desktop/TweedieLab/People/Hayden/FluxData_USJo1_LE_H_2020.csv", sep=",", dec=".",
            row.names=FALSE)


