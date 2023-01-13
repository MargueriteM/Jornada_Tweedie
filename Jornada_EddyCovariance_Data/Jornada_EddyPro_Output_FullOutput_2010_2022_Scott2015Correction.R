
############################################
#  This code gets data output by Eddy Pro  #
#       and w'Co2 corrected with           #
#        Scott et al 2015 correction.      #
#           written by: M. Mauritz         #
#            12 Jan 2023                   #
############################################

# 20201112: add Jan 2020 data for Hayden
# 20200128: add SD based filters (3 day running mean/SD seperately for day/night and remove Fc, H, LE more than 3SD out of range from running mean)
# 20200205: update with 2010 data processed in batches of files with 14 and 15 data rows (see Jornada_EC_2010_diagnosing.R)
# 20200212: update with full year of 2019 data and simplify filter to only include SD filter steps.
# 20200427: correct tower timestamps

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
# EddyPro 7.0.4 has column name slip problems, using Fluxnet output!

# read the data in full output format after manual wpl correction and adjusted correction
# see RecalculateFluxes.R
# fc_wpl = reproduce WPL correction to check math
# fc_wpl_adjust = adjust w'co2 *0.9 and correct for scf and wpl -> according to Scott et al 2015
# LE_wpl = reproduce WPL correction to check math

load("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_CovarianceCorrect_Scott2015/JER_flux_EddyPro_FullOutput_Scott2015_Correct_20230112.RData")

# visualise reporduced and adjusted CO2 and LE fluxes
# EddyPro corrected CO2 flux (black) and re-calculated CO2 flux (red)
ggplot(flux) +
  geom_point(aes(date, co2_flux), color="black",size=0.3)+
  geom_point(aes(date, fc_wpl),color="red", size=0.1)+
  ylim(-30,10)+
  facet_grid(.~year, scales="free_x")+
  labs(title="James: EddyPro corrected CO2 flux (black) and re-calculated CO2 flux (red)")

# look at offset corrected CO2 flux (sensu Scott et al 2015)
ggplot(flux) +
  geom_point(aes(date, co2_flux), color="black",size=0.3)+
  geom_point(aes(date, fc_wpl_adjust),color="green", size=0.1)+
  ylim(-30,10)+
  facet_grid(.~year, scales="free_x")+
  labs(title="James: EddyPro corrected CO2 flux (black) and 10% adjusted CO2 flux (green)")

# LE
ggplot(flux) +
  geom_point(aes(date, LE), color="black",size=0.3)+
  geom_point(aes(date, LE_wpl),color="blue", size=0.1)+
  facet_grid(.~year, scales="free_x")+
  labs(title="James: EddyPro corrected LE flux (black) and re-calculated LE flux (red)")


# make some plots
# graph precipitation
ggplot(flux,aes(date_time_orig,P_RAIN_1_1_1))+geom_line()
# graph soil moisture
ggplot(flux,aes(date_time_orig,SWC_1_1_1))+geom_line()

# graph air temp
ggplot(flux,aes(date_time_orig,TA_1_1_1))+geom_line()

# graph PAR
ggplot(flux,aes(date_time_orig,PPFD_IN_1_1_1))+geom_line()

# QA/QC process: 
# flag points to remove in 'flag' column
# remove qc flag <2

# qc flags, keep 0 and 1
# https://www.licor.com/env/support/EddyPro/topics/flux-quality-flags.html

# look at time series and if a data point is way out & qc code = 1, remove
# look at time series and data with PAR, TA, Rain and see if points are outliers in 
# time-series and in relationships, remove
# There are some additional points that could be removed but it starts to feel very selective
# and like grooming the data too much. 
# I think it's better to leave these points in and remove them depending on data purpose
# and evaluate whether they have a strong influence or not.
# for budgets and general patterns the data that are here will not make any difference.

# U* filter: 
# u* filter (Aline eliminated data with friction velocity threshold < 0.1)
# will use EddyRe for more systematic approach and to estimate uncertainty related to U* threshold

# AGC, signal strength = INST_LI7500_AGC_OR_RSSI or CUSTOM_AGC_MEAN (they have flipped signs)
ggplot(flux[FC_SSITC_TEST<2,], aes(date_time_orig,FC,colour=(CUSTOM_AGC_MEAN)))+
  geom_point()+
  ylim(c(-10,30))

# create a column for filtering CO2 flux
# filter_fc = 1 to remove and = 0 to keep
flux[,filter_fc := 0L]
# get rid of QC code >2 and low signal strength
flux[FC_SSITC_TEST>1 | CUSTOM_AGC_MEAN>50, filter_fc := 1L]
# fluxes outside 30 umol/m2/s are unrealistic
flux[FC<(-30)|FC>30, filter_fc := 1L]


# look at the fluxes by month for each year
p_flux <- ggplot(flux[filter_fc !=1&month_orig==1,],
       aes(DOY_START,FC))+
  geom_point(aes(colour=factor(FC_SSITC_TEST)))+
  geom_line()+
  geom_hline(yintercept=c(-5,5))+
  #ylim(c(-5,5))+
         facet_grid(year_orig~.,scales="free_y")

# p_flux

###### H ######
# look at H  by month for each year
ggplot(flux[month_orig==1&DOY_START<31,],
       aes(DOY_START,H,colour=factor(H_SSITC_TEST)))+
  geom_point()+
  #ylim(c(-30,30))+
  facet_grid(year_orig~.,scales="free_y")

# remove QC >1 and AGC > 50
flux[,filter_H := 0L]
# get rid of QC code >2 and low signal strength
flux[H_SSITC_TEST>1 | CUSTOM_AGC_MEAN>50, filter_H := 1L]
# remove H < -120 and > 560, these are not typical values.
flux[H < (-120) | H > 560, filter_H := 1L]
# in 2015 March there's one H > 400 that's an outlier
flux[year_orig==2015 & month_orig==3 & H>400, filter_H := 1L]
# in 2018 Jan there's one H>400 that's an outlier
flux[year_orig==2018 & month_orig==1 & H>400, filter_H := 1L]


# look at all filtered H
ggplot(flux[filter_H!=1&month_orig==1&DOY_START<31,],
       aes(DOY_START,H))+
  geom_line()+
  #ylim(c(-30,30))+
  facet_grid(year_orig~.,scales="free_y")


##### LE ####
# look at LE  by month for each year
ggplot(flux[month_orig==1&DOY_START<31,],
       aes(DOY_START,LE,colour=factor(LE_SSITC_TEST)))+
  geom_point()+
  #ylim(c(-30,30))+
  facet_grid(year_orig~.,scales="free_y")

# remove QC >1 and AGC > 50
flux[,filter_LE:= 0L]
# get rid of QC code >2 and low signal strength
flux[LE_SSITC_TEST>1 | CUSTOM_AGC_MEAN>50, filter_LE := 1L]

# Remove unreasonable values

# remove LE < (-50) this is a generally outlying value
flux[LE < (-50), filter_LE := 1L]
# remove LE >1000 this is a generally outlying value
flux[LE >1000, filter_LE := 1L]


# look at LE  by month for each year
ggplot(flux[filter_LE!=1&month_orig==1&DOY_START<31,],
       aes(DOY_START,LE,colour=factor(LE_SSITC_TEST)))+
  geom_point()+
  geom_hline(yintercept=c(-50,200))+
  facet_grid(year_orig~.,scales="free_y")

# Before the SD filter remove all fluxes that occur at the moment of a rain event
ggplot()+
  geom_line(data=flux[filter_fc !=1], aes(DOY_START,FC),alpha=0.5)+
  geom_point(data=flux[filter_fc !=1&P_RAIN_1_1_1>0], aes(DOY_START,FC), colour="red",size=0.25)+
  facet_grid(year_orig~.)
  
flux[P_RAIN_1_1_1>0, ':=' (filter_fc = 1L, filter_LE = 1L, filter_H = 1L)]


# Do timestamp correction  prior to rolling mean filtering
# keep the original timestamp and fix date_time column to make all times MST
# (use tz=UTC to prevent convervsion of data)
flux_corrected <- copy(flux)
##rm(flux_long)
##flux<- copy(flux_corrected)

# do nothing before 2011-03-21
flux[date_time_orig<as.POSIXct("2011-03-21 16:00:00",tz="UTC"),
          date_time := date_time_orig]

# minus 1 hour
flux[(date_time_orig>=as.POSIXct("2011-03-21 17:00:00",tz="UTC") & 
             date_time_orig<=as.POSIXct("2011-11-12 12:00:00",tz="UTC")),
          date_time := date_time_orig - hours(1)]

# do nothing
flux[date_time_orig>=as.POSIXct("2011-11-12 12:30:00",tz="UTC") & 
            date_time_orig<as.POSIXct("2012-05-17 16:00:00",tz="UTC"),
          date_time := date_time_orig]

# minus 1 hour
flux[date_time_orig>=as.POSIXct("2012-05-17 17:00:00",tz="UTC") & 
            date_time_orig<=as.POSIXct("2013-01-11 13:00:00",tz="UTC"),
          date_time := date_time_orig - hours(1)]

# do nothing
flux[date_time_orig>=as.POSIXct("2013-01-11 13:30:00",tz="UTC") & 
            date_time_orig<as.POSIXct("2013-08-02 12:00:00",tz="UTC"),
          date_time := date_time_orig]

# minus 1 hour
flux[date_time_orig>=as.POSIXct("2013-08-02 13:00:00",tz="UTC") & 
            date_time_orig<=as.POSIXct("2015-10-19 12:00:00",tz="UTC"),
          date_time := date_time_orig-hours(1)]


# do nothing
flux[date_time_orig>=as.POSIXct("2015-10-19 12:30:00",tz="UTC"),
          date_time := date_time_orig]


flux[,time_diff := date_time-date_time_orig]

# create a new datatable that has all the timestamps needed and merge to date_time
# this will result in NA for all timesstamps that got lost in adjustment
time_all <- data.table(date_time = seq(ymd_hm("2010-01-01 00:30",tz="UTC"),ymd_hm("2020-01-01 00:00",tz="UTC"),by="30 min"),
                       date_time_start = seq(ymd_hm("2010-01-01 00:00",tz="UTC"),ymd_hm("2019-12-31 23:30",tz="UTC"),by="30 min"))

time_all[, ':=' (TIMESTAMP_START_correct = as.character(date_time_start, format= "%Y%m%d%H%M"),
                 TIMESTAMP_END_correct = as.character(date_time, format= "%Y%m%d%H%M"))]


# merge full date_time with flux
flux <- merge(flux,time_all[,.(date_time,TIMESTAMP_START_correct,TIMESTAMP_END_correct)], by="date_time", all.y=TRUE)

# as SW pot from Ameriflux and check
sw.pot <- fread("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/Ameriflux/QA_QC_Report_Ameriflux/US-Jo1_HH_2010_2019_SW_IN_pot.csv",
                sep=",",header=TRUE, na.strings=c("-9999"))

sw.pot[,date_time := parse_date_time(TIMESTAMP_END, "YmdHM",tz="UTC")]

setnames(sw.pot,("SW_IN_POT"),("SW_IN_POT_AF"))

# merge
flux <- merge(flux,sw.pot[,.(date_time,SW_IN_POT_AF)], by="date_time")

# check adjustment
daycheck <- as.Date("2013-08-30")

ggplot(flux[as.Date(date_time)==daycheck])+
  geom_line(aes(date_time, SW_IN_POT_AF),colour="black")+
  geom_line(aes(date_time,SW_IN_1_1_1),colour="red")


# APPLY ROLLING MEANS APPROACH TO FILTER Fc, LE, H
# use rollmeanr from zoo
# 3 days = (24/0.5)*3 = 144
# exclude the level 1 filtered data that removes QC code==2, bad AGC, and values outside +/-30 range
flux[filter_fc !=1, ':=' (FC_rollmean3 = rollapply(FC, width=(24/0.5)*3, fill=0, FUN=mean, na.rm=TRUE, align="right"),
             FC_rollsd3 = rollapply(FC, width=(24/0.5)*3, fill=0, FUN=sd, na.rm=TRUE, align="right"),
             FC_rollmean5 = rollapply(FC, width=(24/0.5)*5, fill=0, FUN=mean, na.rm=TRUE, align="right"),
             FC_rollsd5 = rollapply(FC, width=(24/0.5)*5, fill=0, FUN=sd, na.rm=TRUE, align="right"),
             FC_rollmean7 = rollapply(FC, width=(24/0.5)*7, fill=0, FUN=mean, na.rm=TRUE, align="right"),
             FC_rollsd7 = rollapply(FC, width=(24/0.5)*7, fill=0, FUN=sd, na.rm=TRUE, align="right"))]

# also calculate a 3 day moving mean and SD for daytime and nighttime
flux[filter_fc !=1, ':=' (FC_rollmean3_daynight = rollapply(FC, width=(24/0.5)*3, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                          FC_rollsd3_daynight = rollapply(FC, width=(24/0.5)*3, fill=0, FUN=sd, na.rm=TRUE, align="right")),
     by=NIGHT]


threshold <- 3

# graph the 3 and 5 day SD ribbons around measured flux
ggplot(flux[filter_fc!=1,])+
  geom_line(aes(DOY_START, FC))+
  #geom_line(aes(DOY_START, FC_rollmean), colour="green")+
  geom_ribbon(aes(x=DOY_START, ymin=FC_rollmean3-threshold*FC_rollsd3, ymax=FC_rollmean3+threshold*FC_rollsd3), alpha=0.5)+
  geom_ribbon(aes(x=DOY_START, ymin=FC_rollmean7-threshold*FC_rollsd7, ymax=FC_rollmean7+threshold*FC_rollsd7), colour="blue",alpha=0.3)+
  
  facet_grid(year_orig~.)

# graph the 3 day SD ribbons around measured flux by day/night
ggplot(flux[filter_fc!=1&year==2010,])+
  geom_line(aes(DOY_START, FC))+
  #geom_line(aes(DOY_START, FC_rollmean), colour="green")+
  geom_ribbon(aes(x=DOY_START, ymin=FC_rollmean3_daynight-threshold*FC_rollsd3_daynight,
                  ymax=FC_rollmean3_daynight+threshold*FC_rollsd3_daynight, fill=factor(NIGHT)), alpha=0.5)+
  facet_grid(NIGHT~.)

# mark any Fc>3*FCrollsd3 (3 day moving SD) for removal
flux[,filter_fc_roll := 0L]
flux[filter_fc==1, filter_fc_roll := 1L]
flux[FC>FC_rollmean3+threshold*FC_rollsd3|FC<FC_rollmean3-threshold*FC_rollsd3, filter_fc_roll := 2L]

# by Day/Night mark any Fc>3*FCrollsd3 (3 day moving SD) for removal
flux[,filter_fc_roll_daynight := 0L]
flux[filter_fc==1, filter_fc_roll_daynight := 1L]
flux[FC>FC_rollmean3_daynight+threshold*FC_rollsd3_daynight|
       FC<FC_rollmean3_daynight-threshold*FC_rollsd3_daynight, filter_fc_roll_daynight := 2L]


# view the marked fluxes in ~25 day chunks
ggplot(flux[filter_fc_roll!=1&DOY_START>=1&DOY_START<=30,], aes(DOY_START, FC, colour=factor(filter_fc_roll)))+
  geom_point()+
  facet_grid(year_orig~.)

# view the marked fluxes in ~25 day chunks for day/night
ggplot(flux[filter_fc_roll_daynight!=1&DOY_START>=301&DOY_START<=365,], aes(DOY_START, FC, colour=factor(filter_fc_roll_daynight)))+
  geom_point()+
  facet_grid(year_orig~.)

# graph with the fluxes from the 3SD 3day day/night filter removed
ggplot(flux[filter_fc_roll_daynight==0,], aes(DOY_START, FC))+
  geom_line()+
  facet_grid(year_orig~.)


# SD filter for LE
# COMPARE ROLLING MEANS APPROACH TO FILTER Fc, LE, H
# use rollmeanr from zoo
# 3 days = (24/0.5)*3 = 144
# exclude the level 1 filtered data that removes QC code==2, bad AGC, and values outside +/-30 range
flux[filter_LE !=1, ':=' (LE_rollmean3 = rollapply(LE, width=(24/0.5)*3, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                          LE_rollsd3 = rollapply(LE, width=(24/0.5)*3, fill=0, FUN=sd, na.rm=TRUE, align="right"),
                          LE_rollmean5 = rollapply(LE, width=(24/0.5)*5, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                          LE_rollsd5 = rollapply(LE, width=(24/0.5)*5, fill=0, FUN=sd, na.rm=TRUE, align="right"),
                          LE_rollmean7 = rollapply(LE, width=(24/0.5)*7, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                          LE_rollsd7 = rollapply(LE, width=(24/0.5)*7, fill=0, FUN=sd, na.rm=TRUE, align="right"))]

# also calculate a 3 day moving mean and SD for daytime and nighttime
flux[filter_LE !=1, ':=' (LE_rollmean3_daynight = rollapply(LE, width=(24/0.5)*3, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                          LE_rollsd3_daynight = rollapply(LE, width=(24/0.5)*3, fill=0, FUN=sd, na.rm=TRUE, align="right")),
     by=NIGHT]


# threshold <- 3

# graph the 3 and 5 day SD ribbons around measured flux
ggplot(flux[filter_LE!=1&year==2015,])+
  geom_line(aes(DOY_START, LE))+
  #geom_line(aes(DOY_START, LE_rollmean), colour="green")+
  geom_ribbon(aes(x=DOY_START, ymin=LE_rollmean3-threshold*LE_rollsd3, ymax=LE_rollmean3+threshold*LE_rollsd3), alpha=0.5)+
  geom_ribbon(aes(x=DOY_START, ymin=LE_rollmean7-threshold*LE_rollsd7, ymax=LE_rollmean7+threshold*LE_rollsd7), colour="blue",alpha=0.3)+
  
  facet_grid(year_orig~.)

# graph the 3 day SD ribbons around measured flux by day/night
ggplot(flux[filter_LE!=1&year==2015,])+
  geom_line(aes(DOY_START, LE))+
  #geom_line(aes(DOY_START, LE_rollmean), colour="green")+
  geom_ribbon(aes(x=DOY_START, ymin=LE_rollmean3_daynight-threshold*LE_rollsd3_daynight,
                  ymax=LE_rollmean3_daynight+threshold*LE_rollsd3_daynight, fill=factor(NIGHT)), alpha=0.5)+
  facet_grid(NIGHT~.)

# mark any LE>3*LErollsd3 (3 day moving SD) for removal
flux[,filter_le_roll := 0L]
flux[filter_LE==1, filter_le_roll := 1L]
flux[LE>LE_rollmean3+threshold*LE_rollsd3|LE<LE_rollmean3-threshold*LE_rollsd3, filter_le_roll := 2L]

# by Day/Night mark any LE>3*LErollsd3 (3 day moving SD) for removal
flux[,filter_le_roll_daynight := 0L]
flux[filter_LE==1, filter_le_roll_daynight := 1L]
flux[LE>LE_rollmean3_daynight+threshold*LE_rollsd3_daynight|
       LE<LE_rollmean3_daynight-threshold*LE_rollsd3_daynight, filter_le_roll_daynight := 2L]


# view the marked fluxes in ~25 day chunks
ggplot(flux[filter_le_roll!=1&DOY_START>=25&DOY_START<=50,], aes(DOY_START, LE, colour=factor(filter_le_roll)))+
  geom_point()+
  facet_grid(year_orig~.)

# view the marked fluxes in ~25 day chunks for day/night
ggplot(flux[filter_le_roll_daynight!=1&DOY_START>=51&DOY_START<=100,], aes(DOY_START, LE, colour=factor(filter_le_roll_daynight)))+
  geom_point()+
  facet_grid(year_orig~.)

# graph with the fluxes from the 3SD 3day day/night filter removed
ggplot(flux[filter_le_roll_daynight==0,], aes(DOY_START, LE))+
  geom_line()+
  facet_grid(year_orig~.)

# graph fluxes with the 3SD 3day day/night filter removed and with my manual filter
ggplot()+
  geom_line(data=flux[filter_LE==0], aes(DOY_START, LE, colour="manual"), colour="blue")+
  geom_line(data=flux[filter_le_roll_daynight==0], aes(DOY_START, LE, colour="SD day/night"), colour="green")+
  facet_grid(year_orig~.)


# SD filter for H
# COMPARE ROLLING MEANS APPROACH TO FILTER Fc, LE, H
# use rollmeanr from zoo
# 3 days = (24/0.5)*3 = 144
# exclude the level 1 filtered data that removes QC code==2, bad AGC, and values outside +/-30 range
flux[filter_H !=1, ':=' (H_rollmean3 = rollapply(H, width=(24/0.5)*3, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                          H_rollsd3 = rollapply(H, width=(24/0.5)*3, fill=0, FUN=sd, na.rm=TRUE, align="right"),
                          H_rollmean5 = rollapply(H, width=(24/0.5)*5, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                          H_rollsd5 = rollapply(H, width=(24/0.5)*5, fill=0, FUN=sd, na.rm=TRUE, align="right"),
                          H_rollmean7 = rollapply(H, width=(24/0.5)*7, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                          H_rollsd7 = rollapply(H, width=(24/0.5)*7, fill=0, FUN=sd, na.rm=TRUE, align="right"))]

# also calculate a 3 day moving mean and SD for daytime and nighttime
flux[filter_H !=1, ':=' (H_rollmean3_daynight = rollapply(H, width=(24/0.5)*3, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                          H_rollsd3_daynight = rollapply(H, width=(24/0.5)*3, fill=0, FUN=sd, na.rm=TRUE, align="right")),
     by=NIGHT]


# threshold <- 3

# graph the 3 and 5 day SD ribbons around measured flux
ggplot(flux[filter_H!=1&year==2015,])+
  geom_line(aes(DOY_START, H))+
  #geom_line(aes(DOY_START, H_rollmean), colour="green")+
  geom_ribbon(aes(x=DOY_START, ymin=H_rollmean3-threshold*H_rollsd3, ymax=H_rollmean3+threshold*H_rollsd3), alpha=0.5)+
  geom_ribbon(aes(x=DOY_START, ymin=H_rollmean7-threshold*H_rollsd7, ymax=H_rollmean7+threshold*H_rollsd7), colour="blue",alpha=0.3)+
  
  facet_grid(year_orig~.)

# graph the 3 day SD ribbons around measured flux by day/night
ggplot(flux[filter_H!=1&year==2015,])+
  geom_line(aes(DOY_START, H))+
  #geom_line(aes(DOY_START, H_rollmean), colour="green")+
  geom_ribbon(aes(x=DOY_START, ymin=H_rollmean3_daynight-threshold*H_rollsd3_daynight,
                  ymax=H_rollmean3_daynight+threshold*H_rollsd3_daynight, fill=factor(NIGHT)), alpha=0.5)+
  facet_grid(NIGHT~.)

# mark any H>3*Hrollsd3 (3 day moving SD) for removal
flux[,filter_h_roll := 0L]
flux[filter_H==1, filter_h_roll := 1L]
flux[H>H_rollmean3+threshold*H_rollsd3|H<H_rollmean3-threshold*H_rollsd3, filter_h_roll := 2L]

# by Day/Night mark any H>3*Hrollsd3 (3 day moving SD) for removal
flux[,filter_h_roll_daynight := 0L]
flux[filter_H==1, filter_h_roll_daynight := 1L]
flux[H>H_rollmean3_daynight+threshold*H_rollsd3_daynight|
       H<H_rollmean3_daynight-threshold*H_rollsd3_daynight, filter_h_roll_daynight := 2L]


# view the marked fluxes in ~25 day chunks
ggplot(flux[filter_h_roll!=1&DOY_START>=25&DOY_START<=50,], aes(DOY_START, H, colour=factor(filter_h_roll)))+
  geom_point()+
  facet_grid(year_orig~.)

# view the marked fluxes in ~25 day chunks for day/night
ggplot(flux[filter_h_roll_daynight!=1&DOY_START>=51&DOY_START<=100,], aes(DOY_START, H, colour=factor(filter_h_roll_daynight)))+
  geom_point()+
  facet_grid(year_orig~.)

# graph with the fluxes from the 3SD 3day day/night filter removed
ggplot(flux[filter_h_roll_daynight==0,], aes(DOY_START, H))+
  geom_line()+
  facet_grid(year_orig~.)

# graph fluxes with the 3SD 3day day/night filter removed and with my manual filter
ggplot()+
  geom_line(data=flux[filter_H==0], aes(DOY_START, H, colour="manual"), colour="blue")+
  geom_line(data=flux[filter_h_roll_daynight==0], aes(DOY_START, H, colour="SD day/night"), colour="green")+
  facet_grid(year~.)


#########
########### Filtering DONE ###############
########### 

# save filtered data with SD filter
 setwd("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_EddyPro_filtered")

# 20200212: added all of 2019 (reran Jan - June)
# save(flux,file="JER_flux_2010_2019_EddyPro_Output_filterID_SD_20200212.Rdata")


# 
# 20200206: fixed 2010!! save filtered data set. 
# save(flux,file="JER_flux_2010_2019_EddyPro_Output_filterID_SD_2010good_20200206.Rdata")

# 20200128: save with SD filter implemented
# save(flux,file="JER_flux_2010_2019_EddyPro_Output_filterID_SD_20200128.Rdata")
# 


 flux_filter_sd <- copy(flux[,!(c("date_time_orig","TIMESTAMP_START","TIMESTAMP_END","DOY_START","DOY_END","time_diff","SW_IN_POT_AF")),with=FALSE])
 flux_filter_sd[filter_fc_roll_daynight!=0, FC := NA]
 flux_filter_sd[filter_h_roll_daynight!=0, H := NA]
 flux_filter_sd[filter_le_roll_daynight!=0, LE := NA]

 
 # 20200427: corrected timestamps!
# save(flux_filter_sd,
# file="JER_flux_2010_2019_EddyPro_Output_filtered_SD_TIMEcorr_20200427.Rdata")
 
 
 # 20200212: added all of 2019 (reran Jan - June)
#  save(flux_filter_sd,file="JER_flux_2010_2019_EddyPro_Output_filtered_SD_20200212.Rdata")
 
 
# 20200206: fixed 2010!! save filtered data set. 
# save(flux,file="JER_flux_2010_2019_EddyPro_Output_filtered_SD_2010good_20200206.Rdata")

# 20200128: save with SD filter implemented
# save(flux_filter_sd,
#      file="JER_flux_2010_2019_EddyPro_Output_filtered_SD_20200128.Rdata")

 ### save data for Dawn's LTAR synthesis
 # 20200427: LTAR data is not timestamp corrected... but using daily data so that really won't matter!
 # select 2014-2018 and use the SD filtered data. 
 # the data contains only filtered data, all flagged data in Fc, H, LE removed
 flux.ltar <- copy(flux_filter_sd[year %in% c(2014,2015,2016,2017,2018),])
 
 # subset only desired data
 flux.ltar1 <- copy(flux.ltar[,.(TIMESTAMP_START,
                                 TIMESTAMP_END,
                                 FC,
                                 H,
                                 LE,
                                 FC_SSITC_TEST,
                                 H_SSITC_TEST,
                                 LE_SSITC_TEST,
                                 USTAR,
                                 TA_1_1_1,
                                 RH_1_1_1,
                                 PA_1_1_1,
                                 WD_1_1_1,
                                 MWS_1_1_1,
                                 PPFD_IN_1_1_1,
                                 P_RAIN_1_1_1,
                                 LW_IN_1_1_1,
                                 LW_OUT_1_1_1,
                                 SW_OUT_1_1_1,
                                 SW_IN_1_1_1)])
 
 
 # make a quick figures to make sure data is there and OK
 flux.ltar.long <- melt.data.table(flux.ltar1[,.(TIMESTAMP_END,FC,H,LE,USTAR)],
                                   c("TIMESTAMP_END"))
 
 ggplot(flux.ltar.long,aes(parse_date_time_orig(TIMESTAMP_END,"YmdHM",tz="UTC"), value))+
   geom_line()+
   facet_grid(variable~.,scales="free_y")
 
 # save
 setwd("~/Desktop/TweedieLab/Projects/Jornada/LTAR_Synthesis_Browning")
 
 # 20200310 version has 2014-2018 data with SD filter and 30min with rain events removed
 # write.table(flux.ltar1, file="FluxData_jerbajada_20200310.csv",
#  sep=",", dec=".",row.names=FALSE, na="-9999", quote=FALSE)
 
 # metadata file is created in Jornada_EddyPro_Output.R because that is able to take units from the full output files
 # I manually modified and renamed the metadata file because the output here has _1_1_1 in all the biomet variables
 ## write.table(description.ltar, file="FluxData_jerbajada_METADATA_20190813.csv",sep=",", dec=".", row.names=FALSE)
 
 
 
