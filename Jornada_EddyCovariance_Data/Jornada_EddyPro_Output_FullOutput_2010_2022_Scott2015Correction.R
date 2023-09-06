
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
library(cowplot)
#############
# IMPORT DATA
#############
# EddyPro 7.0.4 has column name slip problems, using Fluxnet output!
#     -> I think I figured it out Jan 2023 in RecalculateFluxes.R?
#     -> some files have 180 columns because of an additional diag_75_mean column
#     -> I think in 2020 agc_mean value is in diag_75_mean & agc_meean is same as Ta_1_1_1.
#     -> In 2021 agc_mean is correct 

# read the data in full output format after manual wpl correction and adjusted correction
# see RecalculateFluxes.R
# fc_wpl = reproduce WPL correction to check math
# fc_wpl_adjust = adjust w'co2 *0.9 and correct for scf and wpl -> according to Scott et al 2015
# LE_wpl = reproduce WPL correction to check math

load("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_CovarianceCorrect_Scott2015/JER_flux_EddyPro_FullOutput_Scott2015_Correct_20230112.RData")

# create date_time variable
flux[,date_time := ymd_hm(paste(date,time,sep="_"))]

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

# Import biomet data to join with fluxes
# load biomet data
biomet.names <- colnames(fread("/Volumes/SEL_Data_Archive/Research Data/Desert/Jornada/Bahada/Tower/EddyCovariance_ts/EddyPro_Biomet/Biomet_EddyPro_2010.csv",
                                   header=TRUE))

biomet2010 <- fread("/Volumes/SEL_Data_Archive/Research Data/Desert/Jornada/Bahada/Tower/EddyCovariance_ts/EddyPro_Biomet/Biomet_EddyPro_2010.csv",
                    skip=2, header=FALSE, col.names=biomet.names, na.strings=c("-9999"))

biomet2011 <- fread("/Volumes/SEL_Data_Archive/Research Data/Desert/Jornada/Bahada/Tower/EddyCovariance_ts/EddyPro_Biomet/Biomet_EddyPro_2011.csv",
                    skip=2, header=FALSE, col.names=biomet.names, na.strings=c("-9999"))

biomet2012 <- fread("/Volumes/SEL_Data_Archive/Research Data/Desert/Jornada/Bahada/Tower/EddyCovariance_ts/EddyPro_Biomet/Biomet_EddyPro_2012.csv",
                    skip=2, header=FALSE, col.names=biomet.names, na.strings=c("-9999"))
biomet2013 <- fread("/Volumes/SEL_Data_Archive/Research Data/Desert/Jornada/Bahada/Tower/EddyCovariance_ts/EddyPro_Biomet/Biomet_EddyPro_2013.csv",
                    skip=2, header=FALSE, col.names=biomet.names, na.strings=c("-9999"))
biomet2014 <- fread("/Volumes/SEL_Data_Archive/Research Data/Desert/Jornada/Bahada/Tower/EddyCovariance_ts/EddyPro_Biomet/Biomet_EddyPro_2014.csv",
                    skip=2, header=FALSE, col.names=biomet.names, na.strings=c("-9999"))
biomet2015 <- fread("/Volumes/SEL_Data_Archive/Research Data/Desert/Jornada/Bahada/Tower/EddyCovariance_ts/EddyPro_Biomet/Biomet_EddyPro_2015.csv",
                    skip=2, header=FALSE, col.names=biomet.names, na.strings=c("-9999"))
biomet2016 <- fread("/Volumes/SEL_Data_Archive/Research Data/Desert/Jornada/Bahada/Tower/EddyCovariance_ts/EddyPro_Biomet/Biomet_EddyPro_2016.csv",
                    skip=2, header=FALSE, col.names=biomet.names, na.strings=c("-9999"))
biomet2017 <- fread("/Volumes/SEL_Data_Archive/Research Data/Desert/Jornada/Bahada/Tower/EddyCovariance_ts/EddyPro_Biomet/Biomet_EddyPro_2017.csv",
                    skip=2, header=FALSE, col.names=biomet.names, na.strings=c("-9999"))
biomet2018 <- fread("/Volumes/SEL_Data_Archive/Research Data/Desert/Jornada/Bahada/Tower/EddyCovariance_ts/EddyPro_Biomet/Biomet_EddyPro_2018.csv",
                    skip=2, header=FALSE, col.names=biomet.names, na.strings=c("-9999"))
biomet2019 <- fread("/Volumes/SEL_Data_Archive/Research Data/Desert/Jornada/Bahada/Tower/EddyCovariance_ts/EddyPro_Biomet/Biomet_EddyPro_2019.csv",
                    skip=2, header=FALSE, col.names=biomet.names, na.strings=c("-9999"))
biomet2020 <- fread("/Volumes/SEL_Data_Archive/Research Data/Desert/Jornada/Bahada/Tower/EddyCovariance_ts/EddyPro_Biomet/Biomet_EddyPro_2020.csv",
                    skip=2, header=FALSE, col.names=biomet.names, na.strings=c("-9999"))
biomet2021 <- fread("/Volumes/SEL_Data_Archive/Research Data/Desert/Jornada/Bahada/Tower/EddyCovariance_ts/EddyPro_Biomet/Biomet_EddyPro_2021.csv",
                    skip=2, header=FALSE, col.names=biomet.names, na.strings=c("-9999"))
biomet2022 <- fread("/Volumes/SEL_Data_Archive/Research Data/Desert/Jornada/Bahada/Tower/EddyCovariance_ts/EddyPro_Biomet/Biomet_EddyPro_2022.csv",
                    skip=2, header=FALSE, col.names=biomet.names, na.strings=c("-9999"))

# combine
biomet <- rbind(biomet2010,biomet2011,biomet2012,biomet2013, biomet2014, biomet2015,
                biomet2016, biomet2017, biomet2018, biomet2019, biomet2020,
                biomet2021, biomet2022)

biomet <- biomet[,':='
                         (date_time=ymd_hm(paste(timestamp_1,timestamp_2,timestamp_3,timestamp_4,timestamp_5, sep=" ")))]

ggplot(biomet, aes(date_time,P_rain_1_1_1))+
  geom_line()+
  facet_grid(.~year(date_time), scales="free_x")

# the rain record in 2010 that's >40mm is an error (no rain according to phenocams)
biomet <- biomet[P_rain_1_1_1>40, P_rain_1_1_1:= NA]

# merge flux and biomet data on date_time
flux_orig <- copy(flux)

flux <- merge(flux,biomet, by="date_time",all=TRUE)

# remove individual year biomet files
rm(biomet2010,biomet2011,biomet2012,biomet2013, biomet2014, biomet2015,
      biomet2016, biomet2017, biomet2018, biomet2019, biomet2020,
      biomet2021, biomet2022)
# make some plots
# graph precipitation
ggplot(flux,aes(date_time,P_rain_1_1_1))+geom_line()
# graph soil moisture
ggplot(flux,aes(date_time,SWC_1_1_1))+geom_line()

# graph air temp
ggplot(flux,aes(date_time,Ta_1_1_1))+geom_line()+
  facet_grid(.~year(date_time), scales="free_x")

# graph PAR
ggplot(flux,aes(date_time,PPFD_1_1_1))+geom_line()

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

# graph agc_mean
ggplot(flux, aes(date_time,agc_mean))+
  geom_point()+
  facet_grid(.~year(date_time), scales="free_x")

# 2023-01-12: it looks like in 2020 agc_mean values are in diag_75_mean, but I am not sure why
# in 2021 diag75_mean and agc_mean exist, and AGC values are in agc_mean and diag values are totally different
flux[year(date)<2020|year(date)>2020, agc_mean_correct := agc_mean]
flux[year(date)==2020, agc_mean_correct := diag_75_mean]

# AGC, signal strength = INST_LI7500_AGC_OR_RSSI or CUSTOM_AGC_MEAN (they have flipped signs)
ggplot(flux[qc_co2_flux<2,], aes(DOY,co2_flux,colour=factor(qc_co2_flux)))+
  geom_point()+
  ylim(c(-10,30))+
  facet_grid(.~year(date_time))


ggplot(flux, aes(date_time,agc_mean_correct))+
  geom_point()+
  facet_grid(.~year(date_time), scales="free_x")

# AGC filter for all fluxes
# create a column for filtering CO2 flux, H2O flux, and H
# filter_fc = 1 to remove and = 0 to keep
flux[,filter_fc := 0L]
flux[,filter_LE := 0L]
flux[,filter_H := 0L]

# agc_mean 50 in most years
# in 2020, 2021 and 2022 different due to instrument changes

# 2020-2022 IRGA changes: 20 Nov 20 -> loaner  (AGC baseline: ~50)
#               26 Feb 21 -> remove loaner & re-install lab IRGA & CSAT
#               15/17 Dec -> loaner (AGC baseline: 63)
#              8 Apr 22 -> remove loaner & re-install lab IRGA (AGC: 56)

# remove all very low AGC (only few points)
flux[agc_mean_correct <41, ':=' (filter_fc = 1L,
                                 filter_LE = 1L,
                                 filter_H = 1L)]

# filter AGC by values less than 52 prior to 17 Dec 2021
flux[date_time < as.Date ("2021-12-16") & agc_mean_correct>50,
         ':=' (filter_fc = 1L,
                                 filter_LE = 1L,
                                 filter_H = 1L)]

# filter AGC by values less than 66 after the 17 Dec
flux[date_time > as.Date ("2021-12-16") &
           date_time < as.Date ("2022-04-08") & agc_mean_correct>66,
         ':=' (filter_fc = 1L,
               filter_LE = 1L,
               filter_H = 1L)]

# filter AGC by values between 50 to 65 on and after 8th Apr
flux[date_time >= as.Date ("2022-04-08") & 
           (agc_mean_correct<50 & agc_mean_correct>65),
         ':=' (filter_fc = 1L,
                                 filter_LE = 1L,
                                 filter_H = 1L)]
 ###### CO2  ######
 # get rid of QC code >2 and low signal strength
 flux[qc_co2_flux>1, filter_fc := 1L]
 # fluxes outside 30 umol/m2/s are unrealistic
 flux[co2_flux<(-30)|co2_flux>30, filter_fc := 1L]
 
 # IRGA was behaving badly from 20 Apr to  11 Jun ~7am MDT 2021
 # due to issues with connection and sensor head power reset
 flux[as.Date(date_time)>as.Date("2021-04-20") & 
        ymd_hms(date_time)<ymd_hms("2021-07-01 07:00:00"), filter_fc := 1L]
 
 
 # look at the fluxes by month for each year
 # EddyPro CO2 flux and recalculated Scott correction
 p_fc <- ggplot(flux[filter_fc !=1&month(date)==5,],
                  aes(x=DOY))+
   geom_point(aes(y=co2_flux), size=0.2)+
   geom_point(aes(y=fc_wpl_adjust),colour="green",size=0.2)+
   geom_hline(yintercept=c(-5,5))+
   #ylim(c(-5,5))+
   facet_grid(year(date)~.,scales="free_y")
 
 p_fc
 
###### H ######
# look at H  by month for each year
ggplot(flux,
       aes(DOY,H,colour=factor(qc_H)))+
  geom_point(size=0.2)+
  facet_grid(year~.)


# get rid of QC code >2 
flux[qc_H>1 , filter_H := 1L]
# remove H < -120 and > 560, these are not typical values.
flux[H < (-120) | H > 560, filter_H := 1L]
# in 2015 March there's one H > 400 that's an outlier
flux[year==2015 & month(date_time)==3 & H>400, filter_H := 1L]
# in 2018 Jan there's one H>400 that's an outlier
flux[year==2018 & month(date_time)==1 & H>400, filter_H := 1L]

# IRGA was behaving badly from 20 Apr to  11 Jun ~7am MDT 2021
# due to issues with connection and sensor head power reset
flux[as.Date(date_time)>as.Date("2021-04-20") & 
       ymd_hms(date_time)<ymd_hms("2021-07-01 07:00:00"), filter_H := 1L]


# look at all filtered H
ggplot(flux[filter_H!=1,],
       aes(DOY,H, colour=filter_H))+
  geom_line()+
  facet_grid(year~.,)


##### LE ####
# look at LE  
ggplot(flux,
       aes(DOY,LE,colour=factor(qc_LE)))+
  geom_point(size=0.2)+
  facet_grid(year~.)


# get rid of QC code >2 
flux[qc_LE>1 , filter_LE := 1L]

# Remove unreasonable values

# remove LE < (-50) this is a generally outlying value
flux[LE < (-50), filter_LE := 1L]
# remove LE >1000 this is a generally outlying value
flux[LE >1000, filter_LE := 1L]

# IRGA was behaving badly from 20 Apr to  11 Jun ~7am MDT 2021
# due to issues with connection and sensor head power reset
flux[as.Date(date_time)>as.Date("2021-04-20") & 
       ymd_hms(date_time)<ymd_hms("2021-07-01 07:00:00"), filter_LE := 1L]

# look at LE  by month for each year
ggplot(flux[filter_LE!=1,],
       aes(DOY,LE,colour=factor(qc_LE)))+
  geom_point(size=0.2)+
  geom_hline(yintercept=c(-50,200))+
  facet_grid(year~.)

# In 2013 there were issues with water concentration during the summer
# remove LE, H, CO2 flux
ggplot(flux[filter_LE!=1,],
       aes(DOY,LE,colour=factor(qc_LE)))+
  geom_point(size=0.2)+
  facet_grid(year~.)

ggplot(flux[filter_fc!=1,],
       aes(DOY,co2_molar_density,colour=factor(qc_co2_flux)))+
  geom_point(size=0.2)+
  facet_grid(year~.)

# 2013 only
LE.2013 <- ggplot(flux[filter_LE!=1&(year==2012 | year==2013 | year==2014),],
       aes(DOY,LE,colour=factor(qc_LE)))+
  geom_point(size=0.2)+
  geom_hline(yintercept=c(-50,200))+
  facet_grid(year~.)+
  theme(legend.position = "none")

h20.2013 <- ggplot(flux[filter_LE!=1&(year==2012 | year==2013 | year==2014),],
       aes(DOY,h2o_molar_density,colour=factor(qc_LE)))+
  geom_point(size=0.2)+
  facet_grid(year~.)+
  theme(legend.position = "none")

fc.2013 <- ggplot(flux[filter_fc!=1&(year==2012 | year==2013 | year==2014),],
       aes(DOY,co2_flux,colour=factor(qc_co2_flux)))+
  geom_point(size=0.2)+
  facet_grid(year~.)+
  theme(legend.position = "none")

plot_grid(LE.2013, h20.2013, fc.2013, nrow=3, align="v")

# remove fluxes in 2013 between DOY 99-291 
ggplot(flux[filter_LE!=1&year==2013&DOY>98&DOY<292,],
               aes(DOY,LE,colour=factor(qc_LE)))+
     geom_point(size=0.2)+
     facet_grid(year~.)

# remove with filter
flux[date_time >= as.Date ("2013-04-09") &
       date_time < as.Date ("2013-10-18"),
     ':=' (filter_fc = 1L,
           filter_LE = 1L,
           filter_H = 1L)]

# Before the SD filter remove all fluxes that occur at the moment of a rain event
ggplot()+
  geom_line(data=flux[filter_fc !=1], aes(DOY,co2_flux),alpha=0.5)+
  geom_point(data=flux[filter_fc !=1&P_rain_1_1_1>0], aes(DOY,co2_flux), colour="red",size=0.25)+
  facet_grid(year~.)
  
flux[P_rain_1_1_1>0, ':=' (filter_fc = 1L, filter_LE = 1L, filter_H = 1L)]


# Do timestamp correction  prior to rolling mean filtering
# keep the original timestamp and fix date_time column to make all times MST
# (use tz=UTC to prevent convervsion of data)
flux_orig <- copy(flux)
##flux<- copy(flux_orig)

# rename date_time as date_time_orig and remove other date/time associated variables
# timestamp_1, timestamp_2, timestamp_3, timestamp_4, date, time, DOY

flux[,':=' (date_time_orig = date_time,
            timestamp_1 = NULL,
            timestamp_2 = NULL,
            timestamp_3 = NULL, 
            timestamp_4 = NULL,
            date = NULL,
            time = NULL,
            DOY = NULL)][,':='(date_time = NULL)]

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
time_all <- data.table(date_time = seq(ymd_hm("2010-01-01 00:30",tz="UTC"),ymd_hm("2022-12-31 00:00",tz="UTC"),by="30 min"))

# merge full date_time with flux
flux <- merge(flux,time_all[,.(date_time)], by="date_time", all.y=TRUE)

# as SW pot from Ameriflux and check
sw.pot <- fread("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/Ameriflux/QA_QC_Report_Ameriflux/US-Jo1_HH_2010_2019_SW_IN_pot.csv",
                sep=",",header=TRUE, na.strings=c("-9999"))

sw.pot[,date_time := parse_date_time(TIMESTAMP_END, "YmdHM",tz="UTC")]

setnames(sw.pot,("SW_IN_POT"),("SW_IN_POT_AF"))

# merge
flux.swpot <- merge(flux,sw.pot[,.(date_time,SW_IN_POT_AF)], by="date_time")

# check adjustment
daycheck <- as.Date("2014-01-01")

ggplot(flux[as.Date(date_time)==daycheck])+
  geom_line(aes(date_time_orig, Rn_1_1_1),colour="black")+
  geom_line(aes(date_time,Rn_1_1_1),colour="red")

ggplot(flux.swpot[as.Date(date_time)==daycheck])+
  geom_line(aes(date_time, SW_IN_POT_AF),colour="blue")+
  geom_line(aes(date_time,Rn_1_1_1),colour="red")


# APPLY ROLLING MEANS APPROACH TO FILTER Fc, LE, H
# use rollmeanr from zoo
# 3 days = (24/0.5)*3 = 144
# exclude the level 1 filtered data that removes QC code==2, bad AGC, and values outside +/-30 range
flux[filter_fc !=1, ':=' (FC_rollmean3 = rollapply(co2_flux, width=(24/0.5)*3, fill=0, FUN=mean, na.rm=TRUE, align="right"),
             FC_rollsd3 = rollapply(co2_flux, width=(24/0.5)*3, fill=0, FUN=sd, na.rm=TRUE, align="right"),
             FC_rollmean5 = rollapply(co2_flux, width=(24/0.5)*5, fill=0, FUN=mean, na.rm=TRUE, align="right"),
             FC_rollsd5 = rollapply(co2_flux, width=(24/0.5)*5, fill=0, FUN=sd, na.rm=TRUE, align="right"),
             FC_rollmean7 = rollapply(co2_flux, width=(24/0.5)*7, fill=0, FUN=mean, na.rm=TRUE, align="right"),
             FC_rollsd7 = rollapply(co2_flux, width=(24/0.5)*7, fill=0, FUN=sd, na.rm=TRUE, align="right"))]

# also calculate a 3 day moving mean and SD for daytime and nighttime
# daytime column, 0 = night, 1= day
flux[filter_fc !=1, ':=' (FC_rollmean3_daynight = rollapply(co2_flux, width=(24/0.5)*3, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                          FC_rollsd3_daynight = rollapply(co2_flux, width=(24/0.5)*3, fill=0, FUN=sd, na.rm=TRUE, align="right")),
     by=daytime]


threshold <- 3

# graph the 3 and 5 day SD ribbons around measured flux
ggplot(flux[filter_fc!=1&year(date_time)==2011,])+
  geom_line(aes(yday(date_time), co2_flux))+
  #geom_line(aes(DOY_START, FC_rollmean), colour="green")+
  geom_ribbon(aes(x=yday(date_time), ymin=FC_rollmean3-threshold*FC_rollsd3, ymax=FC_rollmean3+threshold*FC_rollsd3), alpha=0.5)+
  geom_ribbon(aes(x=yday(date_time), ymin=FC_rollmean7-threshold*FC_rollsd7, ymax=FC_rollmean7+threshold*FC_rollsd7), colour="blue",alpha=0.3)+
  facet_grid(year(date_time)~.)

# graph the 3 day SD ribbons around measured flux by day/night
ggplot(flux[filter_fc!=1&year(date_time)==2010,])+
  geom_line(aes(yday(date_time), co2_flux))+
  #geom_line(aes(DOY_START, FC_rollmean), colour="green")+
  geom_ribbon(aes(x=yday(date_time), ymin=FC_rollmean3_daynight-threshold*FC_rollsd3_daynight,
                  ymax=FC_rollmean3_daynight+threshold*FC_rollsd3_daynight, fill=factor(daytime)), alpha=0.5)+
  facet_grid(daytime~.)

# mark any Fc>3*FCrollsd3 (3 day moving SD) for removal
flux[,filter_fc_roll := 0L]
flux[filter_fc==1, filter_fc_roll := 1L]
flux[co2_flux>FC_rollmean3+threshold*FC_rollsd3|
       co2_flux<FC_rollmean3-threshold*FC_rollsd3, filter_fc_roll := 2L]

# don't remove the first 6 days of Jan just because they are at the begining of the record
flux[date_time<as.Date("2010-01-07"), filter_fc_roll := 0L]

# by Day/Night mark any Fc>3*FCrollsd3 (3 day moving SD) for removal
flux[,filter_fc_roll_daynight := 0L]
flux[filter_fc==1, filter_fc_roll_daynight := 1L]
flux[co2_flux>FC_rollmean3_daynight+threshold*FC_rollsd3_daynight|
       co2_flux<FC_rollmean3_daynight-threshold*FC_rollsd3_daynight, filter_fc_roll_daynight := 2L]

# don't remove the first 6 days of Jan just because they are at the begining of the record
flux[date_time<as.Date("2010-01-07"), filter_fc_roll_daynight := 0L]

# view the marked fluxes in ~25 day chunks
ggplot(flux[filter_fc_roll!=1&year(date_time)==2010&month(date_time)==1,],
       aes(date_time, co2_flux, colour=factor(filter_fc_roll)))+
  geom_point()+
  facet_grid(year(date_time)~.)

# view the marked fluxes in ~25 day chunks for day/night
ggplot(flux[filter_fc_roll!=1&year(date_time)==2010,],
       aes(date_time, co2_flux, colour=factor(filter_fc_roll_daynight)))+
  geom_point()+
  facet_grid(year(date_time)~.)

# graph with the fluxes from the 3SD 3day day/night filter removed
ggplot(flux[filter_fc_roll_daynight==0,], aes(yday(date_time), co2_flux))+
  geom_line()+
  facet_grid(year~.)

# graph EddyPro and Scott corrected with the fluxes from the 3SD 3day day/night filter removed
ggplot(flux[filter_fc_roll_daynight==0,])+
  geom_point(aes(yday(date_time), co2_flux), colour="black", size=0.2)+
  geom_point(aes(yday(date_time), fc_wpl_adjust), colour="green", size=0.2)+
  facet_grid(year~.)


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
# daytime= 0 = night, daytime = 1 = day
flux[filter_LE !=1, ':=' (LE_rollmean3_daynight = rollapply(LE, width=(24/0.5)*3, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                          LE_rollsd3_daynight = rollapply(LE, width=(24/0.5)*3, fill=0, FUN=sd, na.rm=TRUE, align="right")),
     by=daytime]


# threshold <- 3

# graph the 3 and 5 day SD ribbons around measured flux
ggplot(flux[filter_LE!=1&year(date_time)==2013,])+
  geom_line(aes(yday(date_time), LE))+
  #geom_line(aes(DOY_START, LE_rollmean), colour="green")+
  geom_ribbon(aes(x=yday(date_time), ymin=LE_rollmean3-threshold*LE_rollsd3, ymax=LE_rollmean3+threshold*LE_rollsd3), alpha=0.5)+
  geom_ribbon(aes(x=yday(date_time), ymin=LE_rollmean7-threshold*LE_rollsd7, ymax=LE_rollmean7+threshold*LE_rollsd7), colour="blue",alpha=0.3)+
  
  facet_grid(year(date_time)~.)

# graph the 3 day SD ribbons around measured flux by day/night
ggplot(flux[filter_LE!=1&year(date_time)==2010,])+
  geom_line(aes(yday(date_time), LE))+
  #geom_line(aes(DOY_START, LE_rollmean), colour="green")+
  geom_ribbon(aes(x=yday(date_time), ymin=LE_rollmean3_daynight-threshold*LE_rollsd3_daynight,
                  ymax=LE_rollmean3_daynight+threshold*LE_rollsd3_daynight, fill=factor(daytime)), alpha=0.5)+
  facet_grid(daytime~.)

# mark any LE>3*LErollsd3 (3 day moving SD) for removal
flux[,filter_le_roll := 0L]
flux[filter_LE==1, filter_le_roll := 1L]
flux[LE>LE_rollmean3+threshold*LE_rollsd3|LE<LE_rollmean3-threshold*LE_rollsd3, filter_le_roll := 2L]

# don't remove the first 6 days of Jan just because they are at the begining of the record
flux[date_time<as.Date("2010-01-07"), filter_le_roll := 0L]


# by Day/Night mark any LE>3*LErollsd3 (3 day moving SD) for removal
flux[,filter_le_roll_daynight := 0L]
flux[filter_LE==1, filter_le_roll_daynight := 1L]
flux[LE>LE_rollmean3_daynight+threshold*LE_rollsd3_daynight|
       LE<LE_rollmean3_daynight-threshold*LE_rollsd3_daynight, filter_le_roll_daynight := 2L]

# don't remove the first 6 days of Jan just because they are at the begining of the record
flux[date_time<as.Date("2010-01-07"), filter_le_roll_daynight := 0L]

# view the marked fluxes in ~25 day chunks
ggplot(flux[filter_le_roll!=1&year(date_time)==2010,],
       aes(date_time, LE, colour=factor(filter_le_roll)))+
  geom_point()+
  facet_grid(year(date_time)~.)

# view the marked fluxes in ~25 day chunks for day/night
ggplot(flux[filter_le_roll_daynight!=1&year(date_time)==2010,],
       aes(date_time, LE, colour=factor(filter_le_roll_daynight)))+
  geom_point()+
  facet_grid(year(date_time)~.)

# graph with the fluxes from the 3SD 3day day/night filter removed
ggplot(flux[filter_le_roll_daynight==0,], aes(yday(date_time), LE))+
  geom_line()+
  facet_grid(year(date_time)~.)


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
     by=daytime]


# threshold <- 3

# graph the 3 and 5 day SD ribbons around measured flux
ggplot(flux[filter_H!=1&year(date_time)==2010,])+
  geom_line(aes(yday(date_time), H))+
  #geom_line(aes(DOY_START, H_rollmean), colour="green")+
  geom_ribbon(aes(x=yday(date_time), ymin=H_rollmean3-threshold*H_rollsd3, ymax=H_rollmean3+threshold*H_rollsd3), alpha=0.5)+
  geom_ribbon(aes(x=yday(date_time), ymin=H_rollmean7-threshold*H_rollsd7, ymax=H_rollmean7+threshold*H_rollsd7), colour="blue",alpha=0.3)+
  
  facet_grid(year(date_time)~.)

# graph the 3 day SD ribbons around measured flux by day/night
ggplot(flux[filter_H!=1&year(date_time)==2015,])+
  geom_line(aes(yday(date_time), H))+
  #geom_line(aes(DOY_START, H_rollmean), colour="green")+
  geom_ribbon(aes(x=yday(date_time), ymin=H_rollmean3_daynight-threshold*H_rollsd3_daynight,
                  ymax=H_rollmean3_daynight+threshold*H_rollsd3_daynight, fill=factor(daytime)), alpha=0.5)+
  facet_grid(daytime~.)

# mark any H>3*Hrollsd3 (3 day moving SD) for removal
flux[,filter_h_roll := 0L]
flux[filter_H==1, filter_h_roll := 1L]
flux[H>H_rollmean3+threshold*H_rollsd3|H<H_rollmean3-threshold*H_rollsd3, filter_h_roll := 2L]

# don't remove the first 6 days of Jan just because they are at the begining of the record
flux[date_time<as.Date("2010-01-07"), filter_h_roll := 0L]

# by Day/Night mark any H>3*Hrollsd3 (3 day moving SD) for removal
flux[,filter_h_roll_daynight := 0L]
flux[filter_H==1, filter_h_roll_daynight := 1L]
flux[H>H_rollmean3_daynight+threshold*H_rollsd3_daynight|
       H<H_rollmean3_daynight-threshold*H_rollsd3_daynight, filter_h_roll_daynight := 2L]

# don't remove the first 6 days of Jan just because they are at the begining of the record
flux[date_time<as.Date("2010-01-07"), filter_h_roll_daynight := 0L]


# view the marked fluxes in ~25 day chunks
ggplot(flux[filter_h_roll!=1,],
       aes(yday(date_time), H, colour=factor(filter_h_roll)))+
  geom_point()+
  facet_grid(year(date_time)~.)

# view the marked fluxes in ~25 day chunks for day/night
ggplot(flux[filter_h_roll!=1,],
       aes(yday(date_time), H, colour=factor(filter_h_roll_daynight)))+
  geom_point()+
  facet_grid(year(date_time)~.)

# graph with the fluxes from the 3SD 3day day/night filter removed
ggplot(flux[filter_h_roll_daynight==0,], aes(yday(date_time), H))+
  geom_line()+
  facet_grid(year(date_time)~.)


#########
########### Filtering DONE ###############
########### 

# save filtered data with SD filter
 setwd("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_EddyPro_filtered")

# drop unwamnted columns and make fluxes NA with filter criteria applied
flux_filter_sd <- copy(flux[,!(c("time_diff")),with=FALSE])
flux_filter_sd[filter_fc_roll_daynight!=0, ':=' (co2_flux = NA, 
                                                 fc_wpl_adjust = NA)]
flux_filter_sd[filter_h_roll_daynight!=0, H := NA]
flux_filter_sd[filter_le_roll_daynight!=0, LE := NA]

# make sure there are no duplicates
flux_filter_sd <- (flux_filter_sd[!(duplicated(flux_filter_sd, by=c("date_time")))])

# 20230115: run with full output from Eddy Pro with fixes for column differences
#           apply all filtering on Eddypro Co2, LE, H fluxes
#           include test column _wpl and Scott et al 2015 corrected covariances for 
#           co2 only: fc_wpl_adjust

 save(flux_filter_sd,file="JER_flux_2010_2022_EddyPro_FullOutput_filterSD_20230115.Rdata")

# 20200212: added all of 2019 (reran Jan - June)
# save(flux,file="JER_flux_2010_2019_EddyPro_Output_filterID_SD_20200212.Rdata")

# 
# 20200206: fixed 2010!! save filtered data set. 
# save(flux,file="JER_flux_2010_2019_EddyPro_Output_filterID_SD_2010good_20200206.Rdata")

# 20200128: save with SD filter implemented
# save(flux,file="JER_flux_2010_2019_EddyPro_Output_filterID_SD_20200128.Rdata")
# 

 
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
 
 # ENERGY BALANCE 
 # look at energy balance - 
 # STEPS (from Gerald)
 # U* filter and remove erroneous fluxes (can use u* > 0.2 for a rough threshold)
 # make sure there's the same number of points for each day
 # (do only when fluxes are available) 
 # daily: (H+LE) / (Rn - (G+S))
 # Storage formula comes from EasyFluxDL program
 # with default values for soil heat storage and BD
 
# # if want to use biomet files, but soil temps are still averaged to 40cm
#   # import biomet2 files that have soil temp and moisture by depth
# biomet2files <- list.files(path="/Volumes/SEL_Data_Archive/Research Data/Desert/Jornada/Bahada/Tower/Ameriflux_USJo1/Biomet2_20201227",
#                             pattern="Biomet_USJo1_wide",
#                             full.names=TRUE) 
#  
#  # read files and bind them into one file. fill=TRUE because of the missing columns in 2011
#  biomet2 <- do.call("rbind", lapply(biomet2files[1:11], header = TRUE,
#                                     fread, sep=",", fill=TRUE,
#                                     na.strings=c(-9999,"#NAME?")))
#                                     
 
 
 flux_filter_sd[,':='(Year=year(date_time),
                      DOY=yday(date_time),
                      date=as.Date(date_time),
                      SHF_1 = (SHF_1_1_1+SHF_1_2_1)/2,
                      SHF_2 = (SHF_2_1_1+ SHF_2_2_1)/2,
                      VWC_Ts = (Ts_1_1_1+273.15)*SWC_1_1_1)][,':='(
                        Tsoil_change = Ts_1_1_1+273.15 - shift(Ts_1_1_1+273.15,n=1,fill=NA), # lag by 1
                        VWC_change = VWC_Ts-shift(VWC_Ts,n=1,fill=NA) # lag by 1
                      )][,':='(
                        Storage=(((870*1300*Tsoil_change+
                                   4210*1000*VWC_change))*0.08)/(30*60)
                      )]
 
 
  flux_filter_sd_eb <- flux_filter_sd[`u*`>0.2,.(date=date,
                                                EB_ec = H+LE,
                                           EB_met1 = (Rn_1_1_1-(SHF_1+Storage)), # surface SHF
                                           EB_met2 = (Rn_1_1_1-(SHF_2+Storage))), # deeper SHF
                                     by="date_time"][,.(EB_ec = sum(EB_ec),
                                                        EB_Met1 = sum(EB_met1),
                                                        EB_Met2 = sum(EB_met2)),
                                                     by="date"]

 # plot storage
 ggplot(flux_filter_sd, aes(date_time,Storage))+
   geom_line(linewidth=0.3)
 
  # compare EB Met using SHF from surface or deep
 ggplot(flux_filter_sd_eb, aes(EB_Met1,EB_Met2))+
   geom_point()+
   geom_abline(intercept=0, slope=1)
 
 # compare EB from ec with met data
 ggplot(flux_filter_sd_eb, aes(EB_ec,EB_Met1))+
   geom_point()+
   geom_abline(intercept=0, slope=1)
 
 # graph ratio of EB_ec/EB_Met
 
 ggplot(flux_filter_sd_eb, aes(date,EB_ec/EB_Met1))+
   geom_point()+
   geom_hline(yintercept=1)
 
 # histogram of EB
 ggplot(flux_filter_sd_eb, aes(EB_ec/EB_Met1))+
     geom_histogram(bins=60)+
   geom_vline(xintercept=c(0.9,0.8,0.7,0.6))
 
# graph Tair and Tsoil
 ggplot(flux_filter_sd, aes(date_time))+
   geom_line(aes(y=Ta_1_1_1), colour="lightblue", linewidth=0.5)+
   geom_line(aes(y=Ts_1_1_1), colour="brown", linewidth=0.5)
 
 # graph VWC
 ggplot(flux_filter_sd, aes(date_time))+
   geom_line(aes(y=SWC_1_1_1), colour="blue", linewidth=0.5)

 # graph SHF of two depths
 ggplot(flux_filter_sd, aes(SHF_1,SHF_2))+
   geom_point()
 
  