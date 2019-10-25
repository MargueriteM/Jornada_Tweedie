############################################
#  This code gets data output by Eddy Pro  #
#           written by: M. Mauritz         #
#             May 2019                     #
#    update: 9 August 2019                   #
############################################

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
#############
# IMPORT DATA
#############
# EddyPro 7.0.4 has column name slip problems, using Fluxnet output!

# read the data in fluxnet format
flux2010a <- fread("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_2010/eddypro_JER_2010_fluxnet_2019-08-08T131210_adv.csv",
                  sep=",", header=TRUE, na.strings=c("-9999"))

flux2010b <- fread("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_2010/20190806_2/eddypro_JER_2010_fluxnet_2019-08-09T172917_adv.csv",
                  sep=",", header=TRUE, na.strings=c("-9999"))


flux2011a <- fread("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_2011/eddypro_JER_2011_fluxnet_2019-08-08T131507_adv.csv",
                  sep=",", header=TRUE, na.strings=c("-9999"))

flux2011b <- fread("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_2011/20190806_2/eddypro_JER_2011_fluxnet_2019-08-10T020726_adv.csv",
                  sep=",", header=TRUE, na.strings=c("-9999"))


flux2012a <- fread("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_2012/eddypro_JER_2012_fluxnet_2019-08-08T131951_adv.csv",
                  sep=",", header=TRUE, na.strings=c("-9999"))

flux2012b <- fread("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_2012/20190806_2/eddypro_JER_2012_fluxnet_2019-08-10T022928_adv.csv",
                   sep=",", header=TRUE, na.strings=c("-9999"))


flux2013 <- fread("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_2013/eddypro_JER_2013_fluxnet_2019-08-08T105511_adv.csv",
                  sep=",", header=TRUE, na.strings=c("-9999"))

flux2014 <- fread("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_2014/eddypro_JER_2014_fluxnet_2019-08-07T231114_adv.csv",
                  sep=",", header=TRUE, na.strings=c("-9999"))


flux2015 <- fread("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_2015/20190806/eddypro_JER_2015_fluxnet_2019-08-08T051142_adv.csv",
               sep=",", header=TRUE, na.strings=c("-9999"))

flux2016 <- fread("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_2016/20190806/eddypro_JER_2016_fluxnet_2019-08-08T051828_adv.csv",
               sep=",", header=TRUE, na.strings=c("-9999"))

flux2017 <- fread("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_2017/20190806/eddypro_JER_2017_fluxnet_2019-08-07T114629_adv.csv",
                  sep=",", header=TRUE, na.strings=c("-9999"))

flux2018 <- fread("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_2018/eddypro_JER_2018_fluxnet_2019-08-02T101350_adv.csv",
                  sep=",", header=TRUE, na.strings=c("-9999"))

# combine all individual years of flux runs
flux <- rbind(flux2010a, flux2010b, flux2011a, flux2011b, flux2012a, flux2012b,
              flux2013,flux2014,flux2015, flux2016, flux2017, flux2018)

# format date
flux[,':=' (date_time = parse_date_time(TIMESTAMP_END,"YmdHM",tz="UTC"))][
  ,':='(date=as.Date.POSIXct(date_time),month=month(date_time),year=year(date_time))]

# figure out the missing 2010 data
# read raw file names: what data files are missing?
rawfiles2010a <-  list.files(path="~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_IN_EddyCovariance/2010_2012/2010",
                        full.names=FALSE,pattern="dataL1_ts_")

rawfiles2010aDT <- as.data.table(rawfiles2010a)

colnames(rawfiles2010aDT) <- "files"

rawfiles2010b <-  list.files(path="~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_IN_EddyCovariance/2010_2012/2010/20190908_run",
                            full.names=FALSE,pattern="dataL1_ts_")

rawfiles2010bDT <- as.data.table(rawfiles2010b)

colnames(rawfiles2010bDT) <- "files"

rawfiles2010 <- rbind(rawfiles2010aDT,rawfiles2010bDT)

rawfiles2010[,':=' (filedate = sapply(strsplit(as.character(files),"_"),"[",3),
                    filetime = str_extract(sapply(strsplit(as.character(files),"_"),"[",4),"[0-9]{4}"))][, ':='
                      (date = as.Date(filedate, format="%Y%m%d"),
                        record = 1L)]

# check that the header columns all match for raw flux data
wd_rawa <- "~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_IN_EddyCovariance/2010_2012/2010/"
wd_rawb <- "~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_IN_EddyCovariance/2010_2012/2010/20190908_run"

col.compare<- function(x){t(as.data.table(strsplit(readLines(con=x,
                                                             n=2),","))[,2])}

colnames2010a <- do.call("rbind",lapply(paste(wd_rawa,rawfiles2010a,sep="/"),
                                        col.compare))

colnames2010b <- do.call("rbind",lapply(paste(wd_rawb,rawfiles2010b,sep="/"),
                                        col.compare))



# look at dates with data files created
ggplot(rawfiles2010, aes(date, record))+geom_point(size=0.1)

ggplot(rawfiles2010[month(date)==6,], aes(date, record))+geom_point(size=0.1)

# make some plots
# graph precipitation
ggplot(flux,aes(date_time,P_RAIN_1_1_1))+geom_line()
# graph soil moisture
ggplot(flux,aes(date_time,SWC_1_1_1))+geom_line()

# graph air temp
ggplot(flux,aes(date_time,TA_1_1_1))+geom_line()

# graph PAR
ggplot(flux,aes(date_time,PPFD_IN_1_1_1))+geom_line()

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
ggplot(flux[FC_SSITC_TEST<2,], aes(date_time,FC,colour=(CUSTOM_AGC_MEAN)))+
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
p_flux <- ggplot(flux[filter_fc !=1&month==1&DOY_START<31,],
       aes(DOY_START,FC))+
  geom_point(aes(colour=factor(FC_SSITC_TEST)))+
  geom_line()+
  geom_hline(yintercept=c(-5,5))+
  #ylim(c(-5,5))+
         facet_grid(year~.,scales="free_y")

# p_flux

# make figure of full time series with plotly
p_flux_ts <- ggplot(flux[filter_fc !=1,],
                 aes(date_time,FC,colour=factor(FC_SSITC_TEST)))+
  geom_point()
  #ylim(c(-5,5))

# ggplotly(p_flux_ts)

# 27 Sep 2019: refilter

# Jan: remove qc==1 and flux >5 or flux < -5
# ggplot(flux[DOY_START<31&filter_fc !=1&month==1&
#               !((FC_SSITC_TEST==1&FC>5|FC_SSITC_TEST==1&FC<(-5))),]

# Feb: 


# January:
# 2010 
# 2011 is OK
# 2012: FC>5 | FC<(-5)
# 2013: FC>5 | FC<(-5)
# 2014: check out point FC< (-6)
# 2015: FC>5 | FC<(-5) and check out with PAR/Temp
# 2016: FC>5 | FC<(-5) and check out with PAR/Temp
# 2017: FC>5 | FC<(-5) and check out with PAR/Temp
# 2018: FC>5 | FC<(-5) and check out with PAR/Temp
flux[month==1 & (FC>5 | FC<(-5))& filter_fc!=1 , filter_fc := 2L]

# February: 
# all years remove FC>6 & FC<(-6)
flux[month==2 & (FC>6 | FC<(-6))& filter_fc!=1 , filter_fc := 2L]

# March
# all years exept 2013 (had big uptake spike ~15... check that out)
# remove FC>7 | FC< (-7)
flux[month==3 & year!=2013 & (FC>7 | FC<(-7))& filter_fc!=1 , filter_fc := 2L]

# April
# all years except 2013 (check out lump)
# remove FC>10 | FC< (-10)
flux[month==4 & year!=2013 & (FC>10 | FC<(-10))& filter_fc!=1 , filter_fc := 2L]

# May 
# 2010: ~1 day of data, looks Ok
# 2011: FC>4
# 2012: FC > 6
# 2012: looks OK
# 2013: FC< (-6) | FC>4
# 2014: FC>5
# 2015: FC>6 | FC< (-10)
# 2016: FC>5
# 2017: no data
# 2018: OK
flux[month==5 & year ==2011 & FC>4 & filter_fc!=1 , filter_fc := 2L]
flux[month==5 & year ==2012 & FC>6 & filter_fc!=1 , filter_fc := 2L]
flux[month==5 & year ==2013 & (FC>4 | FC<(-6))& filter_fc!=1 , filter_fc := 2L]
flux[month==5 & year ==2014 & (FC>4)& filter_fc!=1 , filter_fc := 2L]
flux[month==5 & year ==2015 & (FC>6 | FC<(-10))& filter_fc!=1 , filter_fc := 2L]
flux[month==5 & year ==2016 & (FC>5)& filter_fc!=1 , filter_fc := 2L]

# June
# all years remove FC>7 & FC<(-7)
flux[month==6 & (FC>7 | FC<(-7))& filter_fc!=1 , filter_fc := 2L]

# July
# all years remove FC>6 & FC<(-6)
flux[month==7 & (FC>6 | FC<(-6))& filter_fc!=1 , filter_fc := 2L]

# August
# all years remove FC>6 & FC<(-6)
flux[month==8 & (FC>6 | FC<(-6))& filter_fc!=1 , filter_fc := 2L]

# September
# all years remove FC>10 & FC<(-10)
flux[month==9 & (FC>10 | FC<(-10))& filter_fc!=1 , filter_fc := 2L]

# 2013, 2014, 2017: FC>5
flux[month==9 & year %in% c(2013,2014,2017) & (FC>5)& filter_fc!=1 , filter_fc := 2L]

# 2015, 2018: FC>5 | FC< (-5)
flux[month==9 & year %in% c(2015,2018) & (FC>5 | FC<(-5))& filter_fc!=1 , filter_fc := 2L]

# 2016: FC>2.5 | FC<(-4)
flux[month==9 &  year==2016 & (FC>2.5 | FC<(-4))& filter_fc!=1 , filter_fc := 2L]

# October
# 2011: FC>10
#     : DOY_START>297 & DOY_START<298 & FC>4
flux[month==10 &  year==2011 & (FC>10)& filter_fc!=1 , filter_fc := 2L]
flux[month==10 &  year==2011 & DOY_START>297 & DOY_START<298 & FC>4 & filter_fc!=1 , filter_fc := 2L]

# 2012: FC>19
 # spike on DOY 300 looks real.
flux[month==10 &  year==2012 & (FC>19)& filter_fc!=1 , filter_fc := 2L]

# 2013: FC>5 | FC< (-7.5)
flux[month==10 &  year==2013 & (FC>5 | FC<(-7.5))& filter_fc!=1 , filter_fc := 2L]

# 2014: FC < (-7.5)
flux[month==10 &  year==2014 & (FC<(-7.5))& filter_fc!=1 , filter_fc := 2L]

# 2015: FC>5 | FC< (-10)
flux[month==10 &  year==2015 & (FC>5 | FC<(-10))& filter_fc!=1 , filter_fc := 2L]

# 2016: FC>3
flux[month==10 &  year==2016 & (FC>3)& filter_fc!=1 , filter_fc := 2L]

# 2017: FC>5 | FC< (-5)
flux[month==10 &  year==2017 & (FC>5 | FC<(-5))& filter_fc!=1 , filter_fc := 2L]

# 2018 FC>5 | FC < (-4)
flux[month==10 &  year==2018 & (FC>5 | FC<(-4))& filter_fc!=1 , filter_fc := 2L]

# November
# 2011: DOY_START>325.4 & DOY_START<325.7
flux[month==11 &  year==2011 & DOY_START>325.4 & DOY_START<325.7 & filter_fc!=1 , filter_fc := 2L]

# 2012: FC > 5 & FC < -5
flux[month==11 &  year==2012 & (FC>5 | FC<(-5))& filter_fc!=1 , filter_fc := 2L]

# 2013: FC>5 | FC< (-5)
flux[month==11 &  year==2013 & (FC>5 | FC<(-5))& filter_fc!=1 , filter_fc := 2L]

# 2014: FC>5 | FC< (-5)
flux[month==11 &  year==2014 & (FC>5 | FC<(-6))& filter_fc!=1 , filter_fc := 2L]

# 2015: FC>5 | FC< (-5)
flux[month==11 &  year==2015 & (FC>5 | FC<(-6))& filter_fc!=1 , filter_fc := 2L]

# 2016: FC>10 | FC< (-10)
flux[month==11 &  year==2016 & (FC>10 | FC<(-10))& filter_fc!=1 , filter_fc := 2L]

# 2017: FC>10 | FC< (-10)
flux[month==11 &  year==2017 & (FC>10 | FC<(-10))& filter_fc!=1 , filter_fc := 2L]

# 2018 FC>5 | FC < (-6)
flux[month==11 &  year==2018 & (FC>5 | FC<(-6))& filter_fc!=1 , filter_fc := 2L]

# December
# 2011: DOY_START>338 & DOY_START<340 & FC<(-4)
#     : DOY_START>336.7 & DOY_START<337.25
#     : DOY_START>353 & DOY_START<353.75
#     : DOY_START>356.75 & DOY_START<358
flux[month==12 &  year==2011 &DOY_START>338 & DOY_START<340 & FC<(-4)& filter_fc!=1 , filter_fc := 2L]
flux[month==12 &  year==2011 & DOY_START>336.7 & DOY_START<337.25 & filter_fc!=1 , filter_fc := 2L]
flux[month==12 &  year==2011 & DOY_START>353 & DOY_START<353.75 & filter_fc!=1 , filter_fc := 2L]
flux[month==12 &  year==2011 & DOY_START>356.75 & DOY_START<358 & filter_fc!=1 , filter_fc := 2L]

# 2012: DOY>360 & FC>4
# there's a spike near DOY 349, leaving that because there's a similar magnitude uptake spike a few days later
# 
flux[month==12 &  year==2012 & DOY_START>360 & FC>4 & filter_fc!=1 , filter_fc := 2L]

# 2013: FC>10 | FC< (-6)
flux[month==12 &  year==2013 & (FC>10 | FC<(-6))& filter_fc!=1 , filter_fc := 2L]

# 2014: FC>9 | FC< (-10)
flux[month==12 &  year==2014 & (FC>9 | FC<(-10))& filter_fc!=1 , filter_fc := 2L]

# 2015: OK

# 2017: FC>9| FC< (-10)
flux[month==12 &  year==2016 & (FC>9 | FC<(-10))& filter_fc!=1 , filter_fc := 2L]

# 2017: FC>10 |FC< (-6)
flux[month==12 &  year==2017 & (FC>10 | FC<(-6))& filter_fc!=1 , filter_fc := 2L]

# 2018 FC>10 | FC < (-10)
flux[month==12 &  year==2018 & (FC>10 | FC<(-10))& filter_fc!=1 , filter_fc := 2L]

# after time-series look at light and temperature relationships
ggplot(flux[filter_fc !=1 & PPFD_IN_1_1_1>20,],
       aes(PPFD_IN_1_1_1,FC,colour=factor(FC_SSITC_TEST)))+
  geom_point()+
geom_vline(xintercept=0)+
  facet_grid(month~year,scales="free_y")

ggplot(flux[filter_fc !=1 & PPFD_IN_1_1_1<20,],
       aes(TA_1_1_1,FC,colour=factor(FC_SSITC_TEST)))+
  geom_point()+
  geom_vline(xintercept=0)+
  geom_hline(yintercept=-2.5)+
  facet_grid(month~year,scales="free_y")


# also look at rain events coloured by QC code. 
# if there's rain and qc =>1 and the data looks weird on time series or relationships,
# remove
ggplot(flux[month==1&hour(date_time)>0,],
    aes(DOY_START,P_RAIN_1_1_1,colour=factor(FC_SSITC_TEST)))+
     geom_point()+
  facet_grid(year~.,scales="free_y")

ggplot(flux[month==12,],
       aes(DOY_START,P_RAIN_1_1_1,colour=factor(FC_SSITC_TEST)))+
  geom_point()+
  facet_grid(year~.,scales="free_y")

# based on PPFD relationship
# 2011-2012 (until May): OK
# 2013: remove June FC <(-5) & FC_SSITC_TEST==1,
#       remove July FC < (-3) & FC_SSITC_TEST==1, 
#       remove July FC > 5 & FC_SSITC_TEST==1
flux[month==6 &  year==2013 & FC<(-5) & FC_SSITC_TEST==1& filter_fc!=1 , filter_fc := 2L]
flux[month==7 &  year==2013 & (FC<(-3) | FC>5) & FC_SSITC_TEST==1& filter_fc!=1 , filter_fc := 2L]

# 2015: remove January FC < (-3) & FC_SSITC_TEST==1
flux[month==1 &  year==2015 & FC<(-3) & FC_SSITC_TEST==1& filter_fc!=1 , filter_fc := 2L]

# 2016: remove April FC < (-3) & FC_SSITC_TEST==1
flux[month==4 &  year==2016 & FC<(-3) & FC_SSITC_TEST==1& filter_fc!=1 , filter_fc := 2L]

# based on TA relationship
# remove all nighttime FC < -2.5. 
# based on all years these are really unlikely values
flux[PPFD_IN_1_1_1<20 & FC < (-2.5)& filter_fc!=1 , filter_fc := 2L]
# 2010: OK
# 2011: remove March FC >5 & PPFD < 20
flux[month==3 &  year==2011 & FC>(5) & PPFD_IN_1_1_1<20& filter_fc!=1 , filter_fc := 2L]

# 2012: OK
# 2013: remove April DOY_START>98.5 & DOY_START<99.25 (ALL data)
flux[month==4 &  year==2013 & DOY_START>98.5 & DOY_START<99.25& filter_fc!=1 , filter_fc := 2L]

# 2014: OK
# 2015: remove September FC>5
flux[month==9 &  year==2015 & FC>(5)& filter_fc!=1 , filter_fc := 2L]

# 2018: remove August FC>4.5
#       remove November FC>2.5
#       remove December FC < (-5)
flux[month==8 &  year==2018 & FC>(4.5)& filter_fc!=1 , filter_fc := 2L]
flux[month==11 &  year==2018 & FC>(2.5)& filter_fc!=1 , filter_fc := 2L]
flux[month==12 &  year==2018 & FC<(-5)& filter_fc!=1 , filter_fc := 2L]

# a few points remain that are a bit out of pattern but it gets into really fine-tooth combing
# and depending on the analysis and need for the data those points can be removed if they 
# have strong influence on something else. 
# otherwise they likely have little effect. 

ggplot(flux[filter_fc !=1&year==2015&month>4&month<9,],
       aes(DOY_START,FC))+
  geom_line()+
  #ylim(c(-30,30))+
  facet_grid(year~.,scales="free_y")

# look at what got removed with the visual filter, = 2L
ggplot(flux,
       aes(DOY_START,FC))+
  geom_point(aes(colour=factor(filter_fc)))+
  geom_line()+
  #ylim(c(-30,30))+
  facet_grid(year~.,scales="free_y")

ggplot(flux[filter_fc !=1&month==1&DOY_START<31],
       aes(DOY_START,FC))+
  geom_point(aes(colour=factor(filter_fc)))+
  geom_line()+
  #ylim(c(-30,30))+
  facet_grid(year~.,scales="free_y")


ggplot(flux[filter_fc !=1&filter_fc!=2&month==1&DOY_START<31,],
       aes(DOY_START,FC))+
  geom_point(aes(colour=factor(FC_SSITC_TEST)))+
  geom_line()+
  #ylim(c(-30,30))+
  facet_grid(year~.,scales="free_y")

# make a histogram of what got removed with 2L
ggplot(flux,
       aes(FC, fill=factor(filter_fc)))+
         geom_density()+
         facet_grid(.~year)

ggplot(flux[filter_fc!=1,],
       aes(FC, fill=factor(filter_fc)))+
  geom_density(alpha=0.5)+
  facet_grid(.~year)

ggplot(flux[filter_fc==2,],
       aes(FC, fill=factor(FC_SSITC_TEST)))+
  geom_density(alpha=0.5)+
  facet_grid(.~year)


# graph the removed values with 2L
ggplot(flux[filter_fc!=1 & month==12,])+
  geom_point(aes(DOY_START, FC, colour=factor(filter_fc)))+
  geom_line(aes(DOY_START, FC))+
  facet_grid(year~.)

# plot the FC with all 'bad' points removed
ggplot(flux[filter_fc==0 & month==12,])+
     geom_point(aes(DOY_START, FC, colour=factor(filter_fc)))+
     geom_line(aes(DOY_START, FC))+
     facet_grid(year~.)

###### H ######
# look at H  by month for each year
ggplot(flux[filter_H!=1,],
       aes(DOY_START,H,colour=factor(H_SSITC_TEST)))+
  geom_point()+
  #ylim(c(-30,30))+
  facet_grid(year~.,scales="free_y")

# remove QC >1 and AGC > 50
flux[,filter_H := 0L]
# get rid of QC code >2 and low signal strength
flux[H_SSITC_TEST>1 | CUSTOM_AGC_MEAN>50, filter_H := 1L]
# remove H < -120 and > 560, these are not typical values.
flux[H < (-120) | H > 560, filter_H := 1L]
# in 2015 March there's one H > 400 that's an outlier
flux[year==2015 & month==3 & H>400, filter_H := 1L]
# in 2018 Jan there's one H>400 that's an outlier
flux[year==2018 & month==1 & H>400, filter_H := 1L]


# look at all filtered H
ggplot(flux[filter_H!=1,],
       aes(DOY_START,H))+
  geom_line()+
  #ylim(c(-30,30))+
  facet_grid(year~.,scales="free_y")


##### LE ####
# look at LE  by month for each year
ggplot(flux,
       aes(DOY_START,LE,colour=factor(LE_SSITC_TEST)))+
  geom_point()+
  #ylim(c(-30,30))+
  facet_grid(year~.,scales="free_y")

# remove QC >1 and AGC > 50
flux[,filter_LE:= 0L]
# get rid of QC code >2 and low signal strength
flux[LE_SSITC_TEST>1 | CUSTOM_AGC_MEAN>50, filter_LE := 1L]

# look at LE  by month for each year
ggplot(flux[filter_LE!=1&month==1&hour(date_time)>0,],
       aes(DOY_START,LE,colour=factor(LE_SSITC_TEST)))+
  geom_point()+
  geom_hline(yintercept=c(-50,200))+
  facet_grid(year~.,scales="free_y")


# remove LE < (-50) this is a generally outlying value
flux[LE < (-50), filter_LE := 1L]

# remove LE >1000 this is a generally outlying value
flux[LE >1000, filter_LE := 1L]


# 2013: remove DOY_START>98.25 & DOY_START<100
flux[year==2013 & DOY_START>98.25 & DOY_START<100, filter_LE := 1L]

# 2014: remove March LE>500
flux[year==2014 & month==3 & LE>500, filter_LE := 1L]

# 2015: remove July LE > 900
flux[year==2015 & month==7 & LE>900, filter_LE := 1L]

# 2016: remove Novemeber LE > 1000
flux[year==2016 & month==11 & LE>1000, filter_LE := 1L]

# 2017: remove August LE >500
flux[year==2017 & month==8 & LE>500, filter_LE := 1L]

# 2018: remove March LE>300
#       remove August LE > 400
#       remove Novermber LE > 150
flux[year==2018 & month==3 & LE>300, filter_LE := 1L]
flux[year==2018 & month==8 & LE>400, filter_LE := 1L]
flux[year==2018 & month==11 & LE>150, filter_LE := 1L]

# look at all LE
ggplot(flux[filter_LE!=1,],
       aes(DOY_START,LE))+
  geom_line()+
  facet_grid(year~.,scales="free_y")


# energy balance
ggplot(flux[filter_LE!=1 | filter_H!=1,], aes(NETRAD_1_1_1 + (G_1_1_1+G_2_1_1)/2, (H+LE)))+
  geom_point()+
  geom_abline(intercept=0,slope=1)


# plot radiation
ggplot(flux,aes(x=date_time))+
  geom_point(aes(y=SW_OUT_1_1_1, colour="SWout"))+
  geom_line(aes(y=NETRAD_1_1_1, colour="Rn"))+
  geom_point(aes(y=NETRAD_1_1_1, colour="Rn"))+
  geom_line(aes(y=SW_IN_1_1_1, colour="Rg"))+
  geom_point(aes(y=SW_IN_1_1_1, colour="Rg"))+
  geom_line(aes(y=((G_1_1_1+G_1_2_1)/2), colour="SHF"))+
  geom_point(aes(y=((G_1_1_1+G_1_2_1)/2), colour="SHF"))+
  geom_line(aes(y=LW_IN_1_1_1, colour="LWin"))+
  geom_point(aes(y=LW_IN_1_1_1, colour="LWin"))+
  geom_line(aes(y=LW_OUT_1_1_1, colour="LWout"))+
  geom_point(aes(y=LW_OUT_1_1_1, colour="LWout"))

# look at energy balance closure
# first calculate daily sums 

# residual = Rn - G - H - LE
# closure fration = (H + LE) / (Rn + G)
flux[filter_H!=1 | filter_LE!=1,
     ':=' (eb.res=(NETRAD_1_1_1 - ((G_1_1_1+G_2_1_1)/2) - H - LE),
            cf = (H + LE)/(NETRAD_1_1_1 + ((G_1_1_1+G_2_1_1)/2)))]


ggplot(flux[filter_H!=1 | filter_LE!=1,], aes(x=DOY_START))+
 # geom_line(aes(y=eb.res, colour="eb.res"))+
 # geom_point(aes(y=eb.res, colour="eb.res"))+
  geom_line(aes(y=NETRAD_1_1_1, colour="NETRAD_1_1_1"))+
  #geom_point(aes(y=-NETRAD_1_1_1, colour="NETRAD_1_1_1"))+
  geom_line(aes(y=((G_1_1_1+G_1_2_1+G_2_1_1+G_2_2_1)/4), colour="G"))+
 # geom_point(aes(y=((G_1_1_1+G_1_2_1)/2), colour="G"))+
  geom_line(aes(y=H, colour="H"))+
 # geom_point(aes(y=H, colour="H"))+ 
  geom_line(aes(y=LE, colour="LE"))+
  geom_line(aes(y=H+LE+(G_1_1_1+G_1_2_1+G_2_1_1+G_2_2_1)/4, colour="H+LE+G"))+
  #geom_point(aes(y=LE, colour="LE"))
  ylim(c(-1000,1200))+
  facet_grid(.~year)

ggplot(flux, aes(x=date_time))+
  geom_line(aes(y=cf, colour="closure fraction"))+
  geom_point(aes(y=cf, colour="closure fraction"))+
  ylim(c(-100,100))



# heatmap plots, flux cut from 5 to -28
f <-ggplot(flux[!is.na(date_time)&filter_fc!=1,],
           aes(yday(date),hour(date_time)*10+minute(date_time),fill=FC))+
  geom_tile(color= "white",size=0.1) + 
  scale_fill_viridis(name=expression(paste('C',O[2],' flux',sep='')))+
  facet_grid(year~.)+
 scale_y_continuous(breaks=c(0,120,230),
                    labels=c("00:00","12:00","23:00"),
                    expand=c(0,0))+
  scale_x_continuous(breaks =c(31,61,91,121,151,181,211,241,271,301,331,361),limits=c(1,367),
                     labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
                     expand=c(0,0))+
  labs(title= expression("Half-hourly flux (μmol C" *O[2]* m^-2* "se" *c^-1*")"), x="Day", y="Half-hour") +
  theme(
        plot.title=element_text(size = 14),
        axis.text.x=element_text(size=10),
        axis.text.y=element_text(size=8),
        axis.title=element_text(size=11),
        strip.background = element_blank(),
        strip.text=element_text(size=10),
        axis.ticks=element_blank(),
        legend.title=element_text(size=10),
        legend.text=element_text(size=9),
        panel.grid=element_blank(),
        panel.background=element_rect(fill="white"))

f

# time series of rain and temperature
p <- ggplot(flux,aes(date_time,P_rain_1_1_1,colour=factor(year)))+geom_line()

t <- ggplot(flux,aes(date_time,Ta_1_1_1-275.3, colour=factor(year)))+geom_line()

grid.arrange(p,t, nrow=2)

# plot all energy fluxes looking at daily sums
eb_daily <- flux[filter_H!=1 | filter_LE!=1,lapply(.SD,function (x) {sum(x, na.rm=TRUE)}),
                 by="date",
                 .SDcols=c("NETRAD_1_1_1",
                                                      "H", "LE",
                                                      "G_1_1_1", "G_1_2_1",
                                                      "G_2_1_1", "G_2_2_1")]


# residual = Rn - G - H - LE
# closure fration = (H + LE) / (Rn + G)
eb_daily[, ':=' (eb.res=(NETRAD_1_1_1 - ((G_1_1_1+G_2_1_1)/2) - H - LE),
           cf = (H + LE)/(NETRAD_1_1_1 + ((G_1_1_1+G_2_1_1)/2)))]


ggplot(eb_daily[month(date)==12,], aes(x=yday(date)))+
  # geom_line(aes(y=eb.res, colour="eb.res"))+
  # geom_point(aes(y=eb.res, colour="eb.res"))+
  geom_line(aes(y=NETRAD_1_1_1, colour="Rn_1_1_1"))+
  #geom_point(aes(y=-Rn_1_1_1, colour="Rn_1_1_1"))+
  geom_line(aes(y=((G_1_1_1+G_1_2_1+G_2_1_1+G_2_2_1)/4), colour="SHF"))+
  # geom_point(aes(y=((SHF_1_1_1+SHF_1_2_1)/2), colour="SHF"))+
  geom_line(aes(y=H, colour="H"))+
  # geom_point(aes(y=H, colour="H"))+ 
  geom_line(aes(y=LE, colour="LE"))+
  geom_line(aes(y=H+LE+(G_1_1_1+G_1_2_1+G_2_1_1+G_2_2_1)/4, colour="H+LE+SHF"),colour="black")+
  #geom_point(aes(y=LE, colour="LE"))
  facet_grid(year(date)~., scales="free_y")

ggplot(eb_daily[month(date)==12,], aes(x=yday(date)))+
  geom_line(aes(y=NETRAD_1_1_1, colour="Rn_1_1_1"))+
  geom_line(aes(y=H+LE+(G_1_1_1+G_1_2_1+G_2_1_1+G_2_2_1)/4, colour="H+LE+SHF"))+
  facet_grid(year(date)~., scales="free_y")

# daily closure fraction
ggplot(eb_daily, aes(x=date))+
  geom_line(aes(y=cf, colour="closure fraction"))+
  geom_point(aes(y=cf, colour="closure fraction"))+
  ylim(c(-100,100))


### save data for Dawn's LTAR synthesis

# select 2017 & 2018
flux.ltar <- copy(flux[year %in% c(2014,2015,2016,2017,2018),])

# create new columns with only filtered FC, H, LE
flux.ltar[,":=" (FC_unfilter = FC,
                 H_unfilter = H, 
                 LE_unfilter = LE)]

# overwrite the FC, H, LE columns to contain only filtered data
flux.ltar[,":="(FC = NULL, 
                H = NULL, 
                LE = NULL)]

flux.ltar[filter_fc !=1 , FC := FC_unfilter]
flux.ltar[filter_H !=1 , H := H_unfilter]
flux.ltar[filter_LE !=1 , LE := LE_unfilter]

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

ggplot(flux.ltar.long,aes(parse_date_time(TIMESTAMP_END,"YmdHM",tz="UTC"), value))+
  geom_line()+
  facet_grid(variable~.,scales="free_y")

# save
setwd("~/Desktop/TweedieLab/Projects/Jornada/LTAR_Synthesis_Browning")

# 20190827 version has 2014-2018 data.
# write.table(flux.ltar1, file="FluxData_jerbajada_20190827.csv",
# sep=",", dec=".",row.names=FALSE, na="-9999", quote=FALSE)

# metadata file is created in Jornada_EddyPro_Output.R because that is able to take units from the full output files
# I manually modified and renamed the metadata file because the output here has _1_1_1 in all the biomet variables
## write.table(description.ltar, file="FluxData_jerbajada_METADATA_20190813.csv",sep=",", dec=".", row.names=FALSE)

# save all filtered data for ReddyProc: ustar filter & gap-fill
setwd("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_EddyPro_filtered")

save(flux,file="JER_flux_2010_2018_EddyPro_Output_filterID_20190929.Rdata")
flux_filter <- copy(flux)
flux_filter[filter_fc!=0, FC := NA]
flux_filter[filter_H!=0, H := NA]
flux_filter[filter_LE!=0, LE := NA]

save(flux_filter,
     file="JER_flux_2010_2018_EddyPro_Output_filtered_20190929.Rdata")

