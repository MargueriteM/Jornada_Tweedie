#################################################
# This code processes Climate Data from JER     #
#                                               #
# Data come from:                               #
#                * Met Tower                    #
#                * Sensor Network               #
#  Get data from SEL Drive, view time series,   #
# identify sensor gaps, combine tower and SN    #
# to get full time series                       #
#                                               #
#    written by: Marguerite Mauritz             #
#                13 March, 2019                 #
#################################################

# Jornada Precip Data: import and explore

library(data.table)
library(ggplot2)
library(lubridate)


# copied files from server to computer
# server source: "/Volumes/SEL_Data_Archive/Research Data/Desert/Jornada/UtepJrnBahadaSite/Tower/TowerClimate/"

# WARNING: 2010, 2011, 2012 files have #NAME in some cells. I think mainly for albedo.
# read.table can't handle that. read.csv is better, fread is MUCH faster

# 2011 has some breaks in the columns where data is missing and for some reason is not recording -9999


# Units of data:
# timestamp: mm/dd/yyyy HH:MM:SS
# t_hmp (air temperature): Celsius
# rh_hmp (relative humidity): percent
# e_hmp (absolute humidity): kPa
# atm_press (atmospheric pressure): kPa
# hor_wnd_spd (horizontal wind speed): m/s
# hor_wnd_dir (horizontal wind direction): degrees
# precip_tot (total precipitation): mm
# par (photosynthetically active radiation): umol/m/s
# albedo: unitless
# lws_2 (leaf wetness): mV
# NetRs (net solar radiation): W/m2
# NetRI (net radiation): W/m2
# UpTot (total upwelling): W/m2
# DnTot (total downwelling): W/m2
# CO2_raw: mmol/m3
# H2O_raw: mmol/m3

# met2010 <- read.table(file="/Volumes/SEL_Data_Archive/Research Data/Desert/Jornada/UtepJrnBahadaSite/Tower/TowerClimate/2010/Raw_asci/dataL1_met_2010.csv",
#                       sep=",",dec=".",na.strings=c(-9999))

setwd("~/Desktop/TweedieLab/Projects/Jornada/Data/Tower/Climate/")

# fread imports as data table
# list all files in relevant folder
metfiles <- list.files(path="~/Desktop/TweedieLab/Projects/Jornada/Data/Tower/Climate/RawFiles", full.names=TRUE) 
# read files and bind them into one file. fill=TRUE because of the missing columns in 2011
met_all <- do.call("rbind", lapply(metfiles, header = TRUE, fread, sep=",", skip = 4,fill=TRUE,
                                   na.strings=c(-9999,"#NAME?"),
                                   col.names=c("timestamp","record","airtemp","rh","e",
                                               "atm_press","wnd_spd","wnd_dir",
                                               "precip","par","albedo",
                                               "lws","net_rs","net_ri","up_tot","dn_tot",
                                               "co2_raw","h2o_raw")))

# convert the time stamp to a posixct format
met_all[,date_time := parse_date_time(timestamp, c("%m-%d-%y %H:%M","%m-%d-%y %H:%M:%S",
                                                   "%Y!-%m-%d %H:%M:%S"))]


# create derivative date columns
met_all[,':=' (year = year(date_time), doy = yday(date_time), date = date(date_time))]

summary(met_all)

# calculate daily precip
met_daily1 <- met_all[,list(precip_daily = mean(precip),
                            temp_daily = mean(airtemp),
                            year = unique(year),
                            doy=unique(doy)), 
                      by="date"]
# calculate daily cumulative 
met_precip_cum <- met_daily1[!is.na(precip_daily),list(precip_cum = cumsum(precip_daily),
                                                       date = unique(date)),
                             by="year"]

# merge daily mean and cumulative data
met_daily <- merge(met_daily1, met_precip_cum[,.(date,precip_cum)], by=c("date"),all.x=TRUE)

# graph precip and temperature
ggplot(met_daily, aes(date, precip_daily))+geom_point()
ggplot(met_daily, aes(date, precip_cum))+geom_line()
ggplot(met_daily, aes(date, temp_daily))+geom_line()+geom_hline(yintercept=0)

# graph only 2015
ggplot(met_all[year==2015,], aes(date_time, precip))+geom_point()

# graph only from June to August 2015
ggplot(met_all[date_time>as.Date("2015-06-01") & date_time<as.Date("2015-08-01"),],
       aes(date_time, precip))+geom_point()+
  facet_grid(.~year)

# graph leaf wetness from June to August 2015
ggplot(met_all[date_time>as.Date("2015-06-01") & date_time<as.Date("2015-08-01"),],
       aes(date_time, lws))+geom_point()+
  facet_grid(.~year)

# graph RH from June to August 2015
ggplot(met_all[date_time>as.Date("2015-06-01") & date_time<as.Date("2015-08-01"),],
       aes(date_time, rh))+geom_point()+
  facet_grid(.~year)

# graph June/July of every year for lab meeting

# precip
ggplot(met_all[doy>180 & doy<240,],
       aes(doy, precip))+geom_line()+
  facet_grid(.~year)+
  theme_bw()

# graph leaf wetness 
ggplot(met_all[doy>180 & doy<240,],
       aes(doy, lws))+geom_line()+
  facet_grid(.~year)+
  theme_bw()

# graph RH 
ggplot(met_all[doy>180 & doy<240,],
       aes(doy, rh))+geom_line()+
  facet_grid(.~year)+
  theme_bw()

# save the merged data
# setwd("~/Desktop/TweedieLab/Projects/Jornada/Data/Tower/Climate/Compiled")
# write.csv(met_daily[,.(year,date,doy,precip_daily,
#                       precip_cum,temp_daily)], file='Daily_Precip_AirTemp_Mean_Cumulative_2010_2016_PRELIMINARY_20190314.csv',
#          row.names=FALSE)
