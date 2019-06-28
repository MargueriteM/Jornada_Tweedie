#################################################
# This code processes Climate Data from JER     #
#                                               #
# Data come from:                               #
#                * Met Tower                    #
#  Get data from SEL Drive, view time series,   #
# identify sensor gaps, remove bad data         #
#                                               #
#    written by: Marguerite Mauritz             #
#                13 March, 2019                 #
#################################################

# load libraries
library(data.table)
library(ggplot2)
library(lubridate)
library(gridExtra)
library(lattice)


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
# lws_2 (leaf wetness): mV  THIS IS 'leaf' wetness at 5m; lws_1 is in the Flux_Table data and measures in a shrub
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
met_all <- do.call("rbind", lapply(metfiles, header = FALSE, fread, sep=",", skip = 4,fill=TRUE,
                                   na.strings=c(-9999,"#NAME?"),
                                   col.names=c("timestamp","record","airtemp","rh","e",
                                               "atm_press","wnd_spd","wnd_dir",
                                               "precip","par","albedo",
                                               "lws_5m","net_rs","net_ri","up_tot","dn_tot",
                                               "co2_raw","h2o_raw")))

# convert the time stamp to a posixct format
met_all[,date_time := parse_date_time(timestamp, c("%m-%d-%y %H:%M","%m-%d-%y %H:%M:%S",
                                                   "%Y!-%m-%d %H:%M:%S"))]


# create derivative date columns
met_all[,':=' (year = year(date_time), doy = yday(date_time), date = date(date_time))]

# check data
# summary(met_all)

# calculate half-hourly data (data are already in full time-series for all minutes of a year)
# 1440 min/day
# leap year: 525600 minutes/year
# non-leap year: 52740 minutes/year

# several NA options: 
# if any 1 min is NA in the 30 minute interval, mean = NA
# ignore all NA and calcualte mean, regardless of how many NA/30min
# more complex: use some NA threshold to get 30min (eg: no more than 10% NA = 3 NAs)

# decision: calculate with and without NA, use the NA removed values unless they are really off and then gap-fill
# missing values would be filled either way, with straight interpolation, so may as well use some of the 1 min data
# count NAs so that this can be checked if there's a very strange value. 

# at each step, convert timestamp pack to posixct, force to :00 and :30 by subtraction 1 miunute (60s), 

# leave out Co2_raw and H2O_raw, those will be in the eddy processed data
met_30min_rmna <- met_all[,lapply(.SD, function (x) {mean(x, na.rm=TRUE)}), 
                          .SDcols = c("airtemp","rh","e","atm_press","wnd_spd","wnd_dir","par","albedo",
                                      "lws_5m","net_rs","net_ri","up_tot","dn_tot"),
                         by=ceiling_date(date_time,"30 min")]
# keep one dataframe with original column names
met_30min_rmna_use <- copy(met_30min_rmna)
# merge one to means calculated with NA removed to compare data
colnames(met_30min_rmna) <- paste(colnames(met_30min_rmna), 'mean.na', sep=".")
# return date_time to POSIXct format
met_30min_rmna[,date_time := (ymd_hms(ceiling_date.mean.na))][,ceiling_date.mean.na := NULL]
met_30min_rmna_use[,date_time := (ymd_hms(ceiling_date))][,ceiling_date := NULL]

# now calculte with NA included and count NAs
# find numbers of NA in 30min intervals
# from: https://stackoverflow.com/questions/37142181/how-to-change-few-column-names-in-a-data-table

met_all_30min <- met_all[, as.list(unlist(lapply(.SD,
                                      function(x) list(mean=mean(x, na.rm=FALSE), na=sum(is.na(x)))))),
              by=ceiling_date(date_time,"30 min"),
              .SDcols = c("airtemp","rh","e","atm_press","wnd_spd","wnd_dir","precip","par","albedo",
                          "lws_5m","net_rs","net_ri","up_tot","dn_tot")][,date_time := (ymd_hms(ceiling_date))][,ceiling_date := NULL]


# calculate sum of precip separately, and then merge to other data. 
# for precip don't use na.rm=TRUE because otherwise it will look like we captured all precip when we actually didn't
# although - comparing precip with and without NA doesn't appear to make much difference
# that means sensor failure rarely occured during a rain event, if the sensor was already working/recording
precip_tot_30min <- met_all[,list(precip.tot = sum(precip)), 
                         by=ceiling_date(date_time,"30 min")][,date_time := (ymd_hms(ceiling_date))][,ceiling_date := NULL]

# data to rest effect of removing or including NA
met30 <- merge(met_all_30min, met_30min_rmna, by="date_time")
met30 <- merge(met30, precip_tot_30min, by="date_time")
# data to use
met30_use <- merge(met_30min_rmna_use, precip_tot_30min, by="date_time")

# create year, date, and doy columns
met30[,':=' (year=year(date_time), month=month(date_time), date=as_date(date_time), doy=yday(date_time))]

# keep the data with 30mins mean, removing NA: met30_use
met30_long <- melt.data.table(met30_use,c("date_time"))

met30_long[,':=' (year=year(date_time),month=month(date_time),doy=yday(date_time))]

# data to filter: 
# airtemp, RH, e, press, precip, are good



# ALL:
# remove >Oct 6 2017 12:00 and < Nov 3 2017 12:30
# remove > Dec 1 2017 and < Apr 3 2018 7:30
met30_long[(date_time > as.POSIXct("2017-10-06 12:00", tz="UTC") & date_time < as.POSIXct("2017-11-03 12:30", tz="UTC")) |
             (date_time > as.POSIXct("2017-12-01 00:00", tz="UTC") & date_time < as.POSIXct("2018-04-03 7:30", tz="UTC"))  , 
           value := NA]

# lws_5m, par, wnd_dir
# remove > Oct 20 2015 12:00 to <Nov 6 2015 12:30 (flat-line)
met30_long[(date_time > as.POSIXct("2015-10-20 12:00", tz="UTC") &
              date_time < as.POSIXct("2015-10-06 12:30", tz="UTC")) &
             variable %in% c("lws_5m","par","wnd_dir"), 
           value := NA]

# albedo
# remove values < -300 and > 300
met30_long[variable=="albedo" & (value <(-300) | value > 300), value := NA]
# albedo remove > March 17 2017 and < June 1 2011
met30_long[variable=="albedo"&date_time>as.Date("2011-03-17")&
             date_time<as.POSIXct("2011-06-17 12:30", tz="UTC"), value := NA]
# albedo remove > Feb 1 2012 and < Mar 8 2012 (flat-line)
met30_long[variable=="albedo"&date_time>as.Date("2012-02-01")&
             date_time<as.Date("2012-03-08"), value := NA]
# albedo remove >Oct 20 2015 and < Nov 7 2015 (flat-line)
met30_long[variable=="albedo"&date_time>as.Date("2015-10-20")&
             date_time<as.Date("2015-11-07"), value := NA]

# lws_5m. There are some high spike values, not sure what those mean (rain?). Leave them in.
# remove lws_5m prior to Aug 25 2011
met30_long[variable=="lws_5m"&date_time<as.Date("2011-08-25"), value := NA]

# lws_5m remove > Feb 1 2012 and < Mar 8 2012 (flat-line)
met30_long[variable=="lws_5m"&date_time>as.Date("2012-02-01")&
             date_time<as.Date("2012-03-08"), value := NA]

# par remove > Feb 1 2012 and < Mar 8 2012 (flat-line)
met30_long[variable=="par"&date_time>as.Date("2012-02-01")&
             date_time<as.Date("2012-03-08"), value := NA]

# par remove >Oct 20 2015 and < Nov 7 2015 (flat-line)
met30_long[variable=="par"&date_time>as.Date("2015-10-20")&
             date_time<as.Date("2015-11-07"), value := NA]


# wnd_dir remove > Feb 1 2012 and < Mar 8 2012 (flat-line)
met30_long[variable=="wnd_dir"&date_time>as.Date("2012-02-01")&
             date_time<as.Date("2012-03-08"), value := NA]
# wind_dir remove >Oct 20 2015 and < Nov 7 2015 (flat-line)
met30_long[variable=="wnd_dir"&date_time>as.Date("2015-10-20")&
             date_time<as.Date("2015-11-07"), value := NA]


# net_rs
# remove values < -25 and > 1000
met30_long[variable=="net_rs"&(value<(-25)|value>1000), value := NA]

# net_rs remove > Feb 1 2012 and < Mar 8 2012 (flat-line)
met30_long[variable=="net_rs"&date_time>as.Date("2012-02-01")&
             date_time<as.Date("2012-03-08"), value := NA]
# net_rs remove >Oct 20 2015 and < Nov 7 2015 (flat-line)
met30_long[variable=="net_rs"&date_time>as.Date("2015-10-20")&
             date_time<as.Date("2015-11-07"), value := NA]

# net_ri
# remove values > 100
met30_long[variable=="net_ri"&value>100, value := NA]

# net_ri remove > Feb 1 2012 and < Mar 8 2012 (flat-line)
met30_long[variable=="net_ri"&date_time>as.Date("2012-02-01")&
             date_time<as.Date("2012-03-08"), value := NA]
# net_ri remove >Oct 20 2015 and < Nov 7 2015 (flat-line)
met30_long[variable=="net_ri"&date_time>as.Date("2015-10-20")&
             date_time<as.Date("2015-11-07"), value := NA]


# up_tot
# remove value >1500
met30_long[variable=="up_tot" & value>1500, value := NA]
# up_tot remove > Feb 1 2012 and < Mar 8 2012 (flat-line)
met30_long[variable=="up_tot"&date_time>as.Date("2012-02-01")&
             date_time<as.Date("2012-03-08"), value := NA]
# up_tot remove >Oct 20 2015 and < Nov 7 2015 (flat-line)
met30_long[variable=="up_tot"&date_time>as.Date("2015-10-20")&
             date_time<as.Date("2015-11-07"), value := NA]


# dn_tot
# remove < -900 and > 350
met30_long[variable=="dn_tot" & (value< (-900) | value > 350), value := NA]
# dn_tot remove > Feb 1 2012 and < Mar 8 2012 (flat-line)
met30_long[variable=="dn_tot"&date_time>as.Date("2012-02-01")&
             date_time<as.Date("2012-03-08"), value := NA]
# dn_tot remove >Oct 20 2015 and < Nov 7 2015 (flat-line)
met30_long[variable=="dn_tot"&date_time>as.Date("2015-10-20")&
             date_time<as.Date("2015-11-07"), value := NA]

# up_tot, dn_tot, net_ri, net_rs, albedo
# remove >August 20 2012 and < August 25 12:00 (flat-line)
met30_long[variable%in% c("up_tot","dn_tot","net_ri","net_rs","par","albedo")&
             date_time>as.POSIXct("2012-08-20 12:00", tx="UTC")&
             date_time<as.POSIXct("2012-08-25 12:00", tz="UTC"), value := NA]

# if up or dn is NA then albedo and net are also NA
dn_tot_na <- copy(met30_long[variable == "dn_tot" & is.na(value), (date_time)])
up_tot_na <- copy(met30_long[variable == "up_tot" & is.na(value), (date_time)])

met30_long[date_time %in% dn_tot_na & variable %in% c("albedo","net_rs"),
           value := NA]

met30_long[date_time %in% up_tot_na & variable %in% c("albedo","net_rs"),
           value := NA]

# check filtered data
ggplot(met30_long[year==2019,], aes(date_time, value))+geom_point()+facet_grid(variable~.,scales="free_y")

ggplot(met30_long[variable=="airtemp"], aes(date_time, value))+geom_point()
ggplot(met30_long[variable=="rh"], aes(date_time, value))+geom_point()
ggplot(met30_long[variable=="e"], aes(date_time, value))+geom_point()
ggplot(met30_long[variable=="atm_press"], aes(date_time, value))+geom_point()
ggplot(met30_long[variable=="wnd_spd"], aes(date_time, value))+geom_point()
ggplot(met30_long[variable=="wnd_dir"], aes(date_time, value))+geom_point()
ggplot(met30_long[variable=="precip.tot"], aes(date_time, value))+geom_point()
ggplot(met30_long[variable=="par"], aes(date_time, value))+geom_point()
ggplot(met30_long[variable=="albedo"], aes(date_time, value))+geom_point()
ggplot(met30_long[variable=="lws_5m"], aes(date_time, value))+geom_point()
ggplot(met30_long[variable=="net_rs"], aes(date_time, value))+geom_point()
ggplot(met30_long[variable=="net_ri"], aes(date_time, value))+geom_point()
ggplot(met30_long[variable=="up_tot"], aes(date_time, value))+geom_point()
ggplot(met30_long[variable=="dn_tot"], aes(date_time, value))+geom_point()

# do a bunch of graphic checks on the data: 
# airtemp, rh, e, atm_press, wnd_spd, wnd_dir, precip.tot, par, albedo, lws_5m,
# net_rs, net_ri, up_tot, dn_tot
# columns with mean.na have the NA removed!

# airtemp
ggplot(met30, aes(x=date_time))+
  geom_line(aes(y=airtemp.mean.na), colour="black")+ # NA removed
  geom_line(aes(y=airtemp.mean), colour="red") # with NA
  facet_grid(year~.)
# rh
ggplot(met30, aes(x=date_time))+
  geom_line(aes(y=rh.mean.na), colour="black")+ # NA removed
  geom_line(aes(y=rh.mean), colour="red") # with NA
  facet_grid(year~.)
# e
ggplot(met30, aes(x=date_time))+
  geom_line(aes(y=e.mean.na), colour="black")+ # NA removed
  geom_line(aes(y=e.mean), colour="red") # with NA
  facet_grid(year~.)
# atm_press
ggplot(met30, aes(x=date_time))+
  geom_line(aes(y=atm_press.mean.na), colour="black")+ # NA removed
  geom_line(aes(y=atm_press.mean), colour="red") # with NA
  facet_grid(year~.)
# wnd_spd
ggplot(met30, aes(x=date_time))+
  geom_line(aes(y=wnd_spd.mean.na), colour="black")+ # NA removed
  geom_line(aes(y=wnd_spd.mean), colour="red") # with NA
  facet_grid(year~.)
# wnd_dir
ggplot(met30, aes(x=date_time))+
  geom_line(aes(y=wnd_dir.mean.na), colour="black")+ # NA removed
  geom_line(aes(y=wnd_dir.mean), colour="red") # with NA
  facet_grid(year~.)
# precip.tot
ggplot(met30, aes(x=date_time))+
  geom_line(aes(y=precip.tot), colour="red") # with NA
  facet_grid(year~.)
# par
ggplot(met30, aes(x=date_time))+
  geom_line(aes(y=par.mean.na), colour="black")+ # NA removed
  geom_line(aes(y=par.mean), colour="red") # with NA
  facet_grid(year~.)
# albedo
ggplot(met30[albedo.mean.na<24&albedo.mean.na>(-25),], aes(x=date_time))+
  geom_line(aes(y=albedo.mean.na), colour="black")+ # NA removed
  geom_line(aes(y=albedo.mean), colour="red") # with NA
  facet_grid(year~.)
# lws_5m
ggplot(met30[lws_5m.mean>250&date_time>as.Date("2011-08-25"),], aes(x=date_time))+
  geom_line(aes(y=lws_5m.mean.na), colour="black")+ # NA removed
  geom_line(aes(y=lws_5m.mean), colour="red") # with NA
  facet_grid(year~.)
# net_rs
ggplot(met30[net_rs.mean.na<1000&net_rs.mean.na>(-25),], aes(x=date_time))+
  geom_line(aes(y=net_rs.mean.na), colour="black")+ # NA removed
  geom_line(aes(y=net_rs.mean), colour="red") # with NA
  facet_grid(year~.)
# net_ri
ggplot(met30[net_ri.mean.na<100&year>2010&year<=2012,], aes(x=date_time))+
  geom_line(aes(y=net_ri.mean.na), colour="black")+ # NA removed
  geom_line(aes(y=net_ri.mean), colour="red") # with NA
  facet_grid(year~.)
# up_tot
ggplot(met30[up_tot.mean.na<1500,], aes(x=date_time))+
  geom_line(aes(y=up_tot.mean.na), colour="black")+ # NA removed
  geom_line(aes(y=up_tot.mean), colour="red") # with NA
  facet_grid(year~.)
# dn_tot
ggplot(met30[dn_tot.mean.na>(-1000)&dn_tot.mean.na<350,], aes(x=date_time))+
  geom_line(aes(y=dn_tot.mean.na), colour="black")+ # NA removed
  geom_line(aes(y=dn_tot.mean), colour="red") # with NA
  facet_grid(year~.)


# data FOR ANTHONY: 
# Precip and Air Temp
# 30-min Dec 2018 - March 2019
# keep mean.na columns (NA removed during mean calculation)

# airtemp
ggplot(met30[date_time >= as.Date("2018-12-01") & date_time <= as.Date("2019-03-31"),],
       aes(x=date_time))+
  geom_line(aes(y=airtemp.mean.na), colour="black")+ # NA removed
  geom_line(aes(y=airtemp.mean), colour="red") # with NA

# precip.tot
ggplot(met30[date_time >= as.Date("2018-12-01") & date_time <= as.Date("2019-03-31"),],
       aes(x=date_time))+
  geom_line(aes(y=precip.tot), colour="red") # with NA

# lws_5m
ggplot(met30[lws_5m.mean.na<1000 & lws_5m.mean.na>200 & 
               date_time >= as.Date("2018-12-01") & date_time <= as.Date("2019-03-31"),],
       aes(x=date_time))+
  geom_line(aes(y=lws_5m.mean.na), colour="black")+ # NA removed
  geom_line(aes(y=lws_5m.mean), colour="red") # with NA


# check leaf-wetness against precip, for Dec 2018 to Mar 2019
precip.2018.fig <- ggplot(met30[date_time >= as.Date("2018-12-01") & date_time <= as.Date("2019-03-31"),],
                          aes(x=date_time))+
  geom_line(aes(y=precip.tot), colour="red") # with NA
  
lws_5m.2018.fig <- ggplot(met30[date_time >= as.Date("2018-12-01") & date_time <= as.Date("2019-03-31"),],
                       aes(x=date_time))+
  geom_line(aes(y=lws_5m.mean.na), colour="black")+ # NA removed
  geom_line(aes(y=lws_5m.mean), colour="red") # with NA

grid.arrange(precip.2018.fig,lws_5m.2018.fig, nrow=2)

# save Dec 2018 to end of March 2019 data for Anthony
setwd("~/Desktop/TweedieLab/Projects/Jornada/Anthony_soilCO2_fluxes")
# write.csv(met30[ date_time >= as.Date("2018-12-01") & date_time <= as.Date("2019-03-31"),.(
# date_time,date,year,doy,airtemp.mean.na,rh.mean.na,e.mean.na,atm_press.mean.na,
# wnd_spd.mean.na,wnd_dir.mean.na,par.mean.na,albedo.mean.na,lws_5m.mean.na,
# net_rs.mean.na,net_ri.mean.na,up_tot.mean.na,dn_tot.mean.na,precip.tot.na)],
# file='SEL_JER_MetData_20181201_20190331_20190423.csv',
#           row.names=FALSE)

# write.csv(met30[ date_time >= as.Date("2018-12-01") & date_time <= as.Date("2019-03-31"),.(
# date_time,date,year,doy,airtemp.mean.na,rh.mean.na,e.mean.na,atm_press.mean.na,
# wnd_spd.mean.na,wnd_dir.mean.na,par.mean.na,albedo.mean.na,lws_5m.mean.na,
# net_rs.mean.na,net_ri.mean.na,up_tot.mean.na,dn_tot.mean.na,precip.tot)],
# file='SEL_JER_MetData_20181201_20190331_20190508.csv',
#           row.names=FALSE)

# save 30min filtered data
# save half hour means
setwd("~/Desktop/TweedieLab/Projects/Jornada/Data/Tower/Climate/Compiled")
## write.table(met30_long, file="TowerMet_L1_2010_2019_30min.csv", sep=",", row.names = FALSE)
# save with ceiling_date
# write.table(met30_long, file="TowerMet_L1_2010_2019_30min_20190627.csv", sep=",", row.names = FALSE)


# calculate daily precip
met_daily1 <- met_all[,list(precip_daily_mean = mean(precip),
                            precip_daily_tot = sum(precip),
                            temp_daily = mean(airtemp),
                            year = unique(year),
                            doy=unique(doy)), 
                      by="date"]
# calculate daily cumulative 
met_precip_cum <- met_daily1[!is.na(precip_daily_tot),list(precip_cum = cumsum(precip_daily_tot),
                                                       date = unique(date)),
                             by="year"]

# merge daily mean and cumulative data
met_daily <- merge(met_daily1, met_precip_cum[,.(date,precip_cum)], by=c("date"),all.x=TRUE)

# graph precip and temperature
ggplot(met_daily, aes(date, precip_daily_tot))+geom_point()
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
       aes(date_time, lws_5m))+geom_point()+
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
       aes(doy, lws_5m))+geom_line()+
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
