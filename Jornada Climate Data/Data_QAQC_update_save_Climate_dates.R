###############################################
# Get the most recent data to QA/QC and save  #
#          Tower Climate Data                 #
#      written by: M. Mauritz                 #
#            date: 7 October 2019             #
###############################################

# This code will:
# 1. allow data to be checked
# 2. run standard range filters determined from 2010-2019 and input from Ameriflux
# 3. allow year-specific data removal based on visual checks
# 4. save data with date/time in file name of TowerClimate_met/year/QAQC folder on server
# 5. save a full year of data to TowerClimate_met/Combined with only year in filename
# 6. save a csv file of this code as Data_QAQC_Code_yyyy.csv (or with date/time) to TowerClimate_met/year/QAQC folder to record data filter steps


# load required libraries
library(data.table)
library(ggplot2)
library(gridExtra)
library(lubridate)
library(plotly)


# get windrose function, source directly from Github
source(paste0("https://raw.githubusercontent.com/MargueriteM/R_functions/master/plot.windrose.R"))

## windrose function on Marguerite's local computer, in case internet doesn't work
# source("~/Desktop/R/R_programs/Functions/plot.windrose.R")


# CLIMATE DATA

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

year_file <- 2022

# Based on data checks, no data form Met and CS650 from 16 Dec 17:30 to 17 Jan 2022

# import most recent file
climate.loggerinfo <-fread(paste("/Volumes/SEL_Data_Archive/Research Data/Desert/Jornada/Bahada/Tower/TowerClimate_met/",year_file,"/Raw_Data/ASCII/dataL1_met_",year_file,".csv",sep=""),
                         header = FALSE, sep=",", skip = 0,fill=TRUE,
                         na.strings=c(-9999,"#NAME?"))[1,]

climate.colnames <-fread(paste("/Volumes/SEL_Data_Archive/Research Data/Desert/Jornada/Bahada/Tower/TowerClimate_met/",year_file,"/Raw_Data/ASCII/dataL1_met_",year_file,".csv",sep=""),
                                     header = TRUE, sep=",", skip = 1,fill=TRUE,
                                     na.strings=c(-9999,"#NAME?"))[1:2,]

climate <- fread(paste("/Volumes/SEL_Data_Archive/Research Data/Desert/Jornada/Bahada/Tower/TowerClimate_met/",year_file,"/Raw_Data/ASCII/dataL1_met_",year_file,".csv",sep=""),
                 header = FALSE, sep=",", skip = 4,fill=TRUE,
                 na.strings=c(-9999,"#NAME?"),
                 col.names=c("timestamp","record","airtemp","rh","e",
                             "atm_press","wnd_spd","wnd_dir",
                             "precip","par","albedo",
                             "lws_5m","net_rs","net_ri","up_tot","dn_tot",
                             "co2_raw","h2o_raw"))
# convert the time stamp to a posixct format
climate[,date_time := parse_date_time(timestamp, c("%m-%d-%y %H:%M","%m-%d-%y %H:%M:%S",
                                                   "%Y!-%m-%d %H:%M:%S"))]

# calculate 30 minute data, drop any NAs in the 30min interval and calculate the mean
# leave out Co2_raw and H2O_raw, those will be in the eddy processed data
climate_30min <- climate[,lapply(.SD, function (x) {mean(x, na.rm=TRUE)}), 
                          .SDcols = c("airtemp","rh","e","atm_press","wnd_spd","wnd_dir","par","albedo",
                                      "lws_5m","net_rs","net_ri","up_tot","dn_tot"),
                          by=ceiling_date(date_time,"30 min")]

# calculate sum of precip separately, and then merge to other data. 
# for precip don't use na.rm=TRUE because otherwise it will look like we captured all precip when we actually didn't
# although - comparing precip with and without NA doesn't appear to make much difference
# that means sensor failure rarely occured during a rain event, if the sensor was already working/recording
precip_tot_30min <- climate[,list(precip_tot = sum(precip)), 
                            by=ceiling_date(date_time,"30 min")][,date_time := (ymd_hms(ceiling_date))][,ceiling_date := NULL]


# create derivative date columns
climate_30min[,':=' (date_time=ceiling_date,
                     year = year(ceiling_date),
                     doy = yday(ceiling_date),
                     date = date(ceiling_date))]

# merge with precipitation data
climate_30min <- merge(climate_30min, precip_tot_30min, by="date_time")


# check file start and end date
startdate.check <- (min(climate_30min$date_time))
enddate.check <- (max(climate_30min$date_time))


# select the date on and after which you want to see the data
date_select <- as.POSIXct("2022-01-01 00:00:00", ("%Y-%m-%d %H:%M:%S"), tz="UTC")

climate_30min <- climate_30min[date_time >= date_select,]

# make figures to check data and remove outliers if necessary
ggplot(climate_30min, aes(date_time, airtemp))+geom_line()
ggplot(climate_30min, aes(date_time, rh))+geom_line()
ggplot(climate_30min, aes(date_time, e))+geom_line()
ggplot(climate_30min, aes(date_time, atm_press))+geom_line()
ggplot(climate_30min, aes(date_time, wnd_spd))+geom_line()
ggplot(climate_30min, aes(date_time, wnd_dir))+geom_line()
climate_wind <- copy(climate_30min[!is.na(wnd_dir) | !is.na(wnd_spd),])
plot.windrose(climate_wind,climate_wind$wnd_spd, climate_wind$wnd_dir)

fig.precip <- ggplot(climate_30min, aes(date_time, precip_tot))+geom_line()+scale_x_datetime(date_breaks="1 month", date_labels="%b")+labs(title="Bajada US-Jo1 Rainfall, 2021", y="Total 30 min Rainfall (mm)")
fig.lws <- ggplot(climate_30min, aes(date_time, lws_5m))+geom_line()

# Precip: check patterns with LWS to see if events are misssing.
# LWS and Rain should have similar appearance of spikes
grid.arrange(fig.precip, fig.lws, nrow=2)

ggplot(climate_30min, aes(date_time, par))+geom_line()
ggplot(climate_30min, aes(date_time, albedo))+geom_line()

# selected net Rs and Rl
#ggplot(climate_30min[date_time>as.Date("2021-10-29") & date_time<as.Date("2021-10-30"),])+
#  geom_line(aes(date_time, net_rs, colour="net_rs"))+
#  geom_line(aes(date_time, net_ri, colour="net_rl"))

# all net Rs and Rl  
ggplot(climate_30min)+
  geom_line(aes(date_time, net_rs, colour="net_rs"))+
  geom_line(aes(date_time, net_ri, colour="net_rl"))


ggplot(climate_30min)+
  geom_line(aes(date_time, up_tot, colour="up facing"))+
  geom_line(aes(date_time, dn_tot, colour="down facing"))


# QA/QC steps: 

# make data long for QA/QC
# keep the data with 30mins mean, removing NA
met30_long <- melt.data.table(climate_30min,c("date_time","ceiling_date","year","doy","date"))

# Precip: check patterns with LWS to see if events are misssing.
# LWS and Rain should have similar appearance of spikes
grid.arrange(fig.precip, fig.lws, nrow=2)


# Ameriflux QA/QC: rescale LWS to 0-100
# lws 5m make same adjustments as with lws in shrub in FluxTable
# remove values <250
met30_long[variable %in% c("lws_5m") & value<250, value := NA]
# remove values >375 since this is a very common max value and the SN LWS sensors often appear to max out
met30_long[variable %in% c("lws_5m") & value>375, value := NA]

# find min and max vvalue to rescale
lws.min <- min(met30_long[variable %in% c("lws_5m") & !is.na(value),]$value)
lws.max <- max(met30_long[variable %in% c("lws_5m") & !is.na(value),]$value)

# min val: 250; max val = 375 (based on 2010-2019 and range cut-off do not recalculate for each year)

# RESCALE from 0 to 100
met30_long[variable %in% c("lws_5m"), value := ((value-250)/(375-250))*100]

grid.arrange(fig.precip,
             ggplot(met30_long[variable=="lws_5m"], aes(date_time, value))+geom_point()+labs(title="rescale lws_5m"))


# albedo
# remove values < -300 and > 300
met30_long[variable=="albedo" & (value <(-300) | value > 300), value := NA]
ggplot(met30_long[variable=="albedo"], aes(date_time, value))+geom_point()+labs(title="albedo")

# net_rs
# remove values < -25 and > 1000
met30_long[variable=="net_rs"&(value<(-25)|value>1000), value := NA]

# net_rl
# remove values > 100
met30_long[variable=="net_ri"&value>100, value := NA]

ggplot(met30_long[variable%in% c("net_rs", "net_ri")], aes(date_time, value, colour=variable))+geom_point()+
  labs(title="net_rs and nt_rl")

# up_tot
# remove value >1500
met30_long[variable=="up_tot" & value>1500, value := NA]# dn_tot
# remove < -900 and > 350
met30_long[variable=="dn_tot" & (value< (-900) | value > 350), value := NA]

ggplot(met30_long[variable%in% c("up_tot", "dn_tot")], aes(date_time, value, colour=variable))+geom_point()+
  labs(title="Total up and down")

# 2021 Net Rl and up_tot is bad from 27 Aug 2021 to 29 Oct 2021 19:30 due to broken upward looking Rl sensor						
# met30_long[variable%in% c("net_ri","up_tot") &
#             (date_time > as.POSIXct("2021-08-27 00:00", tz="UTC") &
#                date_time < as.POSIXct("2021-10-29 19:30", tz="UTC")),
#           value := NA]

ggplot(met30_long[variable%in% c("net_rs", "net_ri")], aes(date_time, value, colour=variable))+geom_point()+
  labs(title="net_rs and net_rl")

# 2022 looks good up to "2022-06-01 11:30:00 UTC"

# if up or dn is NA then albedo and net are also NA
dn_tot_na <- copy(met30_long[variable == "dn_tot" & is.na(value), (date_time)])
up_tot_na <- copy(met30_long[variable == "up_tot" & is.na(value), (date_time)])

met30_long[date_time %in% dn_tot_na & variable %in% c("albedo","net_rs"),
           value := NA]

met30_long[date_time %in% up_tot_na & variable %in% c("albedo","net_rs"),
           value := NA]

# save to QAQC folder on data archive
startdate <- (min(met30_long$date_time))
enddate <- (max(met30_long$date_time))

# make data wide again
climate.save <- data.table:: dcast(met30_long[!is.na(date_time),.(date_time, variable,value)],
                                      date_time~variable,
                                      value.var="value")

setnames(climate.save,c("date_time","airtemp","rh","e",
                           "atm_press","wnd_spd","wnd_dir","par","albedo","lws_5m","net_rs","net_ri","up_tot","dn_tot","precip_tot"),
         c("timestamp","t_hmp","rh_hmp","e_hmp","atm_press","hor_wnd_spd","hor_wnd_dir","par",
           "albedo","lws_2","NetRs","Net_Rl","UpTot","DnTot","precip_tot"))


 # add information on the data logger, column names, units in the same format as the raw datafile
 # climate.save <- rbind(climate.loggerinfo,climate.colnames, climate.save)
 
# # save in QAQC folder with start and end date in the file name
qaqc.path<- paste("/Volumes/SEL_Data_Archive/Research Data/Desert/Jornada/Bahada/Tower/TowerClimate_met/",year_file,"/QAQC/", sep="")
setwd(qaqc.path)

############ write with specific date and time ############
# write.table(climate.save,
#   paste("dataL2_met_",year(startdate),sprintf("%02d",(month(startdate))),sprintf("%02d",(day(startdate))),
#                       sprintf("%02d",(hour(startdate))),sprintf("%02d",(minute(startdate))),
#                       sprintf("%02d",(second(startdate))),
#         "_",
#         year(enddate),sprintf("%02d",(month(enddate))),sprintf("%02d",(day(enddate))),
#         sprintf("%02d",(hour(enddate))),sprintf("%02d",(minute(enddate))),
#          sprintf("%02d",(second(enddate))), ".csv",sep=""),
#   sep=",", dec=".", row.names=FALSE)
############################################################

# Save to Qa/QC and Combined folder with only year name
# QAQC folder
write.table(climate.save,
            paste("dataL2_met_",year_file, ".csv",sep=""),
            sep=",", dec=".", row.names=FALSE)

# save a text file that says date that code was run (system time), start and end date of data
run.info <- data.frame(info=c("Data_start","Data_end","Date_processed"),
                       date_time=c(startdate,enddate,ymd_hms(Sys.time(),tz="UTC")))

write.table(run.info, "dataL2_met_DateRange.csv",
            sep=",", dec=".", row.names=FALSE)

# combined folder
setwd("/Volumes/SEL_Data_Archive/Research Data/Desert/Jornada/Bahada/Tower/TowerClimate_met/Combined")

write.table(climate.save,
            paste("dataL2_met_",year_file, ".csv",sep=""),
            sep=",", dec=".", row.names=FALSE)


# save the R script that went along with creating the file to have a record of QA/QC
# use rstudioapi to get the path of the current script and then copy it to the 
# server location

# http://theautomatic.net/2018/07/11/manipulate-files-r/ 
# file.copy("source_file.txt", "destination_folder")

# file.rename(from = rstudioapi::getActiveDocumentContext()$path,
#            # to = file.path("/Volumes/SEL_Data_Archive/Research Data/Desert/Jornada/Bahada/Tower/TowerClimate_met/2019/QAQC/",
#            to = file.path("~/Desktop",                
#            paste("Data_QAQC_update_save_Climate_",year(startdate),sprintf("%02d",(month(startdate))),sprintf("%02d",(day(startdate))),
#                                  sprintf("%02d",(hour(startdate))),sprintf("%02d",(minute(startdate))),
#                                 sprintf("%02d",(second(startdate))),
#                                 "_",
#                                  year(enddate),sprintf("%02d",(month(enddate))),sprintf("%02d",(day(enddate))),
#                                  sprintf("%02d",(hour(enddate))),sprintf("%02d",(minute(enddate))),
#                                 sprintf("%02d",(second(enddate))), ".csv",sep="")))

file.copy(from = rstudioapi::getActiveDocumentContext()$path,
             to = file.path(qaqc.path,
            #to = file.path("~/Desktop",                
                           paste("Data_QAQC_Code_",year_file, ".csv",sep="")))
# If response: [TRUE] the code save worked. If [FALSE], the file already exists. Remove and run again. 

