###############################################
# Get the most recent data to QA/QC and save  #
#          Tower Climate Data                 #
#      written by: M. Mauritz                 #
#            date: 7 October 2019             #
###############################################



# load required libraries
library(data.table)
library(ggplot2)
library(gridExtra)
library(lubridate)
library(plotly)

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

# import most recent file
climate.loggerinfo <-fread("/Volumes/SEL_Data_Archive/Research Data/Desert/Jornada/Bahada/Tower/TowerClimate_met/2019/Raw_Data/ASCII/dataL1_met_2019.csv",
                         header = FALSE, sep=",", skip = 0,fill=TRUE,
                         na.strings=c(-9999,"#NAME?"))[1,]

climate.colnames <-fread("/Volumes/SEL_Data_Archive/Research Data/Desert/Jornada/Bahada/Tower/TowerClimate_met/2019/Raw_Data/ASCII/dataL1_met_2019.csv",
                                     header = TRUE, sep=",", skip = 1,fill=TRUE,
                                     na.strings=c(-9999,"#NAME?"))[1:2,]

climate <- fread("/Volumes/SEL_Data_Archive/Research Data/Desert/Jornada/Bahada/Tower/TowerClimate_met/2019/Raw_Data/ASCII/dataL1_met_2019.csv",
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


# create derivative date columns
climate[,':=' (year = year(date_time), doy = yday(date_time), date = date(date_time))]

# select the date on and after which you want to see the data
date_select <- as.POSIXct("2019-01-01 00:00:00", ("%Y-%m-%d %H:%M:%S"), tz="UTC")

climate <- climate[date_time >= date_select,]

# make figures to check data and remove outliers if necessary
ggplot(climate, aes(date_time, airtemp))+geom_line()
ggplot(climate, aes(date_time, rh))+geom_line()
ggplot(climate, aes(date_time, e))+geom_line()
ggplot(climate, aes(date_time, atm_press))+geom_line()
ggplot(climate, aes(date_time, wnd_spd))+geom_line()
ggplot(climate, aes(date_time, wnd_dir))+geom_line()
fig.precip <- ggplot(climate, aes(date_time, precip))+geom_line()
fig.precip
fig.lws <- ggplot(climate, aes(date_time, lws_5m))+geom_line()
fig.lws 
grid.arrange(fig.precip, fig.lws, nrow=2)

ggplot(climate, aes(date_time, par))+geom_line()
ggplot(climate, aes(date_time, albedo))+geom_line()

ggplot(climate)+
  geom_line(aes(date_time, net_rs, colour="net_rs"))+
  geom_line(aes(date_time, net_ri, colour="net_rl"))
  

ggplot(climate)+
  geom_line(aes(date_time, up_tot, colour="up facing"))+
  geom_line(aes(date_time, dn_tot, colour="down facing"))


# save to QAQC folder on data archive
enddate <- (min(climate$date_time))
enddate <- (max(climate$date_time))

# return column names to original:
climate.save <- copy(climate[,.(timestamp,record,airtemp,rh,e,
                                atm_press,wnd_spd,wnd_dir,
                                precip,par,albedo,
                                lws_5m,net_rs,net_ri,up_tot,dn_tot,
                                co2_raw,h2o_raw)])

colnames(climate.save) <- colnames(climate.colnames)
colnames(climate.loggerinfo) <- colnames(climate.colnames)

# add information on the data logger, column names, units in the same format as the raw datafile
climate.save <- rbind(climate.loggerinfo,climate.colnames, climate.save)

# save in QAQC folder with start and end date in the file name
# write.table(climate.save,
#   paste("/Volumes/SEL_Data_Archive/Research Data/Desert/Jornada/Bahada/Tower/TowerClimate_met/2019/QAQC/",
#         "dataL2_met_",year(startdate),sprintf("%02d",(month(startdate))),sprintf("%02d",(day(startdate))),
#                       sprintf("%02d",(hour(startdate))),sprintf("%02d",(minute(startdate))),
#                       sprintf("%02d",(second(startdate))),
#         "_",
#         year(enddate),sprintf("%02d",(month(enddate))),sprintf("%02d",(day(enddate))),
#         sprintf("%02d",(hour(enddate))),sprintf("%02d",(minute(enddate))),
#          sprintf("%02d",(second(enddate))), ".csv",sep=""),
#   sep=",", dec=".", row.names=FALSE, col.names=FALSE)

