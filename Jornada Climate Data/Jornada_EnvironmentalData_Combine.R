###########################################
#     Jornada Environmental Data          #
# combine 30min data from Met Tower,      #
# Sensor networks, soil sensors.          #
# This code brings together multiple      #
# years of data with obvious sensor       #
# failure and values out of range removed.#
# For each data stream, multiple years    #
# are combined and averaged to 30mins in  #
# seperate R codes.                       #
#                                         #
# Also create file for Eddy Pro Biomet    #
#                                         #
#     written by: M. Mauritz              #
#          April 2019                     #
###########################################

# 30 Jan 2020: added information to biomet preparation (create biomet2) that retains more sensor info. Merge this to the post-EppyPro data
# https://ameriflux.lbl.gov/data/aboutdata/data-variables/ 

# biomet gets used for EddyPro processing and is a reduced and averaged variable list
# biomet2 will be merged to the EddyPro output for ameriflux after SD and precip event filtering!!

# TO DO: 
# fixed tower precip for SN to be sum of 5 min data (on 2019-05-08)
# depths for ECTM?! And cover type -> probably will have to dig up probes.
# lws units in tower and SN..... ?
# check tower PAR data filter.... eg: in 2013 and 2018 (check all), did I delete something wrong? many midday are missing
# PAR check in tower: FOUND ERROR (2019-08-05).
# get heatflux data and lws_1 from flux data tables
# fix depth so it can be numeric

# play with figure output. 

# Data:
# Path: ⁨Macintosh HD⁩ ▸ ⁨Users⁩ ▸ ⁨memauritz⁩ ▸ ⁨Desktop⁩ ▸ ⁨R⁩ ▸ ⁨R_programs⁩ ▸ ⁨Tweedie⁩ ▸ ⁨Jornada⁩ ▸ ⁨Jornada Climate Dat
# Tower Data: Jornada_Climate_Data.R
# Sensor Network Data: Jornada_SensorNetworkData_2010_2019.R
# Soil Sensor Data: Jornada_ECTM_Data.R
# Flux table: Jornada_FluxTable_Data.R (for soil heat flux plates, lws_1, Rl_in, Rl_out, Rs_in, Rs_out)

# very obviously bad sensor data was removed from the individual data streams.

####
# Sensor Network Data Units & Distances:
####
# Units of data:
# rain, mm
# pressure, mbar
# leaf wetness (lws), no unit
# par, uE
# solar radiation (solar), Wm-2
# soil moisture (moisture), m-3/m-3
# battery, V
# voltage, V
# current, mA

# Sensor network distances on tramline
# SN1: 104m
# SN2: 87.5m
# SN3: 64m
# SN4: 24m
# SN5: 36m
# SN6: 22m
# SN7: 79.5m
# SN8: 3m

#####
# Met Tower Climate Data Units
####
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

#####
# FluxTable Data Units
#####
# Units of data:
# I think depths on HFP are switched (assuming that open/bush labels are correct, I think it's:)
# hfp01_1_Avg	W/m^2 Soil heat flux (5cm/15cm) 15 open ... (channel 3H. Field label: 10O)
# hfp01_2_Avg	W/m^2 Soil heat flux (5cm/15cm) 10 open ... (channel 6H. Field label: 15O)
# hfp01_3_Avg	W/m^2 Soil heat flux (5cm/15cm) 15 bush ... (channel 10H. Field label: 15B)
# hfp01_4_Avg	W/m^2 Soil heat flux (5cm/15cm) 10 bush ... (channel 8L (=16 SE). Field label: 10B)
# lws_1_Avg	mV (in shrub)
# Rs_downwell_Avg	W/m^2
# Rs_upwell_Avg	W/m^2
# Rl_downwell_Avg	W/m^2
# Rl_upwell_Avg	W/m^2

######## NRCS Solar Radiation Data
# import hourly data from NRCS for gapfilling SW In (Rg in EddyPro) for 2010 and 2011
# recored in MST (no daylight savings)
# https://wcc.sc.egov.usda.gov/nwcc/site
# Solar Radiation 400 to 1100 nm range in W/m2

###### Potential incoming Short Wave Radiation
# data provided by Ameriflux, calculated SW potential incoming for the site

# load libraries
library(ggplot2) # library for making figures in ggplot package
library(lubridate) # library for easier date manipulation 
library(data.table) # library for data table which is more efficient with large data sets

#############
# IMPORT DATA
#############
# Sensor network data:
setwd("~/Desktop/TweedieLab/Projects/Jornada/Data/SensorNetwork/Combined")
SN_30min <- fread("SensorNetwork_L1_2010_20200213_30min.csv", sep=",", header=TRUE)
# format date and add column to deginate the data stream
SN_30min[, ':=' (date_time = ymd_hms(date_time), datastream = "SN", location = "SN")]
setnames(SN_30min, 'sensor', 'variable')
# change variable names to match other datastreams
SN_30min[variable == "rain", variable := "precip.tot"]
SN_30min[variable == "moisture", variable := "soilmoisture"]
SN_30min[variable == "pressure", variable := "atm_press"]

# make depth in SN negative to indicate 'below the surface'
SN_30min[variable=="soilmoisture" & depth=="5", height:="-5"]
SN_30min[variable=="soilmoisture" & depth=="10", height:="-10"]
SN_30min[variable=="soilmoisture" & depth=="20", height:="-20"]
SN_30min[variable=="soilmoisture" & depth=="30", height:="-30"]

# convert pressure from mbar to kPa (1mbar = 0.1kPa)
SN_30min[variable=="atm_press", mean.val := mean.val/10]

# Tower Met Data
setwd("~/Desktop/TweedieLab/Projects/Jornada/Data/Tower/Climate/Compiled")
met_30min <- fread("TowerMet_L1_2010_20200112_30min.csv", sep=",", header=TRUE)
# format date and add column to deginate the data stream
met_30min[, ':=' (date_time = ymd_hms(date_time), datastream = "climate",location = "tower")]
setnames(met_30min, 'value', 'mean.val')

met_30min[variable=="par", veg := "UP"]
# lws in met is at 5m
met_30min[variable %in% c("lws","airtemp"), ':=' (veg = "BARE", height = "500")]
met_30min[variable=="precip.tot", veg := "BARE"]
 
 
# Data from FluxTable: Rs, Rl, HFP, LWS_1 (in shrub)
setwd("~/Desktop/TweedieLab/Projects/Jornada/Data/Tower/Flux/Compiled_forJoining")
flux_30min <- fread("FluxTable_L1_2010_20200112_30min.csv", sep=",", header=TRUE)
flux_30min[, ':=' (date_time = ymd_hms(date_time), year=year(date_time),datastream = "flux", location = "tower")]
setnames(flux_30min, 'value', 'mean.val')

flux_30min[variable %in% c("lws_1_Avg"), ':=' (veg = "SHRUB", height = "50", variable = "lws")]
flux_30min[variable %in% c("hfp01_1_Avg","hfp01_3_Avg"), ':=' (height = "-15")]
flux_30min[variable %in% c("hfp01_2_Avg","hfp01_4_Avg"), ':=' (height = "-10")]
flux_30min[variable %in% c("hfp01_1_Avg","hfp01_2_Avg"), ':=' (veg = "BARE", variable = "hfp")]
flux_30min[variable %in% c("hfp01_3_Avg","hfp01_4_Avg"), ':=' (veg = "SHRUB", variable = "hfp")]
flux_30min[variable == "Rs_downwell_Avg", variable := "Rs_down"]
flux_30min[variable == "Rs_upwell_Avg", variable := "Rs_up"]
flux_30min[variable == "Rl_downwell_Avg", variable := "Rl_down"]
flux_30min[variable == "Rl_upwell_Avg", variable := "Rl_up"]

# Tower soil temperature and moisture data (ECTM)
setwd("~/Desktop/TweedieLab/Projects/Jornada/Data/SoilSensor_ECTM/Combined")
soil_30min <- fread("Soil_Temp_VWC_ECTM_L1_2010_20200112_30min.csv", sep=",", header=TRUE)
# format date and add column to deginate the data stream, get rid or uneccessary columns
setnames(soil_30min, c('value','variable'), c('mean.val', 'probe_id'))
soil_30min[measurement == "t", variable := "soiltemp"]
soil_30min[measurement == "vwc", variable := "soilmoisture"]

# make a guess at depths
soil_30min[rep%in% c(8,3), height := "-2"]
soil_30min[rep%in% c(1,6), height := "-10"]
soil_30min[rep%in% c(5,7), height := "-15"]
soil_30min[rep%in% c(2,4), height := "-20"]

# assign veg type (should be shrub/bare, once I know.)
# for now assign aribtratryt
soil_30min[rep %in% c(1,3,7,4), veg := "BARE"]
soil_30min[rep %in% c(2,5,6,8), veg := "SHRUB"]

# modify columns to match other datastreams and get rid of redundant ones
soil_30min[, ':=' (date_time = ymd_hms(date_time), datastream = "ectm",location = "tower",
                   diff=NULL, diff2=NULL, date=NULL, rep=NULL, measurement=NULL)]


# import hourly data from NRCS for gapfilling SW In (Rg in EddyPro) for 2010 and 2011
# https://wcc.sc.egov.usda.gov/nwcc/site
# Solar Radiation 400 to 1100 nm range in W/m2
nrcs.2010 <- fread("~/Desktop/TweedieLab/Projects/Jornada/Data/Tower/Climate/External_GapfillData/NRCS_JER_Site2168_SolarRad_2010.csv",
                   sep=",", header=TRUE, col.names=c("siteID","Date","Time","mean.val"), na.strings=c(-99.9,"NA"))
nrcs.2011 <- fread("~/Desktop/TweedieLab/Projects/Jornada/Data/Tower/Climate/External_GapfillData/NRCS_JER_Site2168_SolarRad_2011.csv",
                   sep=",", header=TRUE, col.names=c("siteID","Date","Time","mean.val"),na.strings=c(-99.9,"NA"))

nrcs.2012 <- fread("~/Desktop/TweedieLab/Projects/Jornada/Data/Tower/Climate/External_GapfillData/NRCS_JER_Site2168_SolarRad_2012.csv",
                   sep=",", header=TRUE, col.names=c("siteID","Date","Time","mean.val"), na.strings=c(-99.9,"NA"))
nrcs.2013 <- fread("~/Desktop/TweedieLab/Projects/Jornada/Data/Tower/Climate/External_GapfillData/NRCS_JER_Site2168_SolarRad_2013.csv",
                   sep=",", header=TRUE, col.names=c("siteID","Date","Time","mean.val"),na.strings=c(-99.9,"NA"))
nrcs.2014 <- fread("~/Desktop/TweedieLab/Projects/Jornada/Data/Tower/Climate/External_GapfillData/NRCS_JER_Site2168_SolarRad_2014.csv",
                   sep=",", header=TRUE, col.names=c("siteID","Date","Time","mean.val"), na.strings=c(-99.9,"NA"))
nrcs.2015 <- fread("~/Desktop/TweedieLab/Projects/Jornada/Data/Tower/Climate/External_GapfillData/NRCS_JER_Site2168_SolarRad_2015.csv",
                   sep=",", header=TRUE, col.names=c("siteID","Date","Time","mean.val"),na.strings=c(-99.9,"NA"))
nrcs.2016 <- fread("~/Desktop/TweedieLab/Projects/Jornada/Data/Tower/Climate/External_GapfillData/NRCS_JER_Site2168_SolarRad_2016.csv",
                   sep=",", header=TRUE, col.names=c("siteID","Date","Time","mean.val"), na.strings=c(-99.9,"NA"))
nrcs.2017 <- fread("~/Desktop/TweedieLab/Projects/Jornada/Data/Tower/Climate/External_GapfillData/NRCS_JER_Site2168_SolarRad_2017.csv",
                   sep=",", header=TRUE, col.names=c("siteID","Date","Time","mean.val"),na.strings=c(-99.9,"NA"))
nrcs.2018 <- fread("~/Desktop/TweedieLab/Projects/Jornada/Data/Tower/Climate/External_GapfillData/NRCS_JER_Site2168_SolarRad_2018.csv",
                   sep=",", header=TRUE, col.names=c("siteID","Date","Time","mean.val"), na.strings=c(-99.9,"NA"))
nrcs.2019 <- fread("~/Desktop/TweedieLab/Projects/Jornada/Data/Tower/Climate/External_GapfillData/NRCS_JER_Site2168_SolarRad_2019.csv",
                   sep=",", header=TRUE, col.names=c("siteID","Date","Time","mean.val"),na.strings=c(-99.9,"NA"))


nrcs <- rbind(nrcs.2010, nrcs.2011,nrcs.2012,nrcs.2013,nrcs.2014,nrcs.2015,nrcs.2016,nrcs.2017,nrcs.2018,nrcs.2019)
rm(nrcs.2010, nrcs.2011,nrcs.2012,nrcs.2013,nrcs.2014,nrcs.2015,nrcs.2016,nrcs.2017,nrcs.2018,nrcs.2019)

# format date & time
nrcs[,date_time := ymd_hm(paste(as.Date(Date, format="%m/%d/%y"), Time, sep=" "))]

# format to match env_30min
nrcs[,':=' (year = year(date_time),
            month = month(date_time),
            doy = yday(date_time),
            veg = "UP",
            variable = "solar",
            location = "nrcs",
            datastream = "nrcs",
            siteID = NULL,
            Date = NULL, 
            Time= NULL)]

# # look at nrcs data in 2 month intervals. Looks OK
# ggplot(nrcs[date_time>=as.Date("2011-11-01") & date_time<=as.Date("2011-12-31")], aes(date_time, mean.val,colour=location))+
#   geom_line()+
#   facet_grid(datastream~.)

# combine all three SEL data streams and 2010, 2011 NRCS data
env_30min <- rbind(SN_30min,met_30min,flux_30min,soil_30min, nrcs, fill=TRUE)

# some datastreams don't have all the time stamp columns. Create
env_30min[,doy:=yday(date_time)]

# check levels of variable column
levels(factor(env_30min$variable))

# check the levels of height and order them
levels(factor(env_30min$height))

env_30min[,height := factor(height,levels=c("-30","-20","-15","-10","-5","-2","50","500"))]

# create a variable to show data coverage =1 with data 
env_30min[!is.na(mean.val), coverage := 1]




# Precip: The rain buckets record 0 even if they are broken and not picking up rain
#         As a result, averaging across all sensors will underestimate rain if one bucket is not working
# If there's rain recorded in any bucket and another is 0, make it NA instead of 0
precip <- copy(env_30min[variable=="precip.tot"&veg=="BARE"&!is.na(date_time),.(date_time,
                                                              SN,mean.val)])

precip[is.na(SN), SN := "tower"]

precip <- dcast(precip,date_time~SN)

## check
#ggplot(precip,aes(date_time,tower))+geom_line()
#ggplot(precip,aes(date_time,SN2))+geom_line()
#ggplot(precip,aes(date_time,SN6))+geom_line()

#ggplot(precip,aes(tower,SN2))+geom_point()
#ggplot(precip,aes(tower,SN6))+geom_point()
#ggplot(precip,aes(SN2,SN6))+geom_point()

# if any row has 0 in one column and >0 in another, make the 0 = NA
precip[SN2==0 & (SN6!=0 | tower!=0), SN2 := NA]
precip[SN6==0 & (SN2!=0 | tower!=0), SN6 := NA]
precip[tower==0 & (SN2!=0 | SN6!=0), tower := NA]

# now join the fixed precip data back to the env_30min
precip <- melt(precip,measure.vars=c("tower","SN2","SN6"), variable.name="SN",value.name="mean.val")

# ggplot(precip, aes(date_time, mean.val, colour=SN))+geom_line()

precip_extra <- copy(env_30min[variable=="precip.tot"&veg=="BARE"&!is.na(date_time),
                               .(variable,datastream,location,probe_id,veg,height,depth,
                                 date_time,SN,year,month,doy,
                                 coverage)])
precip[SN=="tower",SN := NA]

precip <- merge(precip, precip_extra, by=c("SN","date_time"))

# replace Bare precip in env_30min
env_30min1 <- copy(env_30min) 
rm(env_30min)
env_30min <- copy(env_30min1[!(variable=="precip.tot"&veg=="BARE"),])

env_30min <- rbind(env_30min, precip)

# create data for Eddy Pro Biomet
# airtemp, rh, atm_press, par, wnd_spd, wnd_dir, precip.tot, Rn_nr, Rl_downwell, Rl_upwell,
# Rs_downwell, Rs_upwell, hfp, soilmoisture

# look at soil moisture and temperature
ggplot(env_30min[variable %in% c("soilmoisture","soiltemp"),],
       aes(date_time, mean.val,colour=veg, linetype=height))+
  geom_line()+
  facet_grid(paste(variable,location,sep="_")~., scales="free_y")

# look at PAR UP from tower and SN
ggplot(env_30min[variable == "par"& veg %in% c("UP"),], aes(date_time, mean.val,colour=SN))+
  geom_line()+
  facet_grid(paste(variable,location,sep="_")~., scales="free_y")

# look at PAR over Veg from tower and SN
ggplot(env_30min[variable == "par"& veg %in% c("LATR","PRGL","DAPU","MUPO","BARE"),],
       aes(date_time, mean.val,colour=SN))+
  geom_line()+
  facet_grid(paste(variable,veg,sep="_")~., scales="free_y")


# look at precip from tower and SN
ggplot(env_30min[variable == "precip.tot"& veg=="BARE",], aes(date_time, mean.val,colour=datastream))+
  geom_point()+
  facet_grid(paste(variable,location,veg,sep="_")~., scales="free_y")

# look at heat flux plate data from tower
ggplot(env_30min[variable == "hfp",], aes(date_time, mean.val,colour=veg))+
  geom_line()+
  facet_grid(paste(variable,location,height,sep="_")~., scales="free_y")

# look at pressure from the Sn and tower
ggplot(env_30min[variable == "atm_press",], aes(date_time, mean.val,colour=location))+
      geom_line()+
      facet_grid(paste(variable,location,sep="_")~., scales="free_y")




# look at PAR from tower and SN
ggplot(env_30min[(yday(date_time)>=303 & yday(date_time)<=313) &
                   ((variable %in% c("par") & veg=="UP")),],
       aes(hour(date_time), mean.val, colour=paste(variable,location)))+
  geom_line()+
  geom_vline(xintercept=c(6.5,17.0))+
  facet_grid(year~doy)

# find mismatch by looking through all data (no tower Rs data until 2011 doy 194: 13 July)
# 2014 no SN solar data
# 2017 doy 69-233 no tower Rs data
# 2017 doy 185 NRCS data ends
# 2017 doy 335 to 2018 doy 93 no tower Rs data
# 2018 doy 70 to doy 250 no SN solar data
# 2018 doy 325 - 2019 doy 137 no SN solar data
# 1 - 55
# 56 - 110
# 111 - 166
# 167 - 222
# 223 - 278
# 279 - 334
# 335 - 366  & location!="nrcs"
ggplot(env_30min[year==2011 & (yday(date_time)>=279 & yday(date_time)<=334) &
                   ((variable %in% c("solar") & veg=="UP" )| variable %in% c("Rs_up")),],
       aes(hour(date_time), mean.val, colour=paste(variable,location)))+
  geom_line()+
  geom_point(size=0.5)+
  geom_vline(xintercept=c(6.5,17.0))+
  facet_wrap(doy~.)


View(env_30min[year==2013 & (yday(date_time)==214) &
                 (variable %in% c("Rs_up"))])


# look at solar radition from SN and NRCS, with Rs_up
# graph all the intervals where a timestamp mismatch and rejoin happens

# assume that NRCS time is always correct (Standard Time) and mark sunrise/sunset
env_30min[location=="nrcs"&mean.val<=5,nrcs_day:=-5]
env_30min[location=="nrcs"&mean.val>5,nrcs_day:=1000]


# prior to Jul 2011 look at PAR becaus there's no Rs for the tower 
# 2010 all match

# off: 80 15:00-15:30 2011 (tower: +1)
 ggplot(env_30min[year==2011 & yday(date_time)>=75 & yday(date_time)<=85&
                             ((variable %in% c("par") & veg=="UP") | variable %in% c("solar") & location=="nrcs"),],
             aes(hour(date_time), mean.val, colour=location))+
     geom_line()+
     facet_grid(year~doy)

# off: 194 18:30 2011 (tower: +1) 
# together: 316 2011 11:30-12:00 (tower to +0)
ggplot(env_30min[year==2011 & (yday(date_time)>=190 & yday(date_time)<=196 | yday(date_time)>=315 & yday(date_time)<=317) &
                   ((variable %in% c("solar") & veg=="UP") | variable %in% c("Rs_up")),],
       aes(hour(date_time), mean.val, colour=location))+
  geom_line()+
  facet_grid(year~doy)

ggplot(env_30min[year==2011 & (yday(date_time)==194) &
                   ((variable %in% c("solar") & veg=="UP") | variable %in% c("Rs_up")),],
       aes(hour(date_time), mean.val, colour=location))+
  geom_line()+
  facet_grid(year~doy)

# off: 138 16:00-16:30 2012 (tower: +1) 
# together: 11 2013 12:30-13:00 (tower to +0)
ggplot(env_30min[((year==2012 & yday(date_time)>=137 & yday(date_time)<=139) |
                    (year==2013 & yday(date_time)>=9 & yday(date_time)<=13)) &
                   ((variable %in% c("solar") & veg=="UP") | variable %in% c("Rs_up")),],
       aes(hour(date_time), mean.val, colour=location))+
  geom_line()+
  facet_grid(year~doy)


ggplot(env_30min[((year==2012 & yday(date_time)==138)) &
                   ((variable %in% c("solar") & veg=="UP") | variable %in% c("Rs_up")),],
       aes(hour(date_time), mean.val, colour=location))+
  geom_line()+
  facet_grid(year~doy)

# off: 95 2013 6:00 (SN: +1) 
# together: 214 2013 12:30-13:30 (tower + 1)
ggplot(env_30min[((year==2013 & yday(date_time)>=92 & yday(date_time)<=97) |
                    (year==2013 & yday(date_time)>=212 & yday(date_time)<=218)) &
                   ((variable %in% c("solar") & veg=="UP") | variable %in% c("Rs_up")),],
       aes(hour(date_time), mean.val, colour=location))+
  geom_line()+
  geom_line(aes(hour(date_time),nrcs_day, colour=location))+
  facet_grid(year~doy)

ggplot(env_30min[((year==2013 & yday(date_time)==95)) &
                   ((variable %in% c("solar") & veg=="UP") | variable %in% c("Rs_up")),],
       aes(hour(date_time), mean.val, colour=location))+
  geom_line()+
  geom_line(aes(hour(date_time),nrcs_day, colour=location))+
  facet_grid(year~doy)

# off: 335 2013 7:00 (tower: +1) 
# together: 91 2015 6:00 (SN: + 1)
ggplot(env_30min[((year==2013 & yday(date_time)>=332 & yday(date_time)<=337) |
                    (year==2015 & yday(date_time)>=87 & yday(date_time)<=93)) &
                   ((variable %in% c("solar") & veg=="UP") | variable %in% c("Rs_up")),])+
  geom_line(aes(hour(date_time), mean.val, colour=location))+
  geom_line(aes(hour(date_time),nrcs_day, colour=location))+
  facet_grid(year~doy)

# 310 2015 (SN: +0)
# 311 2015 (tower: +0)

# off: 75 2016 at or before 7am (SN: +1) 
# together: 313 2013 at or before 7am (SN: +0) 
ggplot(env_30min[((year==2016 & yday(date_time)>=73 & yday(date_time)<=77) |
                    (year==2016 & yday(date_time)>=310 & yday(date_time)<=316)) &
                   ((variable %in% c("solar") & veg=="UP") | variable %in% c("Rs_up")),])+
  geom_line(aes(hour(date_time), mean.val, colour=location))+
  geom_line(aes(hour(date_time),nrcs_day, colour=location))+
  facet_grid(year~doy)


# create a subset with data in columnns to compare
solar.comp.sn <- copy(env_30min[((variable %in% c("solar") & veg=="UP") & location=="SN"),
                                .(date_time,mean.val)])
setnames(solar.comp.sn, c("mean.val"),c("solar.sn"))

solar.comp.tower <- copy(env_30min[variable %in% c("Rs_up"),
                                   .(date_time,mean.val)])
setnames(solar.comp.tower, c("mean.val"),c("solar.tower"))

solar.comp.nrcs <- copy(env_30min[((variable %in% c("solar") & veg=="UP") & location=="nrcs"),
                                  .(date_time,mean.val)])
setnames(solar.comp.nrcs, c("mean.val"),c("solar.nrcs"))


solar.comp <- merge(solar.comp.sn,solar.comp.tower,by=c("date_time"))
solar.comp <- merge(solar.comp,solar.comp.nrcs,by=c("date_time"))

# calculate differences 
solar.comp[,':=' (year=year(date_time),
                  doy=yday(date_time),
                  hour=hour(date_time),
                  tower.sn = solar.tower-solar.sn,
                  tower.nrcs = solar.tower-solar.nrcs,
                  sn.nrcs = solar.sn - solar.nrcs)]

ggplot(solar.comp[year==2013 & yday(date_time)>=91 & yday(date_time)<=100], aes(hour))+
  geom_line(aes(y=tower.sn),colour="blue")+
  geom_line(aes(y=tower.nrcs),colour="green")+
  geom_line(aes(y=sn.nrcs),colour="red")+
  geom_hline(yintercept=0)+
  facet_grid(year~doy)

# try to fix timestamps

  

# average the data measured at both tower and SN: soilmoisture, soiltemperature, par, precip
# at the moment (2019-05-28) average without accounting for sensor outage or different veg cover % (eg:PAR)
# average all soil moisture and temperature at all locations; add detail when I know the depths and locations

# PAR: average UP from tower and SN = PPFD
biomet_mean_parUP <- env_30min[variable %in% c("par") & veg=="UP",
                             list(PPFD_1_1_1 = mean(mean.val, na.rm=TRUE)),
                             by="date_time"]

# PAR: average all veg types from SN = reflected PPFD
biomet_mean_parVEG <- env_30min[variable %in% c("par") & veg %in% c("LATR","PRGL","DAPU","MUPO","BARE"),
                            list(PPFDr_1_1_1 = mean(mean.val, na.rm=TRUE)),
                             by="date_time"]

# precip: average BARE from tower and SN = P_rain
biomet_mean_precip <- env_30min[variable %in% c("precip.tot") & veg=="BARE",
                             list(P_rain_1_1_1 = mean(mean.val, na.rm=TRUE)),
                           by="date_time"]

ggplot(SN_30min[variable %in% c("precip.tot") & veg=="BARE" &year(date_time)==2015,],aes(date_time,mean.val,colour=veg))+geom_point()
ggplot(env_30min[variable %in% c("precip.tot") & veg=="BARE" &year(date_time)==2015,],aes(date_time,mean.val,colour=datastream))+geom_point()

ggplot(biomet_mean_precip[year(date_time)==2015,],aes(date_time,P_rain_1_1_1))+geom_point()

# soilmoisture & soiltemp: average from tower and SN regardless of depth or veg
# soil moisture = soil water content

biomet_mean_soilM <- copy(env_30min[variable %in% c("soilmoisture"),])

# for biomet soil moisture averaging reemove the periods with baseline shift
ggplot(biomet_mean_soilM[year>2017], aes(date_time,mean.val, colour=factor(depth)))+
  geom_line()+
  facet_grid(paste(location,veg)~.)


# LATR 10cm: had a baseline shift 14 Feb 2019.
biomet_mean_soilM[veg=="LATR"&depth==10&
           (date_time>=as.Date("2019-02-14")), mean.val := NA]

# LATR 20cm: had baseline shift after 3 March 2019 and does a few seept until reaching new baseline 3rd Apr 2019 
biomet_mean_soilM[veg=="LATR"&depth==20&
           date_time >= as.Date("2019-03-03"), mean.val := NA]


# MUPO 20cm: in October 2018 there was a baseline shift! After this i believe the dynamics but not the values.
biomet_mean_soilM[veg=="MUPO"&depth==20&
           (date_time>=as.Date("2018-10-26")), mean.val := NA]


biomet_mean_soilM <- biomet_mean_soilM[,list(SWC_1_1_1 = mean(mean.val, na.rm=TRUE)),
            by="date_time"]



# soil temperature = Ts
biomet_mean_soilT <- env_30min[variable %in% c("soiltemp"),
                           list(Ts_1_1_1 = mean(mean.val, na.rm=TRUE)),
                           by="date_time"]


# soil heat flux plates: soil heat flux, average across depths, separate by veg types
biomet_mean_hfp <- env_30min[variable %in% c("hfp"),veg_depth:= paste(veg,height,sep="_")][
  variable %in% c("hfp"),
                            list(SHF = mean(mean.val, na.rm=TRUE)),
                            by="date_time,veg_depth"]

biomet_mean_hfp <- dcast(biomet_mean_hfp,date_time~veg_depth, value.var="SHF")

# change column names to EddyPro format
# shrub = 1, bare = 2
setnames(biomet_mean_hfp,c("BARE_-10","BARE_-15","SHRUB_-10","SHRUB_-15"),
         c('SHF_2_1_1','SHF_2_2_1','SHF_1_1_1','SHF_1_2_1'))

# pressure: average the SN and tower
biomet_mean_Pa <- env_30min[variable %in% c("atm_press"),
                               list(Pa_1_1_1 = mean(mean.val, na.rm=TRUE)),
                               by="date_time"]


# get the other variables that don't need averaging:
# change names to Eddy Pro names and put data into column format
biomet_other <- copy(env_30min[variable %in% c("airtemp","rh","wnd_spd","wnd_dir",
                                         "Rn_nr_Avg","Rl_down","Rl_up","Rs_down","Rs_up"),
                         .(date_time,variable,mean.val)])

biomet_other1 <- dcast(biomet_other,date_time~variable, value.var="mean.val")

# change the name of SWin to Rg for global radiation because they are 
# _down is downward facing sensor = out
# _up is upward facing sensor = in
setnames(biomet_other1,c("Rl_down","Rl_up","Rn_nr_Avg","Rs_down","Rs_up","airtemp","rh","wnd_dir","wnd_spd"),
         c("LWout_1_1_1","LWin_1_1_1","Rn_1_1_1","SWout_1_1_1","Rg_1_1_1","Ta_1_1_1","RH_1_1_1","WD_1_1_1","MWS_1_1_1"))


# combine data into columns:
# biomet_mean_parUP, biomet_mean_parVEG, biomet_mean_precip, biomet_mean_soilM, biomet_mean_soilT, 
# biomet_mean_hfp, biomet_mean_Pa
biomet <- merge(biomet_other1,biomet_mean_parUP, by="date_time",all=TRUE)
biomet <- merge(biomet,biomet_mean_parVEG, by="date_time",all=TRUE)
biomet <- merge(biomet,biomet_mean_precip, by="date_time",all=TRUE)
biomet <- merge(biomet,biomet_mean_soilM, by="date_time",all=TRUE)
biomet <- merge(biomet,biomet_mean_soilT, by="date_time",all=TRUE)
biomet <- merge(biomet,biomet_mean_hfp, by="date_time",all=TRUE)
biomet <- merge(biomet,biomet_mean_Pa, by="date_time",all=TRUE)

# save Biomet Data for EddyPro for all years (R file) before editing timestamp
setwd("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/MetDataFiles_EP")
## incorrect precip data: save(biomet, file="Biomet_EddyPro_2010_2019_20190528.Rdata")

## time rounded with 'cut': save(biomet, file="Biomet_EddyPro_2010_2019_20190617.Rdata")

# time rounded with ceiling_date to match Eddy Pro:
## save(biomet, file="Biomet_EddyPro_2010_2019_20190626.Rdata")

## fixed 2014 data gap':
# save(biomet, file="Biomet_EddyPro_2010_2019_20190709.Rdata")

## fixed mean precip when it rained but one rain guage was broken (results in underestimate of mean precip across guages):
# save(biomet, file="Biomet_EddyPro_2010_2019_20190906.Rdata")

# save biomet data up to 12 Jan 2020
# save(biomet, file="Biomet_EddyPro_2010_2020_20200112.Rdata")


# load("Biomet_EddyPro_2010_2019_20190709.Rdata")

# save Biomet Data for EddyPro for each year (csv) after editing timestamp
## setwd("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/MetDataFiles_EP/20190709")

# add midnight of the next year to each file
# setwd("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/MetDataFiles_EP/20190801")


# # load function to format data for Eddy Pro and save each year

source("~/Desktop/R/R_programs/Functions/SaveFiles_Biomet_EddyPro.R")

# save each year
# savebiomet(biomet,2010,2019)

# save 2019
# savebiomet(biomet,2019,2019)

##################################################
# BIOMET: Expanded format for Ameriflux submission
# call it biomet2
# 2020-02-14: for biomet2 to merge after EddyPro processing (and data filtering, no u*)
#             report all individual sensorns for Ameriflux 

biomet2 <- copy(env_30min)

# PAR UP from tower and SN = PPFD
# tower == 1, SN == 2
# tower: PPFD_1_1_1, SN: PPFD_2_1_1
biomet2[variable %in% c("par") & veg=="UP" & location=="tower",
        ameriflux.id := "PPFD_IN_1_1_1"]

biomet2[variable %in% c("par") & veg=="UP" & location=="SN",
        ameriflux.id := "PPFD_IN_2_1_1"]

# PAR reflecteed from SN = reflected PPFD (PPFD_OUT)
# SN1 PRGL: rPPFD_1_1_1,
# SN2 LATR: 2_1_1
# SN2 PRGL: 3_1_1
# SN3 DAPU: 4_1_1
# SN4 Bare: 5_1_1
# Sn5 LATR: 6_1_1
# SN6 DAPU: 7_1_1
# SN6 LATR: 8_1_1
# SN6 MUPO: 9_1_1
# SN7 MUPO: 10_1_1
# SN8 MUPO: 11_1_1
# SN8 PRGL: 12_1_1


ggplot(biomet2[variable %in% c("par") & veg %in% c("LATR","PRGL","DAPU","MUPO","BARE")&year==2012],
               aes(date_time, mean.val))+geom_line()+facet_grid(SN+veg~.)


biomet2[variable %in% c("par") & SN=="SN1" & veg %in% c("PRGL") ,
                              ameriflux.id := "PPFD_OUT_1_1_1"]

biomet2[variable %in% c("par") & SN=="SN2" & veg %in% c("LATR") ,
        ameriflux.id := "PPFD_OUT_2_1_1"]

biomet2[variable %in% c("par") & SN=="SN2" & veg %in% c("PRGL") ,
        ameriflux.id := "PPFD_OUT_3_1_1"]

biomet2[variable %in% c("par") & SN=="SN3" & veg %in% c("DAPU") ,
        ameriflux.id := "PPFD_OUT_4_1_1"]

biomet2[variable %in% c("par") & SN=="SN4" & veg %in% c("BARE") ,
        ameriflux.id := "PPFD_OUT_5_1_1"]

biomet2[variable %in% c("par") & SN=="SN5" & veg %in% c("LATR") ,
        ameriflux.id := "PPFD_OUT_6_1_1"]

biomet2[variable %in% c("par") & SN=="SN6" & veg %in% c("DAPU") ,
        ameriflux.id := "PPFD_OUT_7_1_1"]

biomet2[variable %in% c("par") & SN=="SN6" & veg %in% c("LATR") ,
        ameriflux.id := "PPFD_OUT_8_1_1"]

biomet2[variable %in% c("par") & SN=="SN6" & veg %in% c("MUPO") ,
        ameriflux.id := "PPFD_OUT_9_1_1"]

biomet2[variable %in% c("par") & SN=="SN7" & veg %in% c("MUPO") ,
        ameriflux.id := "PPFD_OUT_10_1_1"]

biomet2[variable %in% c("par") & SN=="SN8" & veg %in% c("MUPO") ,
        ameriflux.id := "PPFD_OUT_11_1_1"]

biomet2[variable %in% c("par") & SN=="SN8" & veg %in% c("PRGL") ,
        ameriflux.id := "PPFD_OUT_12_1_1"]

# precip: BARE from tower and SN = P_rain
# tower == 1, SN == 2
# tower: P_rain_1_1_1, SN: P_rain_2_1_1
ggplot(env_30min[variable %in% c("precip.tot") & veg=="BARE" & year==2011], aes(date_time, mean.val))+
  geom_line()+
  facet_grid(location+veg~.)
                                

biomet2[variable %in% c("precip.tot") & veg=="BARE" & location=="tower",
        ameriflux.id := "P_RAIN_1_1_1"]

biomet2[variable %in% c("precip.tot") & veg=="BARE" & location=="SN",
        ameriflux.id := "P_RAIN_2_1_1"]


# report SWC seperately for each depth and veg type
# only from SN because I do not know depth of tower sensors
ggplot(biomet2[variable %in% c("soilmoisture") & veg %in% c("LATR","PRGL","MUPO","SHRUB", "BARE") & location=="SN" &
                year==2011,],
       aes(date_time, mean.val))+
  geom_line()+
  facet_grid(veg+height~.)

# BARE: 1
# LATR: 2
# MUPO: 3
# PRGL: 4

 # Bare: 1
biomet2[variable %in% c("soilmoisture") & (veg == "BARE" & location =="SN" & height=="-5"),
        ameriflux.id := "SWC_1_1_1"]

biomet2[variable %in% c("soilmoisture") & (veg == "BARE" & location =="SN" & height=="-10"),
        ameriflux.id := "SWC_1_2_1"]

biomet2[variable %in% c("soilmoisture") & (veg == "BARE" & location =="SN" & height=="-20"),
        ameriflux.id := "SWC_1_3_1"]

biomet2[variable %in% c("soilmoisture") & (veg == "BARE" & location =="SN" & height=="-30"),
        ameriflux.id := "SWC_1_4_1"]

# Latr: 2
biomet2[variable %in% c("soilmoisture") & (veg == "LATR" & location =="SN" & height=="-5"),
        ameriflux.id := "SWC_2_1_1"]

biomet2[variable %in% c("soilmoisture") & (veg == "LATR" & location =="SN" & height=="-10"),
        ameriflux.id := "SWC_2_2_1"]

biomet2[variable %in% c("soilmoisture") & (veg == "LATR" & location =="SN" & height=="-20"),
        ameriflux.id := "SWC_2_3_1"]

biomet2[variable %in% c("soilmoisture") & (veg == "LATR" & location =="SN" & height=="-30"),
        ameriflux.id := "SWC_2_4_1"]

# Mupo: 3
biomet2[variable %in% c("soilmoisture") & (veg == "MUPO" & location =="SN" & height=="-5"),
        ameriflux.id := "SWC_3_1_1"]

biomet2[variable %in% c("soilmoisture") & (veg == "MUPO" & location =="SN" & height=="-10"),
        ameriflux.id := "SWC_3_2_1"]

biomet2[variable %in% c("soilmoisture") & (veg == "MUPO" & location =="SN" & height=="-20"),
        ameriflux.id := "SWC_3_3_1"]

biomet2[variable %in% c("soilmoisture") & (veg == "MUPO" & location =="SN" & height=="-30"),
        ameriflux.id := "SWC_3_4_1"]

# Prgl: 4
biomet2[variable %in% c("soilmoisture") & (veg == "PRGL" & location =="SN" & height=="-5"),
        ameriflux.id := "SWC_4_1_1"]

biomet2[variable %in% c("soilmoisture") & (veg == "PRGL" & location =="SN" & height=="-10"),
        ameriflux.id := "SWC_4_2_1"]

biomet2[variable %in% c("soilmoisture") & (veg == "PRGL" & location =="SN" & height=="-20"),
        ameriflux.id := "SWC_4_3_1"]

biomet2[variable %in% c("soilmoisture") & (veg == "PRGL" & location =="SN" & height=="-30"),
        ameriflux.id := "SWC_4_4_1"]


# soil temperature = Ts
# calculate average 'surface': 5-15cm Ts_1
#           average 'deep': 20cm Ts_2
# then merge the wide format biomet2 with the Ts_1 and Ts_2 dataframes
ggplot(biomet2[variable %in% c("soiltemp") &
                 year==2011,],
       aes(date_time, mean.val))+
  geom_line()+
  facet_grid(veg+height~.)


biomet2_ts1 <- biomet2[!is.na(mean.val) & variable %in% c("soiltemp") & height %in% c("-5","-10","-15"),
        list(TS_1 = mean(mean.val),
             TS_1_SD = sd(mean.val),
             TS_1_N = length(mean.val)),
        by="date_time"]

biomet2_ts2 <- biomet2[!is.na(mean.val) & variable %in% c("soiltemp") & height %in% c("-20"),
                       list(TS_2 = mean(mean.val),
                            TS_2_SD = sd(mean.val),
                            TS_2_N = length(mean.val)),
                       by="date_time"]

# soil heat flux plates: separate by depth and veg types
# shrub = 1 10cm SHF_1_1_1
# shrub = 1 15cm SHF_1_2_1
# bare = 2 10cm SHF_2_1_1
# bare = 2 15cm SHF_2_2_1

ggplot(biomet2[variable %in% c("hfp")&year==2011], aes(date_time, mean.val))+geom_line()+
  facet_grid(veg~height)

biomet2[variable %in% c("hfp") & (veg == "SHRUB" & height=="-10"),
        ameriflux.id := "G_1_1_1"]

biomet2[variable %in% c("hfp") & (veg == "SHRUB" & height=="-15"),
        ameriflux.id := "G_1_2_1"]


biomet2[variable %in% c("hfp") & (veg == "BARE" & height=="-10"),
        ameriflux.id := "G_2_1_1"]

biomet2[variable %in% c("hfp") & (veg == "BARE" & height=="-15"),
        ameriflux.id := "G_2_2_1"]


# pressure: SN and tower
# tower == 1, SN == 2
# tower: Pa_1_1_1, SN: Pa_2_1_1
ggplot(biomet2[variable %in% c("atm_press")&year==2011], aes(date_time, mean.val))+geom_line()+
  facet_grid(location~.)


biomet2[variable %in% c("atm_press") & location=="tower",
        ameriflux.id := "PA_1_1_1"]

biomet2[variable %in% c("atm_press") & location=="SN",
        ameriflux.id := "PA_2_1_1"]

# leaf wetness (leave out tower lws: not filtered.)
ggplot(biomet2[variable %in% c("lws","lws_5m")&year==2011], aes(date_time, mean.val,colour=veg))+geom_line()+
  facet_grid(SN~.)

# SN1 LATR (LEAF_WET_1_1_1)
# SN2 PRGL (LEAF_WET_2_1_1)
# SN4 BARE (LEAF_WET_3_1_1)
# SN6 LATR (LEAF_WET_4_1_1)
# SN7 MUPO (LEAF_WET_5_1_1)
# SN7 FLCE (LEAF_WET_6_1_1)
# SN8 PRGL (LEAF_WET_7_1_1)

biomet2[variable %in% c("lws") & SN=="SN1" & (veg == "LATR"),
        ameriflux.id := "LEAF_WET_1_1_1"]

biomet2[variable %in% c("lws") & SN=="SN2" & (veg == "PRGL"),
        ameriflux.id := "LEAF_WET_2_1_1"]

biomet2[variable %in% c("lws") & SN=="SN4" & (veg == "BARE"),
        ameriflux.id := "LEAF_WET_3_1_1"]

biomet2[variable %in% c("lws") & SN=="SN6" & (veg == "LATR"),
        ameriflux.id := "LEAF_WET_4_1_1"]

biomet2[variable %in% c("lws") & SN=="SN7" & (veg == "MUPO"),
        ameriflux.id := "LEAF_WET_5_1_1"]

biomet2[variable %in% c("lws") & SN=="SN7" & (veg == "FLCE"),
        ameriflux.id := "LEAF_WET_6_1_1"]

biomet2[variable %in% c("lws") & SN=="SN8" & (veg == "PRGL"),
        ameriflux.id := "LEAF_WET_7_1_1"]


# #################
# # get the other variables that don't need averaging:
# # change names to Eddy Pro names and put data into column format
# biomet_other <- copy(env_30min[variable %in% c("airtemp","rh","wnd_spd","wnd_dir",
#                                                "Rn_nr_Avg","Rl_down","Rl_up","Rs_down","Rs_up"),
#                                .(date_time,variable,mean.val)])
# 
# biomet_other1 <- dcast(biomet_other,date_time~variable, value.var="mean.val")
# 
# # change the name of SWin to Rg for global radiation because they are
# # _down is downward facing sensor = out
# # _up is upward facing sensor = in
# setnames(biomet_other1,c("Rl_down","Rl_up","Rn_nr_Avg","Rs_down","Rs_up","airtemp","rh","wnd_dir","wnd_spd"),
#          c("LW_OUT_1_1_1","LW_IN_1_1_1","NETRAD_1_1_1","SW_OUT_1_1_1","SW_IN_1_1_1","TA_1_1_1","RH_1_1_1","WD_1_1_1","WS_1_1_1"))
# ##################

# graph
ggplot(biomet2[!is.na(ameriflux.id) & year==2011,], aes(date_time, mean.val))+
  geom_line()+
  facet_wrap(ameriflux.id~.)

# make biomet2 wide:
# remove variable %in% c("soiltemp","airtemp","rh","wnd_spd","wnd_dir",
#                       "Rn_nr_Avg","Rl_down","Rl_up","Rs_down","Rs_up")
biomet2 <- biomet2[!(variable %in% c("soiltemp","airtemp","rh","wnd_spd","wnd_dir",
                                 "Rn_nr_Avg","Rl_down","Rl_up","Rs_down","Rs_up")) &
                     !is.na(ameriflux.id) & !is.na(date_time),.(date_time, ameriflux.id, mean.val)]

# dcast biomet2 
biomet2_wide <- dcast(biomet2[!is.na(ameriflux.id) & !is.na(date_time),],
                      date_time ~ ameriflux.id, value.var = "mean.val", fun=mean)

# merge wide biomet2 with biomet_other1 and biomet2_ts1, biomet2_ts2
biomet2_wide <- merge(biomet_other1, biomet2_wide, by="date_time")
biomet2_wide <- merge(biomet2_wide, biomet2_ts1, by="date_time")
biomet2_wide <- merge(biomet2_wide, biomet2_ts2, by="date_time")

biomet2_wide[biomet2_wide=="NaN"] <- NA

# save biomet2 to combine with all flux data
setwd("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/MetDataFiles_EP")

# write.table(biomet2_wide, file="Biomet_USJo1_wide_2010_2019_20200218.csv", sep=",", dec=".", row.names=FALSE)

############



# save daily biomet data for Anthony and Isa Nutrient paper
biomet_anthony <- copy(biomet[date_time>=as.POSIXct("2015-05-01",tz="UTC") &
                                date_time<=as.POSIXct("2015-07-31",tz="UTC"),])

ggplot(biomet_anthony, aes(date_time,P_rain_1_1_1))+geom_line()+geom_point()


biomet_a_daily <- biomet_anthony[,lapply(.SD, function (x) {mean(x, na.rm=TRUE)}), 
                                 .SDcols = c("Ta_1_1_1","RH_1_1_1","Pa_1_1_1","WD_1_1_1","MWS_1_1_1",
                                             "PPFD_1_1_1","PPFDr_1_1_1","SWC_1_1_1",
                                             "Ts_1_1_1","SHF_1_1_1","SHF_1_2_1","SHF_2_1_1","SHF_2_2_1",
                                             "LWin_1_1_1","LWout_1_1_1","SWout_1_1_1","Rg_1_1_1","Rn_1_1_1"),
                                 by=date(date_time)]

precip_a_daily <- biomet_anthony[,list(P_rain_1_1_1 = sum(P_rain_1_1_1, na.rm=TRUE)),
                                 by=date(date_time)]

biomet_a_daily <- merge(biomet_a_daily, precip_a_daily, by="date")

ggplot(biomet_a_daily, aes(date,P_rain_1_1_1))+geom_line()+geom_point()
ggplot(biomet_a_daily, aes(date,SWC_1_1_1))+geom_line()

# save

#setwd("~/Desktop/TweedieLab/Projects/Jornada/Anthony_nutrients_fluxes/")
#write.table(biomet_a_daily, "JER_Biomet_daily_MayJuneJuly_2015_20190906.csv", row.names=FALSE, sep=",", dec=".")
#write.table(biomet_anthony, "JER_Biomet_30min_MayJuneJuly_2015_20190906.csv", row.names=FALSE, sep=",", dec=".")


# save data for Hayden
# moisture-related data:
# precip (mean of bare areas), soil moisture (individual probes)

hadn_sm_long <- copy(env_30min[date_time >= as.Date("2018-01-01") &
                                  variable %in% c("soilmoisture") & location=="SN",
                                .(date_time,variable,veg,depth,mean.val)])

hadn_mean_precip <- env_30min[date_time >= as.Date("2018-01-01") &
                                variable %in% c("precip.tot") & veg=="BARE",
                                  list(variable = unique(variable),
                                       veg = "BARE",
                                       depth = "NA",
                                       mean.val = mean(mean.val, na.rm=TRUE)),
                                  by="date_time"]

hadn_long <- rbind(hadn_sm_long,hadn_mean_precip)

ggplot(hadn_long, aes(date_time, mean.val, colour=factor(depth)))+
  geom_line()+
  facet_grid(paste(variable,veg)~., scales="free_y")

# subset for wide format
hadn_wide <- copy(env_30min[date_time >= as.Date("2018-01-01") &
                                  variable %in% c("soilmoisture") & location=="SN",
                                .(date_time,veg,depth,mean.val)][,sensorID := paste("VWC",veg,depth,sep="_")])

# cast data into wide format
hadn_wide <- dcast(hadn_wide, date_time ~ sensorID, value.var = "mean.val")
# add precip data as a column
hadn_wide <- merge(hadn_wide, hadn_mean_precip[,.(date_time,mean.val)], by="date_time")

setnames(hadn_wide,"mean.val","P_rain")

ggplot(hadn_wide, aes(date_time,P_rain))+geom_line()
ggplot(hadn_wide, aes(date_time,VWC_LATR_20))+geom_line()

# save
# write.table(hadn_long,
# file="~/Desktop/TweedieLab/People/Hayden/Precip_SoilMoisture_USJo1_2018_2019_long.csv",
# row.names=FALSE, sep=",", dec=".")
# 
# write.table(hadn_wide,
#             file="~/Desktop/TweedieLab/People/Hayden/Precip_SoilMoisture_USJo1_2018_2019_wide.csv",
#             row.names=FALSE, sep=",", dec=".")


# figures of biomet data for JER Short course Poster
# time series of rain and temperature
library(scales)

p <- ggplot(biomet[!is.na(date_time)&year(date_time)<=2019,],aes(date_time,P_rain_1_1_1,colour=factor(year(date_time))))+
  geom_line()+
  scale_x_datetime(limits=c(as.POSIXct("2010-01-01 00:00:00"),
                            as.POSIXct("2019-05-31 00:00:00")),
                   breaks="1 year",
                   labels=date_format("%Y"))+
  scale_colour_discrete(name="Year")+
  labs(x="Date", y="Total precipitation (mm)") +
  theme(
    plot.title=element_text(size = 14),
    axis.text.x=element_blank(),
    axis.title.x=element_blank(),
    #axis.text.x=element_text(size=10,margin=unit(c(2,2,2,2),"mm")),
    axis.text.y=element_text(size=14,margin=unit(c(2,2,2,2),"mm")),
    axis.title.y=element_text(size=15),
    axis.ticks.length=unit(-1.0,"mm"),
    strip.background = element_blank(),
    strip.text=element_text(size=10),
    legend.title=element_text(size=10),
    legend.text=element_text(size=9),
    legend.key = element_rect(fill=NA, colour=NA),
    legend.position = "none",
    panel.background=element_rect(fill="white",colour="black"),
    plot.margin=unit(c(1,1,1,2.5),"mm"))


t <- ggplot(biomet[!is.na(date_time)&year(date_time)<=2019,],aes(date_time,Ta_1_1_1, colour=factor(year(date_time))))+
  geom_line()+
  geom_hline(yintercept=0)+
  scale_x_datetime(limits=c(as.POSIXct("2010-01-01 00:00:00"),
                            as.POSIXct("2019-05-31 00:00:00")),
                   breaks="1 year",
                   labels=date_format("%Y"))+
  scale_colour_discrete(name="Year")+
  labs( x="Date", y=expression("Temperature ("~degree~"C)")) +
  theme(
    plot.title=element_text(size = 14),
    axis.text.x=element_text(size=14,margin=unit(c(2,2,2,2),"mm")),
    axis.text.y=element_text(size=14,margin=unit(c(2,2,2,2),"mm")),
    axis.title=element_text(size=15),
    axis.ticks.length=unit(-1.0,"mm"),
    strip.background = element_blank(),
    strip.text=element_text(size=12),
    legend.title=element_text(size=10),
    legend.text=element_text(size=9),
    legend.key = element_rect(fill=NA, colour=NA),
    legend.position = "none",
    panel.background=element_rect(fill="white",colour="black"),
    plot.margin=unit(c(2,1,1,1),"mm")) #top,right,bottom,left)

grid.arrange(p,t, nrow=2)

# Lab meeting September 30: 
library(scales)
library(gridExtra)

p1 <- ggplot(biomet[!is.na(date_time)&year(date_time)<=2019,],aes(date_time,P_rain_1_1_1,colour=factor(year(date_time))))+
  geom_line()+
  scale_x_datetime(limits=c(as.POSIXct("2010-01-01 00:00:00"),
                            as.POSIXct("2019-05-31 00:00:00")),
                   breaks="1 year",
                   labels=date_format("%Y"))+
  scale_colour_discrete(name="Year")+
  labs(x="Date", y="Total precipitation (mm)") +
  theme(
    plot.title=element_text(size = 14),
    axis.text.x=element_blank(),
    axis.title.x=element_blank(),
    #axis.text.x=element_text(size=10,margin=unit(c(2,2,2,2),"mm")),
    axis.text.y=element_text(size=14,margin=unit(c(2,2,2,2),"mm")),
    axis.title.y=element_text(size=15),
    axis.ticks.length=unit(-1.0,"mm"),
    strip.background = element_blank(),
    strip.text=element_text(size=10),
    legend.title=element_text(size=10),
    legend.text=element_text(size=9),
    legend.key = element_rect(fill=NA, colour=NA),
    legend.position = "none",
    panel.background=element_rect(fill="white",colour="black"),
    plot.margin=unit(c(1,1,1,2.5),"mm"))


t1 <- ggplot(biomet[!is.na(date_time)&year(date_time)<=2019,],aes(date_time,Ta_1_1_1, colour=factor(year(date_time))))+
  geom_line()+
  geom_hline(yintercept=0)+
  scale_x_datetime(limits=c(as.POSIXct("2010-01-01 00:00:00"),
                            as.POSIXct("2019-05-31 00:00:00")),
                   breaks="1 year",
                   labels=date_format("%Y"))+
  scale_colour_discrete(name="Year")+
  labs( x="Date", y=expression("Temperature ("~degree~"C)")) +
  theme(
    plot.title=element_text(size = 14),
    axis.text.x=element_text(size=14,margin=unit(c(2,2,2,2),"mm")),
    axis.text.y=element_text(size=14,margin=unit(c(2,2,2,2),"mm")),
    axis.title=element_text(size=15),
    axis.ticks.length=unit(-1.0,"mm"),
    strip.background = element_blank(),
    strip.text=element_text(size=12),
    legend.title=element_text(size=10),
    legend.text=element_text(size=9),
    legend.key = element_rect(fill=NA, colour=NA),
    legend.position = "none",
    panel.background=element_rect(fill="white",colour="black"),
    plot.margin=unit(c(2,1,1,1),"mm")) #top,right,bottom,left)

grid.arrange(p1,t1, nrow=2)

# calculate mean monthly temperature and cummulative precip
biomet[,':=' (month = month(date_time),
              year=year(date_time))]

TP_monthly <- biomet[date(date_time)<as.Date("2019-07-01"),list(temp.mean = mean(Ta_1_1_1, na.rm=TRUE),
                           precip.tot = sum(P_rain_1_1_1, na.rm=TRUE)),
                     by="month,year"]

TP_annual <- biomet[date(date_time)<as.Date("2019-07-01"),list(temp.mean = mean(Ta_1_1_1, na.rm=TRUE),
                                                              precip.tot = sum(P_rain_1_1_1, na.rm=TRUE)),
                   by="year"]

# graph scatter of temp and precip
ggplot(TP_monthly, aes(temp.mean, precip.tot, colour=factor(month)))+
  geom_point(size=3)+
  labs(y="Total Rain",x="Mean Temperature")+
  theme_bw()+
  guides(colour=guide_legend(title="Month"))

# graph monthly rain 
p_month <- ggplot(TP_monthly, aes(factor(month),precip.tot, fill=factor(month)))+
  geom_boxplot()+
  labs(y="Total Rain",x="Month")+
  theme_bw()+
  guides(fill="none")

# graph monthly temp 
t_month <- ggplot(TP_monthly, aes(factor(month),temp.mean, fill=factor(month)))+
  geom_boxplot()+
  labs(y="Mean Temperature",x="Month")+
  theme_bw()+
  guides(fill="none")

grid.arrange(p_month,t_month, ncol=2)

# graph annual rain 
p_year <- ggplot(TP_annual, aes(year,precip.tot))+
  geom_line()+
  geom_point(aes(colour=factor(year)),size=3)+
  lims(y=c(0,500))+
  labs(y="Total Rain",x="Year")+
  theme_bw()+
  scale_x_continuous(labels=seq(2010,2019,1),breaks=seq(2010,2019,1))+
  guides(colour="none")

p_year

# graph data coverage for SN and tower
# soil moisture
ggplot(env_30min[location=="SN"&variable=="soilmoisture"&year>=2017,], aes(date_time, coverage))+
  geom_line(size=2)+
  facet_grid(paste(veg,height,sep=".")~.)+
  labs(title="soil moisture")+
  theme_bw()

# Rain
ggplot(env_30min[location%in%c("SN","tower")&variable=="precip.tot",], aes(date_time, coverage))+
  geom_line(size=2)+
  facet_grid(paste(variable,location,veg,SN,sep=".")~.)+
  labs(title="rain")+
  theme_bw()

# PAR
ggplot(env_30min[location%in%c("SN","tower")&variable=="par",], aes(date_time, coverage))+
  geom_line(size=2)+
  facet_grid(paste(variable,location,veg,sep=".")~.)+
  labs(title="PAR")+
  theme_bw()

# solar Rad
ggplot(env_30min[location%in%c("SN","tower")&variable=="solar",], aes(date_time, coverage))+
  geom_line(size=2)+
  facet_grid(paste(variable,location,veg,sep=".")~.)+
  labs(title="Solar")+
  theme_bw()

# Short & Long radiation
ggplot(env_30min[location=="tower"&variable %in% c("Rl_up","Rl_down","Rs_up","Rs_down",
                                                   "net_ri","net_rs","albedo"),], aes(date_time, coverage))+
  geom_line(size=2)+
  facet_grid(paste(variable,sep=".")~.)+
  labs(title="Short & Long Radiation")+
  theme_bw()

# lws
ggplot(env_30min[variable %in% c("lws"),], aes(date_time, coverage))+
  geom_line(size=2)+
  facet_grid(paste(variable,location,veg,height,sep=".")~.)+
  labs(title="Leaf Wetness")+
  theme_bw()

# wind (wnd_dir, wnd_spd)
ggplot(env_30min[variable %in% c("wnd_spd","wnd_dir"),], aes(date_time, coverage))+
  geom_line(size=2)+
  facet_grid(paste(variable,location,veg,height,sep=".")~.)+
  labs(title="Wind")+
  theme_bw()

# Temperature (soiltemp, airtemp)
ggplot(env_30min[variable %in% c("soiltemp","airtemp"),], aes(date_time, coverage, colour=variable))+
  geom_line(size=2)+
  facet_grid(paste(veg,height,sep=".")~.)+
  labs(title="Temperature (all tower)")+
  theme_bw()

# pressure and rh (atm_press, rh)
ggplot(env_30min[variable %in% c("atm_press","rh"),], aes(date_time, coverage,colour=SN))+
  geom_line(size=2)+
  facet_grid(paste(variable,location,sep=".")~.)+
  labs(title="Pressure and RH")+
  theme_bw()

# look at some data
# precip, rh, and lws
ggplot(env_30min[year==2013 & month>6&month<10 & (variable == "precip.tot"& veg=="BARE" |
                   variable %in% c("rh","lws")),], aes(date_time, mean.val,colour=variable))+
  geom_line()+
  facet_grid(paste(variable,location,sep="_")~., scales="free_y")

# soil moisture and precip
ggplot(env_30min[year>=2018 &
      (variable=="soilmoisture" | (variable == "precip.tot"& veg=="BARE")),],
      aes(date_time, mean.val, colour=height))+
  geom_line()+
  facet_grid(paste(variable,veg,location,sep="_")~.,scales="free_y")


# soil and air temperature
ggplot(env_30min[variable %in% c("airtemp","soiltemp"),], aes(date_time, mean.val,colour=height))+
  geom_line(aes(linetype=veg))+
  facet_grid(paste(variable,location,height,sep="_")~.)

# par
ggplot(env_30min[variable%in% c("par","solar")&year==2012&month %in% c(7,8,9),],
       aes(date_time, mean.val,colour=datastream))+
  geom_line()+
  facet_grid(paste(variable,veg,datastream,sep="_")~., scales="free_y")

# radiation
ggplot(env_30min[variable%in% c("net_ri","net_rs")&year<2014,],
       aes(date_time, mean.val,colour=datastream))+
  geom_line()+
  facet_grid(paste(variable,veg,datastream,sep="_")~., scales="free_y")


# make an instagram image with radiation components in 2016
insta <- copy(env_30min[variable%in% c("Rl_down","Rl_up","Rs_down","Rs_up")&year==2016,][
  ,variable := factor(variable, levels=c("Rs_up","Rs_down","Rl_up","Rl_down"))][
    variable %in% c("Rl_down","Rs_down"),direction := "Outgoing"][
      variable %in% c("Rl_up","Rs_up"),direction := "Incoming"][
        variable %in% c("Rl_up","Rl_down"),radiation := "Longwave"][
          variable %in% c("Rs_up","Rs_down"),radiation := "Shortwave"])

insta[,date := as.Date(date_time)]
insta_daily <- insta[,lapply(.SD,mean),by="date,variable,direction,radiation",.SDcols=c("mean.val")]
    

ggplot(insta_daily, aes(date, mean.val,colour=variable))+
  geom_line()+
  facet_grid(direction~.)+
  labs(y="Average Daily Radiation (W/m2)",x="Date")+
  scale_colour_manual(name="Source",
                      values=(c("Rs_up"="red",
                               "Rs_down"="green",
                               "Rl_up"="orange",
                               "Rl_down"="yellow")),
                      breaks=c("Rs_up",
                               "Rs_down",
                               "Rl_up",
                               "Rl_down"),
                      labels=c("Rs_up"="Rs_in",
                               "Rs_down"="Rs_out",
                               "Rl_up"="Rl_in",
                               "Rl_down"="Rl_out"))+
  theme_bw()
  theme

ggplot(insta_daily, aes(date, mean.val,colour=variable))+
  geom_line()+
  facet_grid(radiation~.)+
  scale_colour_manual(name="Source",
                      values=(c("Rs_up"="red",
                                "Rs_down"="green",
                                "Rl_up"="orange",
                                "Rl_down"="yellow")),
                      breaks=c("Rs_up",
                               "Rs_down",
                               "Rl_up",
                               "Rl_down"),
                      labels=c("Rs_up"="Rs_in",
                               "Rs_down"="Rs_out",
                               "Rl_up"="Rl_in",
                               "Rl_down"="Rl_out"))



ggplot(insta, aes(date_time, mean.val,colour=variable))+
  geom_line()+
  scale_colour_manual(name="Source",
                      values=(c("Rs_up"="red",
                                "Rs_down"="green",
                                "Rl_up"="orange",
                                "Rl_down"="yellow")),
                      breaks=c("Rs_up",
                               "Rs_down",
                               "Rl_up",
                               "Rl_down"),
                      labels=c("Rs_up"="Rs_in",
                               "Rs_down"="Rs_out",
                               "Rl_up"="Rl_in",
                               "Rl_down"="Rl_out"))

