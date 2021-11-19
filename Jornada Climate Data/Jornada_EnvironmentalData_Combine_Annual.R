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

# This code will

# 1. connect directly to server to access files
# 2. read climate, flux, ECTM (CS650 after May 2021), SN data from L2 tables (30 min data, checked, and desired variables brought forward)
# 3. read files in wide format, transform to long format
# 4. Merge and cross-check related variables against each other for further QA/QC
# 5. Create biomet file: sensors averaged, to use in EddyPro processing
# 6. Create biomet2 file: all sensons individual, for analysis-sharing-posting to Ameriflux
# 7. save biomet and biomet2 file to server by year and save copy of code to document processing


# Data:
# Tower Climate Data: Data_QAQC_update_save_Climate_dates.R
# Sensor Network Data: Jornada_SensorNetworkData_2010_2019.R
# Soil Sensor Data: Jornada_ECTM_Data.R
# Flux table: Data_QAQC_update_save_Flux_dates.R (for soil heat flux plates, lws_1, Rl_in, Rl_out, Rs_in, Rs_out)


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
library(tidyr)
library(naniar) # replace values with NA
library(zoo)
library(scales)
library(Hmisc)
library(corrplot)

#############
# IMPORT DATA
#############
year_file <- 2020

# Sensor network data:
SN_wide <- fread(paste("/Volumes/SEL_Data_Archive/Research Data/Desert/Jornada/Bahada/SensorNetwork/Data/QAQC/WSN_L2_",year_file,".csv",sep=""),
                   sep=",", header=TRUE)

# make long
SN_30min <- melt.data.table(SN_wide,c("date_time"))
# format date and add column to deginate the data stream
SN_30min[, ':=' (mean.val = value,
                 date_time = ymd_hms(date_time),
                 variableID = as.character(variable),
                 datastream = "SN", location = "SN")][, ':=' (variable=NULL,
                                                              value=NULL)]

# split variable Id column into: SN, variable, unit, veg, depth
SN_30min[,':=' (SN = sapply(strsplit(variableID,"_"), getElement, 1),
                variable = sapply(strsplit(variableID,"_"), getElement, 2),
                unit = sapply(strsplit(variableID,"_"), getElement, 3),
                veg = sapply(strsplit(variableID,"_"), getElement, 4),
                depth = sapply(strsplit(variableID,"_"), getElement, 5))]

# make the 'NA' strings from variable ID NA
SN_30min <- SN_30min %>% 
  naniar::replace_with_na(replace=list(unit = "NA",
                                       veg = "NA",
                                       depth = "NA"))

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

# remove wide data to reduce clutter
rm(SN_wide)

# Tower Met Data (get LWS 5m from here. It's also in FluxTable)
setwd("~/Desktop/TweedieLab/Projects/Jornada/Data/Tower/Climate/Compiled")
met_30min <- fread("TowerMet_L2_2010_20201227_30min.csv", sep=",", header=TRUE)
# format date and add column to deginate the data stream
met_30min[, ':=' (date_time = ymd_hms(date_time),date_time_orig = ymd_hms(date_time_orig),
                  datastream = "climate",location = "tower", TIMESTAMP_START = NULL, TIMESTAMP_END = NULL)]
setnames(met_30min, 'value', 'mean.val')

met_30min[variable=="par", veg := "UP"]
# lws in met is at 5m
met_30min[variable %in% c("lws","airtemp"), ':=' (veg = "BARE", height = "500")]
met_30min[variable=="precip.tot", veg := "BARE"]
 
 
# Data from FluxTable: Rs, Rl, HFP, LWS_1 (in shrub)
setwd("~/Desktop/TweedieLab/Projects/Jornada/Data/Tower/Flux/Compiled_forJoining")
flux_30min <- fread("FluxTable_L2_2010_20201226_30min.csv", sep=",", header=TRUE)
flux_30min[, ':=' (date_time = ymd_hms(date_time),date_time_orig = ymd_hms(date_time_orig),
                   year=year(date_time),datastream = "flux", location = "tower")]
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
soil_30min <- fread("Soil_Temp_VWC_ECTM_L2_2010_20200324_30min.csv", sep=",", header=TRUE)
# format date and add column to deginate the data stream, get rid or uneccessary columns
setnames(soil_30min, c('value','variable'), c('mean.val', 'probe_id'))
soil_30min[measurement == "t", variable := "soiltemp"]
soil_30min[measurement == "vwc", variable := "soilmoisture"]

# get rid of TIMESTAMP variables
soil_30min[,':=' (TIMESTAMP_START=NULL,
                TIMESTAMP_END=NULL)]


# make a guess at depths
soil_30min[rep%in% c(6,7), height := "-2"] 
soil_30min[rep%in% c(1,8), height := "-10"] 
soil_30min[rep%in% c(3,5), height := "-15"] 
soil_30min[rep%in% c(2,4), height := "-20"]


# assign veg type (should be shrub/bare, once I know.)
# for now assign best guess
soil_30min[rep %in% c(1,3,7,4), veg := "BARE"]
soil_30min[rep %in% c(2,5,6,8), veg := "SHRUB"]

# modify columns to match other datastreams and get rid of redundant ones
soil_30min[, ':=' (date_time = ymd_hms(date_time),date_time_orig = ymd_hms(date_time_orig),
                   datastream = "ectm",location = "tower",
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
            date_time_orig = date_time,
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

# import potential short wave Incoming radiation shared by Ameriflux
sw.pot <- fread("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/Ameriflux/QA_QC_Report_Ameriflux/US-Jo1_HH_2010_2019_SW_IN_pot.csv",
                sep=",",header=TRUE, na.strings=c("-9999"))

sw.pot[,date_time := parse_date_time(TIMESTAMP_END, "YmdHM",tz="UTC")]

sw.pot[,':=' (location="potential",
              variable="sw_pot",
              datastream="sw_pot_ameriflux",
              mean.val=SW_IN_POT,
              year=year(date_time),
              month=month(date_time),
              doy=yday(date_time),
              date_time_orig=date_time,
              TIMESTAMP_START=NULL,
              TIMESTAMP_END=NULL)][,SW_IN_POT:=NULL]

# combine all three SEL data streams and 2010, 2011 NRCS data, SW potential
env_30min <- rbind(SN_30min,met_30min,flux_30min,soil_30min, nrcs,sw.pot, fill=TRUE)

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

precip <- dcast(precip,date_time ~SN)

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

# get all the additional columns
precip_extra <- copy(env_30min[variable=="precip.tot"&veg=="BARE"&!is.na(date_time),
                               .(date_time,date_time_orig,variable,datastream,location,probe_id,veg,height,depth,
                                 SN,year,month,doy,
                                 coverage,gapfill_id)])
precip[SN=="tower",SN := NA]

precip <- merge(precip, precip_extra, by=c("SN","date_time"))

# replace Bare precip in env_30min
env_30min1 <- copy(env_30min) 
rm(env_30min)
env_30min <- copy(env_30min1[!(variable=="precip.tot"&veg=="BARE"),])

# last step to create final version of env_30min
env_30min <- rbind(env_30min, precip)
#### STOP HERE. ####
## 

# create data for Eddy Pro Biomet
# airtemp, rh, atm_press, par, wnd_spd, wnd_dir, precip.tot, Rn_nr, Rl_downwell, Rl_upwell,
# Rs_downwell, Rs_upwell, hfp, soilmoisture

# look at soil moisture and temperature
ggplot(env_30min[variable %in% c("soilmoisture","soiltemp"),],
       aes(date_time_orig, mean.val,colour=veg, linetype=height))+
  geom_line()+
  facet_grid(paste(variable,location,sep="_")~., scales="free_y")


# look at PAR UP from tower and SN
ggplot(env_30min[variable == "par"& veg %in% c("UP"),], aes(date_time_orig, mean.val,colour=SN))+
  geom_line()+
  facet_grid(paste(variable,location,sep="_")~., scales="free_y")

# look at PAR over Veg from tower and SN
ggplot(env_30min[variable == "par"& veg %in% c("LATR","PRGL","DAPU","MUPO","BARE"),],
       aes(date_time_orig, mean.val,colour=SN))+
  geom_line()+
  facet_grid(paste(variable,veg,sep="_")~., scales="free_y")


# look at precip from tower and SN
ggplot(env_30min[variable == "precip.tot"& veg=="BARE",], aes(date_time_orig, mean.val,colour=datastream))+
  geom_line()+
  facet_grid(paste(variable,location,veg,sep="_")~., scales="free_y")

# look at heat flux plate data from tower
ggplot(env_30min[variable == "hfp",], aes(date_time_orig, mean.val,colour=veg))+
  geom_line()+
  facet_grid(paste(variable,location,height,sep="_")~., scales="free_y")

# look at pressure from the Sn and tower
ggplot(env_30min[variable == "atm_press",], aes(date_time_orig, mean.val,colour=location))+
      geom_line()+
      facet_grid(paste(variable,location,sep="_")~., scales="free_y")




# look at PAR from tower and SN
# change x variable to compare date_time or date_time_orig!!!
ggplot(env_30min[(yday(date_time)>=303 & yday(date_time)<=313) &
                   ((variable %in% c("par") & veg=="UP")),],
       aes(hour(date_time_orig), mean.val, colour=paste(variable,location)))+
  geom_line()+
  #geom_vline(xintercept=c(6.5,17.0))+
  facet_grid(year~doy)



# 2020-04-14: FOR BIOMET 1 use date_time_orig since that matches the ts data!!! 
# average the data measured at both tower and SN: soilmoisture, soiltemperature, par, precip
# at the moment (2019-05-28) average without accounting for sensor outage or different veg cover % (eg:PAR)
# average all soil moisture and temperature at all locations; add detail when I know the depths and locations

# PAR: average UP from tower and SN = PPFD
biomet_mean_parUP <- env_30min[variable %in% c("par") & veg=="UP",
                             list(PPFD_1_1_1 = mean(mean.val, na.rm=TRUE)),
                             by="date_time_orig"]

# PAR: average all veg types from SN = reflected PPFD
biomet_mean_parVEG <- env_30min[variable %in% c("par") & veg %in% c("LATR","PRGL","DAPU","MUPO","BARE"),
                            list(PPFDr_1_1_1 = mean(mean.val, na.rm=TRUE)),
                             by="date_time_orig"]

# precip: average BARE from tower and SN = P_rain
biomet_mean_precip <- env_30min[variable %in% c("precip.tot") & veg=="BARE",
                             list(P_rain_1_1_1 = mean(mean.val, na.rm=TRUE)),
                           by="date_time_orig"]

ggplot(SN_30min[variable %in% c("precip.tot") & veg=="BARE" &year(date_time_orig)==2020,],aes(date_time_orig,mean.val,colour=veg))+geom_point()
ggplot(env_30min[variable %in% c("precip.tot") & veg=="BARE" &year(date_time_orig)==2020,],aes(date_time_orig,mean.val,colour=datastream))+geom_point()

ggplot(biomet_mean_precip[year(date_time_orig)==2020,],aes(date_time_orig,P_rain_1_1_1))+geom_point()

# soilmoisture & soiltemp: average from tower and SN regardless of depth or veg
# soil moisture = soil water content

biomet_mean_soilM <- copy(env_30min[variable %in% c("soilmoisture"),])

# for biomet soil moisture averaging look at data
ggplot(biomet_mean_soilM[year>2019], aes(date_time_orig,mean.val, colour=factor(depth)))+
  geom_line()+
  facet_grid(paste(location,veg)~.)

# calculate average across depths and veg for biomet
biomet_mean_soilM <- biomet_mean_soilM[,list(SWC_1_1_1 = mean(mean.val, na.rm=TRUE)),
            by="date_time_orig"]



# soil temperature = Ts
biomet_mean_soilT <- env_30min[variable %in% c("soiltemp"),
                           list(Ts_1_1_1 = mean(mean.val, na.rm=TRUE)),
                           by="date_time_orig"]


# soil heat flux plates: soil heat flux, average across depths, separate by veg types
biomet_mean_hfp <- env_30min[variable %in% c("hfp"),veg_depth:= paste(veg,height,sep="_")][
  variable %in% c("hfp"),
                            list(SHF = mean(mean.val, na.rm=TRUE)),
                            by="date_time_orig,veg_depth"]

biomet_mean_hfp <- dcast(biomet_mean_hfp,date_time_orig~veg_depth, value.var="SHF")

# change column names to EddyPro format
# shrub = 1, bare = 2
setnames(biomet_mean_hfp,c("BARE_-10","BARE_-15","SHRUB_-10","SHRUB_-15"),
         c('SHF_2_1_1','SHF_2_2_1','SHF_1_1_1','SHF_1_2_1'))

# pressure: average the SN and tower
biomet_mean_Pa <- env_30min[variable %in% c("atm_press"),
                               list(Pa_1_1_1 = mean(mean.val, na.rm=TRUE)),
                               by="date_time_orig"]


# get the other variables that don't need averaging:
# change names to Eddy Pro names and put data into column format
biomet_other <- copy(env_30min[variable %in% c("airtemp","rh","wnd_spd","wnd_dir",
                                         "Rn_nr_Avg","Rl_down","Rl_up","Rs_down","Rs_up"),
                         .(date_time_orig,variable,mean.val)])

biomet_other1 <- dcast(biomet_other,date_time_orig~variable, value.var="mean.val")

# change the name of SWin to Rg for global radiation because they are 
# _down is downward facing sensor = out
# _up is upward facing sensor = in
setnames(biomet_other1,c("Rl_down","Rl_up","Rn_nr_Avg","Rs_down","Rs_up","airtemp","rh","wnd_dir","wnd_spd"),
         c("LWout_1_1_1","LWin_1_1_1","Rn_1_1_1","SWout_1_1_1","Rg_1_1_1","Ta_1_1_1","RH_1_1_1","WD_1_1_1","MWS_1_1_1"))


# combine data into columns:
# biomet_mean_parUP, biomet_mean_parVEG, biomet_mean_precip, biomet_mean_soilM, biomet_mean_soilT, 
# biomet_mean_hfp, biomet_mean_Pa
biomet <- merge(biomet_other1,biomet_mean_parUP, by="date_time_orig",all=TRUE)
biomet <- merge(biomet,biomet_mean_parVEG, by="date_time_orig",all=TRUE)
biomet <- merge(biomet,biomet_mean_precip, by="date_time_orig",all=TRUE)
biomet <- merge(biomet,biomet_mean_soilM, by="date_time_orig",all=TRUE)
biomet <- merge(biomet,biomet_mean_soilT, by="date_time_orig",all=TRUE)
biomet <- merge(biomet,biomet_mean_hfp, by="date_time_orig",all=TRUE)
biomet <- merge(biomet,biomet_mean_Pa, by="date_time_orig",all=TRUE)

setnames(biomet, c("date_time_orig"),c("date_time"))

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

# save biomet data up to 3 March 2020
# save(biomet, file="Biomet_EddyPro_2010_2020_20203024.Rdata")

# save biomet with SWC baseline shifts removed, Tower PAR in 2010/2011 removed, Tower LWS rescaled 0-100, based on Ameriflux QAQC from June 2020
# save(biomet, file="Biomet_EddyPro_2010_2020_20201227.Rdata")


# load("Biomet_EddyPro_2010_2019_20190709.Rdata")
# load("Biomet_EddyPro_2010_2020_20200112.Rdata")

# save Biomet Data for EddyPro for each year (csv) after editing timestamp
## setwd("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/MetDataFiles_EP/20190709")

# add midnight of the next year to each file
# setwd("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/MetDataFiles_EP/20190801")


# # load function to format data for Eddy Pro and save each year

source("~/Desktop/R/R_programs/Functions/SaveFiles_Biomet_EddyPro.R")

# save each year (NOT UPDATED 2020-12-27 because not rerunning EddyPro and the adjustments wouldn't have a huge impact.)
# savebiomet(biomet,2010,2019)

# save 2019
# savebiomet(biomet,2019,2019)

# save 2020 up to March 2020-03-24 (on 13 Nov 2020, for processing EP data for Hayden)
# savebiomet(biomet,2020,2020)

##################################################
# BIOMET2: Expanded format for Ameriflux submission
# call it biomet2
# 2020-02-14: for biomet2 to merge after EddyPro processing (and data filtering, no u*)
#             report all individual sensorns for Ameriflux 
# 2020-04-14: date_time is the adjusted timestamp to make all data match MST

biomet2 <- copy(env_30min)

# PAR UP from tower and SN = PPFD

ggplot(biomet2[variable %in% c("par") & veg=="UP"], aes(date_time, mean.val))+geom_line()+
  facet_grid(location~.)


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
# tower == 1, SN2 == 2, SN6 == 3
# tower: P_rain_1_1_1, SN: P_rain_2_1_1
ggplot(env_30min[variable %in% c("precip.tot") & veg=="BARE" & year==2015], aes(date_time, mean.val))+
  geom_line()+
  facet_grid(location+veg~.)
                                

biomet2[variable %in% c("precip.tot") & veg=="BARE" & location=="tower",
        ameriflux.id := "P_RAIN_1_1_1"]

biomet2[variable %in% c("precip.tot") & veg=="BARE" & location=="SN" & SN=="SN2",
        ameriflux.id := "P_RAIN_2_1_1"]

biomet2[variable %in% c("precip.tot") & veg=="BARE" & location=="SN" & SN=="SN6",
        ameriflux.id := "P_RAIN_3_1_1"]


# Soil moisture
# report SWC seperately for each depth and veg type only from SN
# because I do not know depth of tower sensors
ggplot(biomet2[variable %in% c("soilmoisture") & veg %in% c("LATR","PRGL","MUPO","SHRUB", "BARE") & location=="SN" &
                year==2018,],
       aes(date_time, mean.val))+
  geom_line()+
  facet_grid(veg+height~.)


# BARE: 1
# LATR: 2
# MUPO: 3
# PRGL: 4

# 2020-04-14: convert all VWC to % soil moisture, requested by Ameriflux QA/QC check.
biomet2[variable %in% c("soilmoisture"), mean.val := mean.val*100]


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
                 year==2020,],
       aes(date_time, mean.val))+
  geom_line()+
  facet_grid(veg+height~.)


biomet2_ts1 <- biomet2[!is.na(mean.val) & variable %in% c("soiltemp") & height %in% c("-5","-10","-15"),
        list(TS_1 = mean(mean.val, na.rm=TRUE),
             TS_1_SD = sd(mean.val),
             TS_1_N = length(mean.val)),
        by="date_time"]

biomet2_ts2 <- biomet2[!is.na(mean.val) & variable %in% c("soiltemp") & height %in% c("-20"),
                       list(TS_2 = mean(mean.val, na.rm=TRUE),
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

# remove SHF values less than -50
biomet2[variable %in% c("hfp") & mean.val< (-50), mean.val := NA]

# assign ameriflux variable names
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

# leaf wetness 
ggplot(biomet2[variable %in% c("lws","lws_5m")&year==2018], aes(date_time, mean.val,colour=veg))+geom_line()+
  facet_grid(SN~.)

# Graph only for tower (lws and lws_5m) to check rescale between 0-100
ggplot(biomet2[variable %in% c("lws","lws_5m")&location=="tower"], aes(date_time, mean.val,colour=veg))+geom_line()+
  facet_grid(variable~.)

# graph again
ggplot(biomet2[variable %in% c("lws","lws_5m")], aes(date_time, mean.val,colour=veg))+geom_line()+
  facet_grid(SN~.)


# Tower 5m (LEAF_WET_1_1_1)
# Tower ~1m in a shrub (LEAF_WET_1_2_1)
# SN1 LATR (LEAF_WET_2_1_1)
# SN2 PRGL (LEAF_WET_3_1_1)
# SN4 BARE (LEAF_WET_4_1_1)
# SN6 LATR (LEAF_WET_5_1_1)
# SN7 MUPO (LEAF_WET_6_1_1)
# SN7 FLCE (LEAF_WET_7_1_1)
# SN8 PRGL (LEAF_WET_8_1_1)

biomet2[variable %in% c("lws_5m") & location=="Tower" ,
        ameriflux.id := "LEAF_WET_1_1_1"]

biomet2[variable %in% c("lws") & location=="Tower",
        ameriflux.id := "LEAF_WET_1_2_1"]

biomet2[variable %in% c("lws") & SN=="SN1" & (veg == "LATR"),
        ameriflux.id := "LEAF_WET_2_1_1"]

biomet2[variable %in% c("lws") & SN=="SN2" & (veg == "PRGL"),
        ameriflux.id := "LEAF_WET_3_1_1"]

biomet2[variable %in% c("lws") & SN=="SN4" & (veg == "BARE") & depth=="UP",
        ameriflux.id := "LEAF_WET_4_1_1"]

biomet2[variable %in% c("lws") & SN=="SN6" & (veg == "LATR"),
        ameriflux.id := "LEAF_WET_5_1_1"]

biomet2[variable %in% c("lws") & SN=="SN7" & (veg == "MUPO"),
        ameriflux.id := "LEAF_WET_6_1_1"]

biomet2[variable %in% c("lws") & SN=="SN7" & (veg == "FLCE"),
        ameriflux.id := "LEAF_WET_7_1_1"]

biomet2[variable %in% c("lws") & SN=="SN8" & (veg == "PRGL"),
        ameriflux.id := "LEAF_WET_8_1_1"]


# #################
# # run for biomet2. You need this to subset out the sensors for which only 1 exists
# # get the other variables that don't need averaging:
# # change names to Eddy Pro names and put data into column format
 biomet_other_dateadj <- copy(env_30min[variable %in% c("airtemp","rh","wnd_spd","wnd_dir",
                                                "Rn_nr_Avg","Rl_down","Rl_up","Rs_down","Rs_up"),
                                .(date_time,variable,mean.val)])

 biomet_other2 <- dcast(biomet_other_dateadj[!is.na(date_time)],date_time~variable, value.var="mean.val")

# change the name of SWin to Rg for global radiation because they are
# _down is downward facing sensor = out
# _up is upward facing sensor = in
setnames(biomet_other2,c("Rl_down","Rl_up","Rn_nr_Avg","Rs_down","Rs_up","airtemp","rh","wnd_dir","wnd_spd"),
         c("LW_OUT_1_1_1","LW_IN_1_1_1","NETRAD_1_1_1","SW_OUT_1_1_1","SW_IN_1_1_1","TA_1_1_1","RH_1_1_1","WD_1_1_1","WS_1_1_1"))
# ##################

# graph
ggplot(biomet2[!is.na(ameriflux.id) & year==2019,], aes(date_time, mean.val))+
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
                      date_time ~ ameriflux.id, value.var = "mean.val")

# merge wide biomet2 with biomet_other1 and biomet2_ts1, biomet2_ts2
biomet2_wide <- merge(biomet_other2, biomet2_wide, by="date_time", all=TRUE)
biomet2_wide <- merge(biomet2_wide, biomet2_ts1, by="date_time", all.x=TRUE)
biomet2_wide <- merge(biomet2_wide, biomet2_ts2, by="date_time", all.x=TRUE)

# old code
## biomet2_wide[biomet2_wide=="NaN"] <- NA

# make the NaN columns = NA, data table gets hung up on date_time, so exclude date_time
date_time_exclude <- "date_time"

biomet2_wide[, names(biomet2_wide[,!date_time_exclude,with=FALSE]) := lapply(.SD, function(x) {x[x=="NaN"] <- NA ; x}),
             .SDcols = !date_time_exclude]

# save biomet2 to combine with all flux data
# update 27 Dec 2020 with Ameriflux QA/QC changes
setwd("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/MetDataFiles_EP/Biomet2_20201227")

# write.table(biomet2_wide, file="Biomet_USJo1_wide_2010_2019_20200218.csv", sep=",", dec=".", row.names=FALSE)

# 15 Apr 2020: save by year!
# 27 Dec 2020: save updated by year. 

saveyears <- function(data,startyear,endyear) {

  for (i in startyear:endyear) {
    data_save <- data[year(date_time)==i,]
    
    write.table (data_save,
                 file=paste("Biomet_USJo1_wide",i, ".csv",sep="_"),
                 sep =',', dec='.', row.names=FALSE,quote=FALSE)
  }}


# saveyears(biomet2_wide,2010,2020)

### GAPFILL ENV Data for internal use #### 

# From ReddyProc FAQ: 
# I have no incoming solar radiation (Rg) - What can I do?
#   
# Incoming radiation (Rg) is used for different purposes in REddyproc. The answer depends on which purpose.
# Option 1: Estimate Rg by measured PAR
# *This should give reasonable results for gap-filling, nighttime partitoning, and daytime partitioning Rg = PAR/0.47
# *References e.g. Yu 2015 et al. 10.1016/j.enconman.2014.09.038, Britton & Dodd 1976, 10.1016/0002-1571(76)90080-7
# Option 2: Use Rg of a geographically not to distant site
# *ok for night-time partitioning,
# *probably reasonable for gap-filling unless the periods where conditions differ much between the sites, and
# *do not use daytime partitioning.
# Option 3: net radiation
# *is not an option for us, but try to convince us with your results.

# see where gaps are
# create a gap variable that 1 for data and 0 for NA
env_30min[is.na(mean.val),gap:=0]
env_30min[!is.na(mean.val),gap:=1]

# graph the gap locations: 
ggplot(env_30min[variable %in% "Rs_up",], aes(doy,gap))+
  geom_line()+
  facet_grid(year~.)

# graph the data
ggplot(env_30min[variable %in% "Rs_up",], aes(doy,mean.val))+
  geom_line()+
  facet_grid(year~.)

# graph the gap locations for SW from other locations and sensors
ggplot(env_30min[((variable %in% c("solar") & veg=="UP" )| variable %in% c("Rs_up")),],
       aes(doy,gap,colour=location))+
  geom_line()+
  facet_grid(location~year)

# graph the NRCS and Sn data with the tower data early in the year
ggplot(env_30min[((variable %in% c("solar") & veg=="UP" )| variable %in% c("Rs_up")) &
                   doy>=1 & doy <=20,],
       aes(hour(date_time),mean.val,colour=location))+
  geom_line()+
  facet_grid(year~doy)

# graph the NRCS and Sn data with the tower data mid-year
ggplot(env_30min[((variable %in% c("solar") & veg=="UP" )| variable %in% c("Rs_up")) &
                   doy>=1 & doy <=10,],
       aes(hour(date_time),mean.val,colour=location))+
  geom_line()+
  facet_grid(year~doy)


# graph the NRCS and Sn data with the tower data mid-year
ggplot(env_30min[((variable %in% c("solar") & veg=="UP" )| variable %in% c("Rs_up")) &
                   doy>=100 & doy <=120,],
       aes(hour(date_time),mean.val,colour=variable))+
  geom_line()+
  facet_grid(year~doy)

# graph the NRCS and Sn data with the tower data late-year
ggplot(env_30min[((variable %in% c("solar") & veg=="UP" )| variable %in% c("Rs_up")) &
                   doy>=300 & doy <=320,],
       aes(hour(date_time),mean.val,colour=location))+
  geom_line()+
  facet_grid(year~doy)


# look at RS_up and PAR at tower
ggplot(env_30min[((variable %in% c("par") & veg=="UP" )|
                    variable %in% c("Rs_up")) &
                   doy>=100 & doy <=120 &
                   location=="tower",],
       aes(hour(date_time),mean.val,colour=variable))+
  geom_line()+
  facet_grid(year~doy)

# look at RS_up and PAR at SN
ggplot(env_30min[(variable %in% c("par",
                                   'solar') & veg=="UP" ) &
                   doy>=100 & doy <=120 &
                   location=="SN",],
       aes(hour(date_time),mean.val,colour=variable))+
  geom_line()+
  facet_grid(year~doy)

# graph the relationship between NRCS, SN, and tower data
# subset only the solar variables
solar.comp.sn <- copy(env_30min[((variable %in% c("solar") & veg=="UP") & location=="SN"),
                                .(date_time,mean.val)])
setnames(solar.comp.sn, c("mean.val"),c("solar.sn"))

par.comp.sn <- copy(env_30min[((variable %in% c("par") & veg=="UP") & location=="SN"),
                                .(date_time,mean.val)])
setnames(par.comp.sn, c("mean.val"),c("par.sn"))

solar.comp.tower <- copy(env_30min[variable %in% c("Rs_up"),
                                   .(date_time,mean.val)])
setnames(solar.comp.tower, c("mean.val"),c("solar.tower"))

par.comp.tower <- copy(env_30min[((variable %in% c("par") & veg=="UP") & location=="tower"),
                                   .(date_time,mean.val)])
setnames(par.comp.tower, c("mean.val"),c("par.tower"))


solar.comp.nrcs <- copy(env_30min[((variable %in% c("solar") & veg=="UP") & location=="nrcs"),
                                  .(date_time,mean.val)])
setnames(solar.comp.nrcs, c("mean.val"),c("solar.nrcs"))


solar.comp <- merge(solar.comp.sn,solar.comp.tower,by=c("date_time"),all=TRUE)
solar.comp <- merge(solar.comp,solar.comp.nrcs,by=c("date_time"), all=TRUE)
solar.comp <- merge(solar.comp,par.comp.tower,by=c("date_time"), all=TRUE)
solar.comp <- merge(solar.comp,par.comp.sn,by=c("date_time"), all=TRUE)

# remove NAs in date_time
solar.comp <- solar.comp[!is.na(date_time)]

# create all dates and then interpolate the half-hours at nrcs
# from 2010-01-01 00:00 to 2020-03-24 13:00:00
alldates <- data.table(date_time=seq(as.POSIXct("2009-12-31 17:00",tz="UTC"),
                                        as.POSIXct("2020-03-24 13:00",tz="UTC"),
                                        by="30 mins"))

solar.comp <- merge(alldates,solar.comp, by="date_time", all.x=TRUE)

# interpolate half hours for solar.nrcs
solar.comp[date_time>=as.POSIXct("2009-12-31 17:00")&
             date_time <= as.POSIXct("2017-07-04 09:00"),
           solar.nrcs.int := na.approx(solar.nrcs)]

# look at a few of the interpolated points
ggplot(solar.comp[yday(date_time)>=100 & yday(date_time) <=110,])+
      geom_point(aes(hour(date_time),solar.nrcs), colour="pink", size=0.5)+
  geom_line(aes(hour(date_time),solar.nrcs.int), size=0.25)+
  facet_grid(year(date_time)~yday(date_time))

# convert PAR using the ReddyProc equation
solar.comp[,':=' (par.sn.adj=par.sn*0.47, par.tower.adj=par.tower*0.47)]

# graph adjusted par and solar: SN
ggplot(solar.comp[yday(date_time)>=100 & yday(date_time) <=110,])+
  geom_line(aes(hour(date_time),par.sn.adj),colour="green")+
  geom_line(aes(hour(date_time),solar.sn))+
  labs(title="SN")+
  facet_grid(year(date_time)~yday(date_time))

# graph adjusted par and solar: Tower
ggplot(solar.comp[yday(date_time)>=100 & yday(date_time) <=110,])+
  geom_line(aes(hour(date_time),par.tower.adj),colour="green")+
  geom_line(aes(hour(date_time),solar.tower))+
  labs(title="Tower")+
  facet_grid(year(date_time)~yday(date_time))


# graph adjusted par and solar: SN PAR with Tower Solar
ggplot(solar.comp[yday(date_time)>=100 & yday(date_time) <=110,])+
  geom_line(aes(hour(date_time),par.sn.adj),colour="green")+
  geom_line(aes(hour(date_time),solar.tower))+
  labs(title="SN Par and Tower Solar")+
  facet_grid(year(date_time)~yday(date_time))


# relationship between SN PAR and Tower Solar
ggplot(solar.comp, aes(par.sn.adj, solar.tower))+
  geom_point()+
  geom_abline(aes(intercept=0, slope=1),colour="blue")

# look at the relationship with SN and Tower Solar coloured by month
ggplot(solar.comp, aes(solar.sn, solar.tower, colour=factor(month(date_time))))+
  geom_point()

# look at the relationship with SN and Tower Solar coloured by month, with smooth
ggplot(solar.comp, aes(solar.sn, solar.tower, colour=factor(month(date_time))))+
  geom_point(alpha=0.5)+
  geom_smooth(method="lm", alpha=0)+
  geom_abline(aes(intercept=0, slope=1))+
  facet_wrap(month(date_time)~.)

# instead split by hour of the day
# look at the relationship with SN and Tower Solar coloured by month, with smooth
ggplot(solar.comp, aes(solar.sn, solar.tower, colour=factor(hour(date_time))))+
  geom_point(alpha=0.5)+
  geom_smooth(method="lm", alpha=0)+
  geom_abline(aes(intercept=0, slope=1))+
  facet_wrap(hour(date_time)~.)


# compare NRCS station with tower
# look at the relationship with NRCS and Tower coloured by month, with smooth
ggplot(solar.comp, aes(solar.nrcs, solar.tower, colour=factor(month(date_time))))+
  geom_point(alpha=0.5)+
  geom_smooth(method="lm", alpha=0)+
  geom_abline(aes(intercept=0, slope=1))+
  facet_wrap(month(date_time)~.)

# instead split by hour of the day
# look at the relationship with NRCS and Tower coloured by month, with smooth
ggplot(solar.comp, aes(solar.nrcs, solar.tower, colour=factor(hour(date_time))))+
  geom_point(alpha=0.5)+
  geom_smooth(method="lm", alpha=0)+
  geom_abline(aes(intercept=0, slope=1))+
  facet_wrap(hour(date_time)~.)

# look at the relationship with Tower PAR and Tower Solar coloured by month, with smooth
ggplot(solar.comp, aes(par.tower.adj, solar.tower, colour=factor(month(date_time))))+
  geom_point(alpha=0.5)+
  geom_smooth(method="lm", alpha=0)+
  geom_abline(aes(intercept=0, slope=1))+
  facet_wrap(month(date_time)~.)

# several of the measurements in tower PAR are bad, look by year...
# the tower PAR to solar is good until 2014. In 2015 the PAR sensor starts going bad
# sometimes during the day and this gets worse and worse into 2019. 2020 doesn't look that bad... 
ggplot(solar.comp, aes(par.tower.adj, solar.tower, colour=factor(month(date_time))))+
  geom_point(alpha=0.5)+
  geom_smooth(method="lm", alpha=0)+
  geom_abline(aes(intercept=0, slope=1))+
  facet_wrap(year(date_time)~.)


# look at the relationship with SN PAR and Tower Solar coloured by month, with smooth
ggplot(solar.comp, aes(par.sn.adj, solar.tower, colour=factor(month(date_time))))+
  geom_point(alpha=0.5)+
  geom_smooth(method="lm", alpha=0)+
  geom_abline(aes(intercept=0, slope=1))+
  facet_wrap(month(date_time)~.)

# look by year, some spots are funky, 
# in 2013 March, April, and May have several outlying values in the solar tower data
# in 2020 March has outlying values in SN PAR and Tower Solar... large spread/cloud in the data
ggplot(solar.comp, aes(par.sn.adj, solar.tower, colour=factor(month(date_time))))+
  geom_point(alpha=0.5)+
  geom_smooth(method="lm", alpha=0)+
  geom_abline(aes(intercept=0, slope=1))+
  facet_wrap(year(date_time)~.)

# make a correlation matrix and calculate by month and year

# add hour, date, month and year to solar.comp
solar.comp[,':='(hour = hour(date_time),date = as.Date(date_time),month=month(date_time),year=year(date_time))]

# use function 
fun.corr <- function(dat) {
 # dat1 <- copy(dat[,solar.nrcs := NULL])
 dat.corr <- rcorr(as.matrix(dat))
 return(dat.corr$r)
}

# create a data table for correlation with non-interpolated nrcs data removed and all NAs omitted
solar.comp.corr.dat <- copy(solar.comp[,solar.nrcs := NULL])
solar.comp.corr.dat <- na.omit(solar.comp.corr.dat)

solar.comp.corr <- solar.comp.corr.dat[,.(r = list(fun.corr(.SD))),by="year,month",
                              .SDcols=c("solar.sn","solar.tower","solar.nrcs.int",
                                       "par.sn.adj","par.tower.adj")]

# graph
corrplot(solar.comp.corr$r[[22]], type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

# http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software 
# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
#########
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

flattenCorrMatrix_r <- function(dat) {
  cormat <- dat$r[[1]]
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut]
  )
}
########

solar.comp.corr.flat <- solar.comp.corr[,flattenCorrMatrix_r(.SD),by="year,month"]

ggplot(solar.comp.corr.flat, aes(row,column,fill=cor))+
  geom_tile()+
  facet_grid(month~year)+
  theme(axis.text.x = element_text(angle=45))

solar.corr <- flattenCorrMatrix(solar.comp.corr$r, solar.comp.corr$P)

# 
solar.comp [,':='(solar.tower.sn = solar.tower-solar.sn,
                                     solar.tower.nrcs = solar.tower-solar.nrcs.int,
                                     solar.par.tower = solar.tower-par.tower.adj,
                                     solar.par.sn = solar.tower-par.sn.adj)]

ggplot(solar.comp, aes(date_time,solar.tower.sn))+geom_point() # +/- 300
ggplot(solar.comp, aes(date_time,solar.tower.nrcs))+geom_point() # +/- 550
ggplot(solar.comp, aes(date_time,solar.par.tower))+geom_point() # +/- 200
ggplot(solar.comp, aes(date_time,solar.par.sn))+geom_point() # +/- 200

solar.comp.sd <- solar.comp[(solar.tower.sn >= (-300) & solar.tower.sn<=300) |
                              (solar.tower.nrcs >= (-550) | solar.tower.nrcs <= 550) |
                              (solar.par.tower >= (-200) & solar.par.tower <= 200) |
                              (solar.par.sn >= (-200) & solar.par.sn <= 200),
  list(solar.tower.sn.mean=mean(solar.tower.sn,na.rm=TRUE),
                                  solar.tower.nrcs.mean=mean(solar.tower.nrcs,na.rm=TRUE),
                                  solar.par.tower.mean=mean(solar.par.tower,na.rm=TRUE),
                                  solar.par.sn.mean=mean(solar.par.sn,na.rm=TRUE),
                                  solar.tower.sn.sd=sd(solar.tower.sn,na.rm=TRUE),
                        solar.tower.nrcs.sd=sd(solar.tower.nrcs,na.rm=TRUE),
                        solar.par.tower.sd=sd(solar.par.tower,na.rm=TRUE),
                        solar.par.sn.sd=sd(solar.par.sn,na.rm=TRUE)),
                  by="hour,month"]

solar.comp1 <- merge(solar.comp,solar.comp.sd,by=c("hour","month"), all.x=TRUE)

ggplot(solar.comp1[year>2010 & month==10,])+geom_point(aes(hour(date_time),solar.tower.sn),size=0.25)+
  geom_line(aes(hour(date_time),solar.tower.sn.mean+2*solar.tower.sn.sd),colour="green",size=0.5)+
  geom_line(aes(hour(date_time),solar.tower.sn.mean-2*solar.tower.sn.sd),colour="green",size=0.5)+
  facet_grid(year~yday(date_time))
  


# gapfill missing Rs (solar.tower)
# use direct-imputation since the relationships generally fall on 1:1 line
# and the diurnal patterns investigated match nicely
# Use hierarchical direct-fill approach:


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
ggplot(env_30min[year==2015 & (yday(date_time)>=111 & yday(date_time)<=166) &
                   ((variable %in% c("solar") & veg=="UP" )| variable %in% c("Rs_up","sw_pot")),],
       aes(hour(date_time_orig), mean.val, colour=paste(variable,location)))+
  geom_line()+
  geom_point(size=0.5)+
  #geom_vline(xintercept=c(6.5,17.0))+
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
                   ((variable %in% c("par") & veg=="UP") |
                      (variable %in% c("solar") & location=="nrcs")|
                      variable %in% c("sw_pot")),],
       aes(hour(date_time_orig), mean.val, colour=location))+
  geom_line()+
  facet_grid(year~doy)

# off: 194 18:30 2011 (tower: +1) 
# together: 316 2011 11:30-12:00 (tower to +0)
ggplot(env_30min[year==2011 & (yday(date_time)>=190 & yday(date_time)<=196 | yday(date_time)>=315 & yday(date_time)<=317) &
                   ((variable %in% c("solar") & veg=="UP") | (variable %in% c("par") & location=="tower")|
                      variable %in% c("sw_pot")),],
       aes(hour(date_time_orig), mean.val, colour=location))+
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
                   ((variable %in% c("solar") & veg=="UP" & location == "SN") | variable %in% c("Rs_up") |
                      variable %in% c("sw_pot")),],
       aes(hour(date_time_orig), mean.val, colour=location))+
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
                                .(date_time_orig,mean.val)])
setnames(solar.comp.sn, c("mean.val"),c("solar.sn"))

solar.comp.tower <- copy(env_30min[variable %in% c("Rs_up"),
                                   .(date_time_orig,mean.val)])
setnames(solar.comp.tower, c("mean.val"),c("solar.tower"))

solar.comp.nrcs <- copy(env_30min[((variable %in% c("solar") & veg=="UP") & location=="nrcs"),
                                  .(date_time_orig,mean.val)])
setnames(solar.comp.nrcs, c("mean.val"),c("solar.nrcs"))


solar.comp <- merge(solar.comp.sn,solar.comp.tower,by=c("date_time_orig"))
solar.comp <- merge(solar.comp,solar.comp.nrcs,by=c("date_time_orig"))

# calculate differences 
solar.comp[,':=' (year=year(date_time_orig),
                  doy=yday(date_time_orig),
                  hour=hour(date_time_orig),
                  tower.sn = solar.tower-solar.sn,
                  tower.nrcs = solar.tower-solar.nrcs,
                  sn.nrcs = solar.sn - solar.nrcs)]

ggplot(solar.comp[year==2013 & yday(date_time_orig)>=91 & yday(date_time_orig)<=100], aes(hour))+
  geom_line(aes(y=tower.sn),colour="blue")+
  geom_line(aes(y=tower.nrcs),colour="green")+
  geom_line(aes(y=sn.nrcs),colour="red")+
  geom_hline(yintercept=0)+
  facet_grid(year~doy)


# look at potential SW

test.sw <- merge(biomet2_wide, sw.pot, by="date_time", all=TRUE)

ggplot(test.sw[as.Date(date_time)>=as.Date("2020-01-01") & as.Date(date_time)<=as.Date("2020-01-31")],
       aes(hour(date_time)))+
  geom_line(aes(y=SW_IN_1_1_1),colour="blue")+
  geom_line(aes(y=PPFD_IN_1_1_1),colour="purple")+
  geom_line(aes(y=PPFD_IN_2_1_1),colour="red")+
  #geom_line(aes(y=mean.val),colour="black")+
  #geom_hline(yintercept=0)+
  facet_wrap(doy~.)

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


# For Maria Saenz Franco
# Weekly temperature and precipitation data
# Create a week column

biomet[, ':='(year=year(date_time),
              date = as.Date(date_time),
              week= week(date_time))]

# calculate daily and weekly cumulative precipitation and mean temperatures
biomet_daily <- biomet[!is.na(date_time),list(precip_daily = sum(P_rain_1_1_1, na.rm=TRUE),
                              Ta_daily = mean(Ta_1_1_1, na.rm=TRUE)),
                              by="year,date"]

biomet_weekly <- biomet[!is.na(date_time),list(date=unique(date),
                                               precip_week = sum(P_rain_1_1_1, na.rm=TRUE),
                                              Ta_week = mean(Ta_1_1_1, na.rm=TRUE)),
                       by="year,week"]


# make figure of daily and weekly precip and air temperature
p_rain_daily <- ggplot(biomet_daily, aes(date, precip_daily, colour=factor(year)))+
  geom_col()+
  scale_color_brewer(palette="Spectral", guide=FALSE)+
  scale_x_date(date_breaks="month")+
  facet_grid(.~year, scales="free_x")+
  theme_bw()

p_Ta_daily <- ggplot(biomet_daily, aes(date, Ta_daily, colour=factor(year)))+
  geom_line()+
  scale_color_brewer(palette="Spectral", guide=FALSE)+
  facet_grid(.~year, scales="free_x")+
  theme_bw()

p_daily <- grid.arrange(p_Ta_daily, p_rain_daily)

# week
p_rain_week <- ggplot(biomet_weekly[year>2011&year<2018,], aes(date, precip_week, fill=factor(year)))+
  geom_col()+
  scale_fill_brewer(palette="Spectral", guide=FALSE)+
  scale_x_date(date_minor_breaks="months",date_labels="%b")+
  facet_grid(.~year, scales="free_x")+
  theme_light(base_size=14)+
  labs(x="Month",y="Total Weekly Precipitation (mm)") 

p_Ta_week <- ggplot(biomet_weekly, aes(date, Ta_week, colour=factor(year)))+
  geom_line()+
  scale_color_brewer(palette="Spectral", guide=FALSE)+
  facet_grid(.~year, scales="free_x")+
  theme_bw()

p_week <- grid.arrange(p_Ta_week, p_rain_week) 


# figures of biomet data for JER Short course Poster
# time series of rain and temperature
library(scales)
library(gridExtra)

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

