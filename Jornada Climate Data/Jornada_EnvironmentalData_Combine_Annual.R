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


############
# CS 650 data
###########
# Numbers in variable names, eg: T_1, P_1, T_2, P_2 follow the SDI address of each sensor. 
# 
# All read 1.9-2.2% VWC at same location
# 
# SDI, SN, Depth (cm) of sensors, Location relative to Caliche
# SDI 1 SN 46381 Depth 100.5cm Caliche inside/below
# SDI 2 SN 46371 Depth 42.5cm Caliche above
# SDI 3 SN 46374 Depth 25.5 cm Caliche above
# SDI 4 SN 46416 Depth 17.5 cm Caliche above
# SDI 5 SN 46414 Depth 11.5 cm Caliche above
# 
# Data logging starts 2021-05-04 12:44:00

# T soiltemp Celsius
# VWC soilmoisture proportion
# EC soilconductivity dS/m
# P soilpermittivity not sure
# PA soilperiodaverage uS
# VR soilvoltratio ratio

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
year_file <- 2021
# Sensor network data:
SN_wide <- fread(paste("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Bahada/SensorNetwork/Data/QAQC/WSN_L2_",year_file,".csv",sep=""),
                   sep=",", header=TRUE)

# make long
SN_30min <- melt.data.table(SN_wide,c("date_time"))
# format date and add column to deginate the data stream
SN_30min[, ':=' (mean.val = value,
                 date_time = ymd_hms(date_time),
                 variable = as.character(variable),
                 datastream = "SN", location = "SN")][, ':=' (value=NULL)]

# split variable Id column into: SN, variable, unit, veg, depth
SN_30min[,':=' (SN = sapply(strsplit(variable,"_"), getElement, 1),
                variable = sapply(strsplit(variable,"_"), getElement, 2),
                unit = sapply(strsplit(variable,"_"), getElement, 3),
                veg = sapply(strsplit(variable,"_"), getElement, 4),
                depth = sapply(strsplit(variable,"_"), getElement, 5))]

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
met_wide <- fread(paste("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Bahada/Tower/TowerClimate_met/Combined/dataL2_met_",year_file,".csv",sep=""),
                   sep=",", header=TRUE)

# adjust name of precip_tot
setnames(met_wide, c('precip_tot','t_hmp','rh_hmp','hor_wnd_spd','hor_wnd_dir'), c('precip.tot','airtemp','rh','wnd_spd','wnd_dir'))


# format date and add column to deginate the data stream
met_30min <- melt.data.table(met_wide,c("timestamp"))

met_30min[, ':=' (date_time = ymd_hms(timestamp),
                  datastream = "climate",location = "tower")][,timestamp:=NULL]
setnames(met_30min, 'value', 'mean.val')

met_30min[variable=="par", veg := "UP"]
# lws in met is at 5m
met_30min[variable %in% c("lws","airtemp"), ':=' (veg = "BARE", height = "500")]
met_30min[variable=="precip.tot", veg := "BARE"]
 
# remove met_wide
rm(met_wide) 

# Data from FluxTable: Rs, Rl, HFP, LWS_1 (in shrub)
flux_wide <- fread(paste("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Bahada/Tower/Flux/Combined/dataL2_flux_",year_file,".csv",sep=""),
                    sep=",", header=TRUE)
# make data long and format to match others for binding
flux_30min <- melt.data.table(flux_wide,c("date_time"))

flux_30min[, ':=' (date_time = ymd_hms(date_time),
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

# get rid of flux_wide
rm(flux_wide)

# ECTM stopped March 2021. After this have CS650 probes in a profile with data since May
# Numbers in variable names, eg: T_1, P_1, T_2, P_2 follow the SDI address of each sensor. 
# 
# All read 1.9-2.2% VWC at same location
# 
# SDI, SN, Depth (cm) of sensors, Location relative to Caliche
# SDI 1 SN 46381 Depth 100.5cm Caliche inside/below
# SDI 2 SN 46371 Depth 42.5cm Caliche above
# SDI 3 SN 46374 Depth 25.5 cm Caliche above
# SDI 4 SN 46416 Depth 17.5 cm Caliche above
# SDI 5 SN 46414 Depth 11.5 cm Caliche above
# 
# Data logging starts 2021-05-04 12:44:00 

cs650_wide <- fread(paste("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Bahada/Tower/SoilSensor_CS650/Combined/dataL2_Soil_",year_file,".csv",sep=""),
                    sep=",", header=TRUE)

cs650 <- melt(cs650_wide,c("TIMESTAMP"))


# extract info on measurent and rep from variable
cs650[,variable := as.character(variable)][,':=' (measurement = sapply(strsplit(variable,"_"), getElement, 2),
                rep = sapply(strsplit(variable,"_"), getElement, 3))]

cs650[measurement == "T", variable := "soiltemp"]
cs650[measurement == "VWC", variable := "soilmoisture"]
cs650[measurement == "EC", variable := "soilconductivity"]
cs650[measurement == "P", variable := "soilpermittivity"]
cs650[measurement == "PA", variable := "soilperiodaverage"]
cs650[measurement == "VR", variable := "soilvoltratio"]
cs650[,variable := droplevels(factor(variable))]

setnames(cs650, c('value'), c('mean.val'))

# add depths
cs650[rep==1, height := "-100.5"]
cs650[rep==2, height := "-42.5"]
cs650[rep==3, height := "-25.5"]
cs650[rep==4, height := "-17.5"]
cs650[rep==5, height := "-11.5"]


# assign veg type 
cs650[, veg := "BARE"]

# modify columns to match other datastreams and get rid of redundant ones
cs650[, ':=' (date_time = ymd_hms(TIMESTAMP),
                   datastream = "cs650",location = "tower",
                    rep=NULL, measurement=NULL, TIMESTAMP=NULL)]

# remove soil_wide
rm(cs650_wide)

# ############################################
# # # Tower soil temperature and moisture data (ECTM)
# ############################################
# soil_wide <- fread(paste("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Bahada/Tower/SoilSensor_ECTM/Combined/dataL2_ectm_",year_file,".csv",sep=""),
#                     sep=",", header=TRUE)
# 
# # format date and add column to deginate the data stream, get rid or uneccessary columns
# soil_30min <- melt(soil_wide,c("date_time"))
# 
# # extract info on measurent and rep from variable
# soil_30min[,variable := as.character(variable)][,':=' (measurement = sapply(strsplit(variable,"_"), getElement, 1),
#                 rep = sapply(strsplit(variable,"_"), getElement, 2))]
# 
# soil_30min[measurement == "t", variable := "soiltemp"]
# soil_30min[measurement == "vwc", variable := "soilmoisture"]
# 
# setnames(soil_30min, c('value'), c('mean.val'))
# 
# # make a guess at depths
# soil_30min[rep%in% c(6,7), height := "-2"]
# soil_30min[rep%in% c(1,8), height := "-10"]
# soil_30min[rep%in% c(3,5), height := "-15"]
# soil_30min[rep%in% c(2,4), height := "-20"]
# 
# 
# # assign veg type (should be shrub/bare, once I know.)
# # for now assign best guess
# soil_30min[rep %in% c(1,3,7,4), veg := "BARE"]
# soil_30min[rep %in% c(2,5,6,8), veg := "SHRUB"]
# 
# # modify columns to match other datastreams and get rid of redundant ones
# soil_30min[, ':=' (date_time = ymd_hms(date_time),
#                    datastream = "ectm",location = "tower",
#                     rep=NULL, measurement=NULL)]
# 
# # remove soil_wide
# rm(soil_wide)
# ##################################

# combine all four SEL data streams 
# soil_30min is for ECTM
env_30min <- rbind(SN_30min,met_30min,flux_30min, cs650,fill=TRUE)

# some datastreams don't have all the time stamp columns. Create
env_30min[,':=' (year = year(date_time),
                 month = month(date_time),
                 doy=yday(date_time))]

# check levels of variable column
levels(factor(env_30min$variable))
# [1] "battery"      "current"      "lws"          "par"          "precip.tot"   "solar"        "voltage"      "soilmoisture" "atm_press"   
# [10] "airtemp"      "rh"           "e_hmp"        "wnd_spd"      "wnd_dir"      "albedo"       "lws_2"        "NetRs"        "Net_Rl"      
# [19] "UpTot"        "DnTot"        "Rn_nr_Avg"    "hfp"          "Rs_down"      "Rs_up"        "Rl_down"      "Rl_up"        "soiltemp"    

# cs650 adds:
# "soilconductivity"  "soilperiodaverage" "soilpermittivity"     
# [31] "soilvoltratio"   
 

# check the levels of height and order them
levels(factor(env_30min$height))

# with ECTM and SN only 
# env_30min[,height := factor(height,levels=c("-30","-20","-15","-10","-5","-2","50","500"))]
# with CS650, SN, and ECTM:
 env_30min[,height := factor(height,levels=c("-100.5","-42.5","-30","-25.5","-20","-17.5","-15","-11.5",
                                             "-10","-5","-2","50","500"))]


 # with CS650 and SN only
 # env_30min[,height := factor(height,levels=c("-100.5","-42.5","-30","-25.5","-20","-17.5","-15","-11.5",
 #                                             "-10","-5","50","500"))]
 
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

# in 2021 and 2022 SN6 was not working, make all SN6 precip NA
precip[, SN6 := NA]

# now join the fixed precip data back to the env_30min
precip <- melt(precip,measure.vars=c("tower","SN2","SN6"), variable.name="SN",value.name="mean.val")

# ggplot(precip, aes(date_time, mean.val, colour=SN))+geom_line()

# get all the additional columns
precip_extra <- copy(env_30min[variable=="precip.tot"&veg=="BARE"&!is.na(date_time),
                               .(date_time,variable,unit,datastream,location,veg,height,depth,
                                 SN,year,month,doy,
                                 coverage)])
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

# look at air temperature
ggplot(env_30min[variable %in% c("airtemp"),],
       aes(date_time, mean.val))+
  geom_line()+
  facet_grid(paste(variable,location,sep="_")~., scales="free_y")


# look at soil moisture and temperature
ggplot(env_30min[variable %in% c("soilmoisture","soiltemp"),],
       aes(date_time, mean.val,colour=veg, linetype=height))+
  geom_line()+
  facet_grid(paste(variable,location,sep="_")~., scales="free_y")


# look at PAR UP from tower and SN
ggplot(env_30min[variable == "par"& veg %in% c("UP"),], aes(date_time, mean.val,colour=SN))+
  geom_line()+
  facet_grid(paste(variable,location,sep="_")~., scales="free_y")

# look at downward PAR over Veg from SN
ggplot(env_30min[variable == "par"& veg %in% c("LATR","PRGL","DAPU","MUPO","BARE"),],
       aes(date_time, mean.val,colour=SN))+
  geom_line()+
  facet_grid(paste(variable,veg,sep="_")~., scales="free_y")


# look at precip from tower and SN
ggplot(env_30min[variable == "precip.tot"& veg=="BARE",], aes(date_time, mean.val,colour=SN))+
  geom_line()+
  facet_grid(paste(variable,location,veg,sep="_")~., scales="free_y")

# look at heat flux plate data from tower
ggplot(env_30min[variable == "hfp",], aes(date_time, mean.val,colour=veg))+
  geom_line()+
  facet_grid(paste(variable,location,height,sep="_")~., scales="free_y")

# look at pressure from the Sn and tower
ggplot(env_30min[variable == "atm_press",], aes(date_time, mean.val,colour=location))+
      geom_line()+
      facet_grid(paste(variable,location,sep="_")~., scales="free_y")


# FOR BIOMET 
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

ggplot(SN_30min[variable %in% c("precip.tot") & veg=="BARE" &year(date_time)==year_file,],aes(date_time,mean.val,colour=veg))+geom_point()
ggplot(env_30min[variable %in% c("precip.tot") & veg=="BARE" &year(date_time)==year_file,],aes(date_time,mean.val,colour=datastream))+geom_point()

ggplot(biomet_mean_precip[year(date_time)==year_file,],aes(date_time,P_rain_1_1_1))+geom_point()

# soilmoisture & soiltemp: average from tower and SN regardless of depth or veg
# soil moisture = soil water content
# remove values <0 (get rid of this on 2023-07-27 because this is too early in the process & Talveer needs <0 to rectify)

biomet_mean_soilM <- copy(env_30min[variable %in% c("soilmoisture"),])

# for biomet soil moisture averaging look at data
ggplot(biomet_mean_soilM[year>2019], aes(date_time,mean.val, colour=factor(depth)))+
  geom_line()+
  facet_grid(paste(location,veg)~.)

#  calculate average across depths and veg for biomet
biomet_mean_soilM <- biomet_mean_soilM[,list(SWC_1_1_1 = mean(mean.val, na.rm=TRUE)),
            by="date_time"]

ggplot(biomet_mean_soilM, aes(date_time, SWC_1_1_1))+geom_line()


# soil temperature = Ts
biomet_mean_soilT <- env_30min[variable %in% c("soiltemp"),
                           list(Ts_1_1_1 = mean(mean.val, na.rm=TRUE)),
                           by="date_time"]

ggplot(biomet_mean_soilT, aes(date_time, Ts_1_1_1))+geom_line()


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

setnames(biomet, c("date_time"),c("date_time"))

# save Biomet Data for EddyPro by year to server

# # load function to format data for Eddy Pro and save each year
setwd("/Volumes/SEL_Data_Archive/Research Data/Desert/Jornada/Bahada/Tower/EddyCovariance_ts/EddyPro_Biomet/")

source("~/Desktop/R/R_programs/Functions/SaveFiles_Biomet_EddyPro.R")

# save
savebiomet(biomet,year_file,year_file)


##################################################
# BIOMET2: Expanded format for Ameriflux submission
# call it biomet2
# 2020-02-14: for biomet2 to merge after EddyPro processing (and data filtering, no u*)
#             report all individual sensorns for Ameriflux 
# 2020-04-14: date_time is the adjusted timestamp to make all data match MST

# 2021-05-04 12:44:00 start CS650 soil moisture and temperature inclusion from -100 to -11 cm

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


ggplot(biomet2[variable %in% c("par") & veg %in% c("LATR","PRGL","DAPU","MUPO","BARE")],
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
ggplot(env_30min[variable %in% c("precip.tot") & veg=="BARE" ], aes(date_time, mean.val, colour=SN))+
  geom_line()+
  facet_grid(location+veg~.)
                                

biomet2[variable %in% c("precip.tot") & veg=="BARE" & location=="tower",
        ameriflux.id := "P_RAIN_1_1_1"]

biomet2[variable %in% c("precip.tot") & veg=="BARE" & location=="SN" & SN=="SN2",
        ameriflux.id := "P_RAIN_2_1_1"]

biomet2[variable %in% c("precip.tot") & veg=="BARE" & location=="SN" & SN=="SN6",
        ameriflux.id := "P_RAIN_3_1_1"]

# NA in ameriflux.id are the rainbuckets underneath shrubs
ggplot(biomet2[variable %in% c("precip.tot"),], aes(date_time, mean.val))+
  geom_line()+facet_grid(ameriflux.id~.)

# Soil moisture
# report SWC seperately for each depth and veg type only from SN (2021 and 2022 add CS650, see below for SWC_5)
# because I do not know depth of tower sensors
ggplot(biomet2[variable %in% c("soilmoisture") & veg %in% c("LATR","PRGL","MUPO","SHRUB", "BARE") & location=="SN",],
       aes(date_time, mean.val))+
  geom_line()+
  facet_grid(veg+height~.)


# BARE: 1
# LATR: 2
# MUPO: 3
# PRGL: 4

#### Do Not Run, Need to keep SWC < 0 for Talveer to do rectification steps
#### 2020-04-14: convert all VWC to % soil moisture, requested by Ameriflux QA/QC check.
### biomet2[variable %in% c("soilmoisture"), mean.val := mean.val*100]
#### make sure all SWC are >0
### biomet2[variable %in% c("soilmoisture") & mean.val<0, mean.val := NA]


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


# add tower data from CSS650 sensors: 5
biomet2[variable %in% c("soilmoisture") & (veg == "BARE" & location =="tower" & height=="-11.5"),
        ameriflux.id := "SWC_5_1_1"]

biomet2[variable %in% c("soilmoisture") & (veg == "BARE" & location =="tower" & height=="-17.5"),
        ameriflux.id := "SWC_5_2_1"]

biomet2[variable %in% c("soilmoisture") & (veg == "BARE" & location =="tower" & height=="-25.5"),
        ameriflux.id := "SWC_5_3_1"]

biomet2[variable %in% c("soilmoisture") & (veg == "BARE" & location =="tower" & height=="-42.5"),
        ameriflux.id := "SWC_5_4_1"]

biomet2[variable %in% c("soilmoisture") & (veg == "BARE" & location =="tower" & height=="-100.5"),
        ameriflux.id := "SWC_5_5_1"]

ggplot(biomet2[variable %in% c("soilmoisture"),], aes(date_time, mean.val))+
  geom_line()+
  facet_grid(ameriflux.id~.)

# soil temperature = Ts
# calculate average 'surface': 5-15cm Ts_1
#           average 'deep': 20cm Ts_2
# then merge the wide format biomet2 with the Ts_1 and Ts_2 dataframes
# from 2021 onward: report CS650 probes as depth-specific temperature measurements

ggplot(biomet2[variable %in% c("soiltemp") ,],
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

# add tower data from CSS650 sensors: 5
biomet2[variable %in% c("soiltemp") & (veg == "BARE" & location =="tower" & height=="-11.5"),
        ameriflux.id := "TS_1_1_1"]

biomet2[variable %in% c("soiltemp") & (veg == "BARE" & location =="tower" & height=="-17.5"),
        ameriflux.id := "TS_1_2_1"]

biomet2[variable %in% c("soiltemp") & (veg == "BARE" & location =="tower" & height=="-25.5"),
        ameriflux.id := "TS_1_3_1"]

biomet2[variable %in% c("soiltemp") & (veg == "BARE" & location =="tower" & height=="-42.5"),
        ameriflux.id := "TS_1_4_1"]

biomet2[variable %in% c("soiltemp") & (veg == "BARE" & location =="tower" & height=="-100.5"),
        ameriflux.id := "TS_1_5_1"]

ggplot(biomet2[variable %in% c("soiltemp") ,],
       aes(date_time, mean.val))+
  geom_line()+
  facet_grid(ameriflux.id~.)

# soil heat flux plates: separate by depth and veg types
# shrub = 1 10cm SHF_1_1_1
# shrub = 1 15cm SHF_1_2_1
# bare = 2 10cm SHF_2_1_1
# bare = 2 15cm SHF_2_2_1

ggplot(biomet2[variable %in% c("hfp")], aes(date_time, mean.val))+geom_line()+
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
ggplot(biomet2[variable %in% c("atm_press")], aes(date_time, mean.val))+geom_line()+
  facet_grid(location~.)


biomet2[variable %in% c("atm_press") & location=="tower",
        ameriflux.id := "PA_1_1_1"]

biomet2[variable %in% c("atm_press") & location=="SN",
        ameriflux.id := "PA_2_1_1"]

# leaf wetness 
ggplot(biomet2[variable %in% c("lws","lws_5m")], aes(date_time, mean.val,colour=veg))+geom_line()+
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

biomet2[variable %in% c("lws_5m") & location=="tower" ,
        ameriflux.id := "LEAF_WET_1_1_1"]

biomet2[variable %in% c("lws") & location=="tower",
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
ggplot(biomet2[!is.na(ameriflux.id) ,], aes(date_time, mean.val))+
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

# 15 Apr 2020: save by year!
# 27 Dec 2020: save updated by year. 

saveyears <- function(data,startyear,endyear) {

  for (i in startyear:endyear) {
    data_save <- data[year(date_time)==i,]
    
    write.table (data_save,
                 file=paste("Biomet_USJo1_wide",i, ".csv",sep="_"),
                 sep =',', dec='.', row.names=FALSE,quote=FALSE)
  }}

# save 
# saveyears(biomet2_wide,year_file,year_file)

## also save to Biomet One Drive preliminary folder
# setwd("~/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Tower Data/JER_Bajada/EddyCovarianceTower/Biomet/Preliminary")

# saveyears(biomet2_wide,year_file,year_file)

#### End of routine processing ####



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

