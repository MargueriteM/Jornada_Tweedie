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

# combine all three data streams
env_30min <- rbind(SN_30min,met_30min,flux_30min,soil_30min, fill=TRUE)

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

# check
ggplot(precip,aes(date_time,tower))+geom_line()
ggplot(precip,aes(date_time,SN2))+geom_line()
ggplot(precip,aes(date_time,SN6))+geom_line()

ggplot(precip,aes(tower,SN2))+geom_point()
ggplot(precip,aes(tower,SN6))+geom_point()
ggplot(precip,aes(SN2,SN6))+geom_point()

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


# BIOMET: Expanded format for Ameriflux submission
# call it biomet2
# 2020-02-14: for biomet2 to merge after EddyPro processing (and data filtering, no u*)
#             report all individual sensorns for Ameriflux 

# PAR UP from tower and SN = PPFD
biomet_mean_parUP <- env_30min[variable %in% c("par") & veg=="UP",
                               list(PPFD_1_1_1 = mean(mean.val, na.rm=TRUE)),
                               by="date_time"]

# PAR reflecteed from SN = reflected PPFD
biomet_mean_parVEG <- env_30min[variable %in% c("par") & veg %in% c("LATR","PRGL","DAPU","MUPO","BARE"),
                                list(PPFDr_1_1_1 = mean(mean.val, na.rm=TRUE)),
                                by="date_time"]

# precip: BARE from tower and SN = P_rain
biomet_mean_precip <- env_30min[variable %in% c("precip.tot") & veg=="BARE",
                                list(P_rain_1_1_1 = mean(mean.val, na.rm=TRUE)),
                                by="date_time"]


# report SWC seperately for each depth and veg type
# only from SN because I do not know depth of tower sensors
# averaged for veg and bare, add SWC_X_Y_A, 'A' at the end to indicate aggregate
biomet2_mean_soilM_veg <- env_30min[!is.na(mean.val) & variable %in% c("soilmoisture") & veg %in% c("LATR","PRGL","MUPO","SHRUB"),
                                    list(SWC = mean(mean.val),
                                         SWC_SD = sd(mean.val),
                                         SWC_N = length(mean.val)),
                                    by="date_time,height"]

biomet2_mean_soilM_veg <- dcast(biomet2_mean_soilM_veg,date_time~height, value.var=c("SWC", "SWC_SD", "SWC_N"))

# change column names to EddyPro format
# shrub = 1, bare = 2
setnames(biomet2_mean_soilM_veg,c("SWC_-10","SWC_-15","SWC_-2", "SWC_-20","SWC_-30","SWC_-5",
                                  "SWC_SD_-10","SWC_SD_-15","SWC_SD_-2", "SWC_SD_-20","SWC_SD_-30","SWC_SD_-5",
                                  "SWC_N_-10","SWC_N_-15","SWC_N_-2", "SWC_N_-20","SWC_N_-30","SWC_N_-5"),
         c("SWC_1_3_A","SWC_1_4_A","SWC_1_1_A", "SWC_1_5_A","SWC_1_6_A","SWC_1_2_A",
           "SWC_1_3_SD","SWC_1_4_SD","SWC_1_1_SD", "SWC_1_5_SD","SWC_1_6_SD","SWC_1_2_SD",
           "SWC_1_3_N","SWC_1_4_N","SWC_1_1_N", "SWC_1_5_N","SWC_1_6_N","SWC_1_2_N"))

biomet2_mean_soilM_bare <- env_30min[!is.na(mean.val) & variable %in% c("soilmoisture") & veg %in% c("BARE"),
                                     list(SWC = mean(mean.val),
                                          SWC_SD = sd(mean.val),
                                          SWC_N = length(mean.val)),
                                     by="date_time,height"]


# soil temperature = Ts
biomet2_mean_soilT <- env_30min[variable %in% c("soiltemp"),
                                list(Ts_1_1_A = mean(mean.val, na.rm=TRUE)),
                                by="date_time"]

# soil heat flux plates: separate by depth and veg types
# shrub = 1, bare = 2
biomet_mean_hfp <- env_30min[variable %in% c("hfp"),veg_depth:= paste(veg,height,sep="_")][
  variable %in% c("hfp"),
  list(SHF = mean(mean.val, na.rm=TRUE)),
  by="date_time,veg_depth"]

biomet_mean_hfp <- dcast(biomet_mean_hfp,date_time~veg_depth, value.var="SHF")


# pressure: SN and tower
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

