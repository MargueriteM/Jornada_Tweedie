#########################################
#     Jornada Environmental Data        #
# combine data from Met Tower,          #
# Sensor networks, soil sensors         #
# data from multiple years are combined #
# in seperate R codes                   #
#     written by: M. Mauritz            #
#          April 2019                   #
#########################################

# TO DO: 
# for tower precip data try mean(1 min by 5 mins) -> sum of 5 mins
# depths for ECTM?! And cover type
# lws units in tower and SN..... ?
# check tower PAR data filter.... eg: in 2013 and 2018 (check all), did I delete something wrong? many midday are missin
# get heatflux data and lws_1 from flux data tables
# fix depth so it can be numeric

# play with figure output. 

# Data:
# Path: ⁨Macintosh HD⁩ ▸ ⁨Users⁩ ▸ ⁨memauritz⁩ ▸ ⁨Desktop⁩ ▸ ⁨R⁩ ▸ ⁨R_programs⁩ ▸ ⁨Tweedie⁩ ▸ ⁨Jornada⁩ ▸ ⁨Jornada Climate Dat
# Tower Data: Jornada_Climate_Data.R
# Sensor Network Data: Jornada_SensorNetworkData_2010_2019.R
# Soil Sensor Data: Jornada_ECTM_Data.R

# very obviously bad sensor data was removed from the individual data streams.

# Sensor network data:
setwd("~/Desktop/TweedieLab/Projects/Jornada/Data/SensorNetwork/Combined")
SN_30min <- fread("SensorNetwork_L1_2010_2019_30min.csv", sep=",", header=TRUE)
# format date and add column to deginate the data stream
SN_30min[, ':=' (date_time = ymd_hms(date_time), datastream = "SN")]
setnames(SN_30min, 'sensor', 'variable')
# change variable names to match other datastreams
SN_30min[variable == "rain", variable := "precip.tot"]
SN_30min[variable == "moisture", variable := "soilmoisture"]
# make depth in SN negative to indicate 'below the surface'
SN_30min[variable=="soilmoisture" & depth=="5", depth:="-5"]
SN_30min[variable=="soilmoisture" & depth=="10", depth:="-10"]
SN_30min[variable=="soilmoisture" & depth=="20", depth:="-20"]
SN_30min[variable=="soilmoisture" & depth=="30", depth:="-30"]

# Tower Met Data
setwd("~/Desktop/TweedieLab/Projects/Jornada/Data/Tower/Climate/Compiled")
met_30min <- fread("TowerMet_L1_2010_2019_30min.csv", sep=",", header=TRUE)
# format date and add column to deginate the data stream
met_30min[, ':=' (date_time = ymd_hms(date_time), datastream = "tower")]
setnames(met_30min, 'value', 'mean.val')

met_30min[variable=="par", veg := "UP"]
met_30min[variable %in% c("lws","airtemp"), ':=' (veg = "BARE", depth = "500")]
met_30min[variable=="precip.tot", veg := "BARE"]

# Tower soil temperature and moisture data (ECTM)
setwd("~/Desktop/TweedieLab/Projects/Jornada/Data/SoilSensor_ECTM/Combined")
soil_30min <- fread("Soil_Temp_VWC_ECTM_L1_2010_2019_30min.csv", sep=",", header=TRUE)
# format date and add column to deginate the data stream, get rid or uneccessary columns
setnames(soil_30min, c('value','variable'), c('mean.val', 'probe_id'))
soil_30min[measurement == "t", variable := "soiltemp"]
soil_30min[measurement == "vwc", variable := "soilmoisture"]

# make a guess at depths
soil_30min[rep%in% c(8,3), depth := "-2"]
soil_30min[rep%in% c(1,6), depth := "-10"]
soil_30min[rep%in% c(5,7), depth := "-15"]
soil_30min[rep%in% c(2,4), depth := "-20"]

# assign veg type (should be shrub/bare, once I know.)
# for now assign aribtratryt
soil_30min[rep %in% c(1,3,7,4), veg := "BARE"]
soil_30min[rep %in% c(2,5,6,8), veg := "SHRUB"]

# modify columns to match other datastreams and get rid of redundant ones
soil_30min[, ':=' (date_time = ymd_hms(date_time), datastream = "ectm",
                   diff=NULL, diff2=NULL, date=NULL, rep=NULL, measurement=NULL)]

# combine all three data streams
env_30min <- rbind(SN_30min,met_30min,soil_30min, fill=TRUE)

# check levels of variable column
levels(factor(env_30min$variable))


# soil moisture
ggplot(env_30min[variable=="soilmoisture"&year==2015,], aes(date_time, mean.val))+
  geom_line()+
  facet_grid(paste(veg,datastream,sep="_")~.)
 
# precip 
ggplot(env_30min[variable=="precip.tot"&year==2018,], aes(date_time, mean.val,colour=veg))+
  geom_line()+
  facet_grid(paste(veg,variable,datastream,sep="_")~.)

# precip, rh, and lws
ggplot(env_30min[year==2013 & month>6&month<10 & (variable == "precip.tot"& veg=="BARE" |
                   variable %in% c("rh","lws")),], aes(date_time, mean.val,colour=variable))+
  geom_line()+
  facet_grid(paste(variable,datastream,sep="_")~., scales="free_y")

# soil moisture and precip
ggplot(env_30min[year==2013 &
      (variable=="soilmoisture" | (variable == "precip.tot"& veg=="BARE")),],
      aes(date_time, mean.val, colour=depth))+
  geom_line()+
  facet_grid(paste(variable,veg,datastream,sep="_")~.,scales="free_y")


# soil and air temperature
ggplot(env_30min[variable %in% c("airtemp","soiltemp")&year==2015&month==7], aes(date_time, mean.val,colour=depth))+
  geom_point()+
  facet_grid(paste(variable,datastream,veg,sep="_")~., scales="free_y")

# par
ggplot(env_30min[variable=="par"&year==2018&month %in% c(7,8,9),], aes(date_time, mean.val,colour=veg))+
  geom_line()+
  facet_grid(paste(veg,variable,datastream,sep="_")~., scales="free_y")

