# Calculate EC Footprints using Klujn et al 2015
# Kljun, N., P. Calanca, M.W. Rotach, H.P. Schmid, 2015: A simple two‐dimensional parameterisation for Flux Footprint Prediction (FFP). Geosci. Model Dev., 8, 3695‐3713. doi:10.5194/gmd‐8‐3695‐2015. 

# link to Heidi's code for graphing the ffps
# https://github.com/HRodenhizer/thermokarst_r_code/commit/e39bca8d992c97227c8908c7cdae391d8c10ca90#diff-d2bba19769ec9c07b1504633ab91cae7e3b5a8574c0082d3ad848dd5af378b75 

# load libraries
library(data.table)
library(lubridate)
library(ggplot2)
library(bit64)
library(dplyr)
library(tidyr)
library(purrr)
library(fields)

setwd("~/Desktop/R/R_programs/Tweedie/Jornada/Jornada_Tweedie/Jornada_EddyCovariance_Data/FFP_R_Klujn")

# load sample EML data from Heidi data

eml.sample <- fread("ffp_ready_2017.csv")

# load US-JO1 data
# lat: 32.581954
# long: -106.635017

# import filtered flux data file from Eddy Pro as data table
# filtered in: Jornada_EddyPro_Output_Fluxnext_2010_2019.R
setwd("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_EddyPro_filtered")

# import data that was filtered by 3SD filter with timestamp corrected
load("JER_flux_2010_2019_EddyPro_Output_filtered_SD_TIMEcorr_20200427.Rdata")

# TIMESTAMP_START = TIMESTAMP_START_correct
# TIMESTAMP_END = TIMESTAMP_END_correct
setnames(flux_filter_sd,c("TIMESTAMP_START_correct","TIMESTAMP_END_correct"),
         c("TIMESTAMP_START","TIMESTAMP_END"))

# convert date to POSIXct and get a year, day, hour column
# if this step doesn't work, make sure bit64 library is loaded otherwise the timestamps import in a non-sensical format
flux_filter_sd[,date_time := NULL]
flux_filter_sd[,':=' (date_time = parse_date_time(TIMESTAMP_END,"YmdHM",tz="UTC"))]

# there's duplicated data in 2012 DOY 138
flux_filter_sd <- (flux_filter_sd[!(duplicated(flux_filter_sd, by=c("date_time")))])

# Should use U* filtered data so import ReddyProc data to get Ustar thresholds
ep.units <- (fread("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/ReddyProc/20200427/REddyResults_Us-Jo1_20200428_586625386/output.txt",
                   header=TRUE))[1,]

flux.ep <- fread("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/ReddyProc/20200427/REddyResults_Us-Jo1_20200428_586625386/output.txt",
                 header=FALSE, skip=2,na.strings=c("-9999", "NA","-"),
                 col.names = colnames(ep.units))

# for some reason na.strings won't recognize the -9999
flux.ep[flux.ep == -9999] <- NA

# create a date format variable for ReddyProc data
filled <- expand.grid(date=seq(as.Date("2010-01-01"),as.Date("2019-12-31"), "days"),
                      Hour=seq(0,23.5, by=0.5))

filled <- data.table(date_time = seq(as.POSIXct("2010-01-01 00:00:00"),
                                        as.POSIXct("2019-12-31 23:30:00"), "30 min"))

filled <- filled[,':='(Year = year(date_time),
                       DoY=yday(date_time),
                       Hours = hour(date_time),
                       Mins = minute(date_time))]

filled[Mins==0, Hour := Hours+0.0]
filled[Mins==30, Hour := Hours+0.5]
filled[,':=' (Mins = NULL, Hours=NULL)]

# combine with filled to get a date_time column
flux.ep <- merge(flux.ep,filled,by=c("Year","DoY","Hour"), all=TRUE)

# get Ustar Threshold values from flux.ep and merge with flux_filter_sd
flux_filter_sd <- merge(flux_filter_sd,
                        flux.ep[,c("date_time","Ustar_Thres")], by="date_time", all.x = TRUE)

# see UStar threshold in figure format
ggplot()+
  geom_point(aes(x=yday(date_time), y=FC),data=flux_filter_sd[USTAR>Ustar_Thres,], colour="grey")+
  geom_point(aes(x=yday(date_time), y=FC),data=flux_filter_sd[USTAR<Ustar_Thres,], colour="pink")
  
# create new dataframe with only data above Ustar_Thres
ffp_data <- copy(flux_filter_sd[USTAR>Ustar_Thres,])

# create data for the Klujn webtool to calculate flux climatologies
# https://geography.swansea.ac.uk/nkljun/ffp/www/

# HEADER-line is NOT read, the order of columns is crucial
# since header-line is not read, no need to rename columns

# yyyy = Year [eddypro: year(date_time)]
# mm = Month [1-12] [eddypro: month(date_time)]
# day = Day of month [1-31] [eddypro: mday(date_time)]
# HH = Hour [0-23] or [1-24] in UTC, NOT local time [eddypro: hour(date_time) - 7 for MST to UTC, if that's REALLY what they mean....]
# MM = minutes [0 30] [eddypro: minute(date_time)]
# zm = measurement height above ground [m] = 5
# d = Displacement height [m] ~ 0.67*zm = 3.35 [eddypro:DISPLACEMENT_HEIGHT]
# z0 = Roughness length [m], enter [-999] if unknown [eddypro:ROUGHNESS_LENGTH]
# u_mean = mean wind speed at zm (ms-1) [eddypro:WS]
# L = Obhukov length (m) [eddypro:MO_LENGTH]
# sigma_v = std deviation of lateral velocity fluctuations (ms-1) [eddypro:V_SIGMA]
# u_star = friction velocity (ms-1) [eddypro:USTAR]
# wind_dir = wind direction in degrees (360) for rotation of footprint [eddypro:WD]

ffp_clim <- copy(ffp_data[,c("date_time","DISPLACEMENT_HEIGHT","ROUGHNESS_LENGTH","WS",
                             "MO_LENGTH","V_SIGMA","USTAR","WD","SW_IN_1_1_1","SW_IN_POT")])

ggplot(ffp_clim, aes(date_time,USTAR))+geom_point(size=0.5)

# add zm and date variables, if they really mean UTC then add -hours(7) to each date_time
ffp_clim <- ffp_clim[,':='(zm=5,
                           Year = year(date_time-hours(7)),
                           Month = month(date_time-hours(7)),
                           mDay = mday(date_time-hours(7)),
                           Hour = hour(date_time-hours(7)),
                           Minutes = minute(date_time-hours(7)))]

# have a quick look
ggplot(ffp_clim, aes(Hour,SW_IN_1_1_1))+geom_point(size=0.5)
ggplot(ffp_clim, aes(Hour,SW_IN_POT))+geom_point(size=0.5)

# create day/night and 3 month season columns (JJA, SON, DJF, MAM) and use these to save
# in parts of 2010 the SW_IN measured is missing, use SW_IN_POT for now
ffp_clim[SW_IN_1_1_1>10, day_night := "day"]
ffp_clim[SW_IN_1_1_1<=10, day_night := "night"]
ffp_clim[is.na(day_night)&SW_IN_POT>10, day_night := "day"]
ffp_clim[is.na(day_night)&SW_IN_POT<=10, day_night := "night"]
# add season labels
ffp_clim[Month %in% c("6","7","8"),season_3 := "jja"]
ffp_clim[Month %in% c("9","10","11"),season_3 := "son"]
ffp_clim[Month %in% c("12","1","2"),season_3 := "djf"]
ffp_clim[Month %in% c("3","4","5"),season_3 := "mam"]
# combine day/night and season into a label
ffp_clim[,dn_sn := paste(day_night,season_3,sep="_")]

# quick visual check that labeling worked
ggplot(ffp_clim, aes(Hour,USTAR))+geom_point()+facet_grid(day_night~season_3)
ggplot(ffp_clim, aes(Hour,USTAR))+geom_point()+facet_grid(dn_sn~.)

# save data as csv by dn_sn using group_walk
# https://luisdva.github.io/rstats/export-iteratively/
# https://community.rstudio.com/t/map-write-csv/33292/2
# https://stackoverflow.com/questions/41233173/how-can-i-write-dplyr-groups-to-separate-files

setwd("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/FFP/Online")

ffp_clim %>%
  group_by(dn_sn) %>%
  select(Year,Month,mDay,Hour,Minutes,zm,DISPLACEMENT_HEIGHT,
         ROUGHNESS_LENGTH,WS,MO_LENGTH,V_SIGMA,USTAR,WD) %>%
  group_walk(~write.table(.x, file= paste("ffp_clim_online_",.y$dn_sn,
                         ".csv",sep=""),
             sep =',', dec='.', row.names=FALSE, na="-999", quote=FALSE))


# also save only 2019 in seasons, for most recent year comparison
ffp_clim %>%
  group_by(season_3) %>%
  select(Year,Month,mDay,Hour,Minutes,zm,DISPLACEMENT_HEIGHT,
         ROUGHNESS_LENGTH,WS,MO_LENGTH,V_SIGMA,USTAR,WD) %>%
  filter(Year==2019) %>%
  group_walk(~write.table(.x, file= paste("ffp_clim_2019_online_",.y$season_3,
                                          ".csv",sep=""),
                          sep =',', dec='.', row.names=FALSE, na="-999", quote=FALSE))

# process half-hourly footprints with R code, in-line

# metadata columns to use for FFP (static, do not come from data)
# enter latitude 32 degree 33' 59.400" N
ffp_data$BADM_LOCATION_LAT[1]

# enter measurement height here and then populate metadata
# measurement height (m)
z <- 5.0

# create metadata table
metadata.ffp <- as.data.table(z)

# displacement (m) (eddypro: DISPLACEMENT_HEIGHT)
metadata.ffp[,d := 0.67*z]

# roughness length (m) (eddypro: ROUGHNESS_LENGTH)
metadata.ffp[,z0 := 0.15*z]

# measurement height above displacement 
metadata.ffp[,zm := z-d]

# mean wind speed at zm (ms-1)
ffp_data$WS

# boundary layer height (m)
# h is caluclated in FFP function

# Obhukov length (m) 
ffp_data$MO_LENGTH[1]

# std deviation of lateral velocity fluctuations (ms-1)
ffp_data$V_SIGMA[1]

# friction velocity (ms-1)
flux$USTAR[1]

# wind direction
ffp_data$WD

# Try running model for 1 half-hour

FFP <- calc_footprint_FFP(#lat=ffp_data$BADM_LOCATION_LAT[100],
                              zm= z - 0.67,
                              z0=ffp_data$ROUGHNESS_LENGTH[100],
                              umean=NaN, # if z0 is given, it will be used instead of umean
                              ol=ffp_data$MO_LENGTH[100], 
                              sigmav=ffp_data$V_SIGMA[100], 
                              ustar=ffp_data$USTAR[100], 
                              wind_dir=ffp_data$WD[100],
                              r=NULL,
                              nx = NULL,
                              rslayer = NULL,
                              crop = NULL)

# plot footprint
ffp_x <- c(FFP$x_2d)
ffp_y <- c(FFP$y_2d)
ffp_f <- c(FFP$f_2d)

quilt.plot(ffp_x,ffp_y,ffp_f,nx=1000,xlim=c(-100,150),ylim=c(-100,150))
for(i in 1:8)lines(FFP$xr[[i]],FFP$yr[[i]],type="l",col="red")


test <- ffp_data %>%
  filter(as.Date(date_time) == as.Date("2012-07-01"))%>%
  group_by(.,date_time) %>%
  tidyr::nest() %>% 
  mutate(ffp = purrr::map(data,~calc_footprint_FFP(lat=.x$BADM_LOCATION_LAT,
                        zm= 5 - 0.67,
                        z0=.x$ROUGHNESS_LENGTH,
                        umean=NaN, # if z0 is given, it will be used instead of umean
                        ol=.x$MO_LENGTH, 
                        sigmav=.x$V_SIGMA, 
                        ustar=.x$USTAR, 
                        wind_dir=.x$WD,
                        r=NULL,
                        nx = NULL,
                        rslayer = NULL,
                        crop = NULL)))

# plot footprint
ffp.hh <- test$ffp[[12]]

ffp_x <- c(ffp.hh$x_2d)
ffp_y <- c(ffp.hh$y_2d)
ffp_f <- c(ffp.hh$f_2d)

quilt.plot(ffp_x,ffp_y,ffp_f,nx=1000,xlim=c(-100,1000),ylim=c(-100,1000))
for(i in 1:8)lines(ffp.hh$xr[[i]],ffp.hh$yr[[i]],type="l",col="red")


test2 <- eml.sample %>%
  filter(yyyy==2017 & mm==5 & day==1) %>%
  group_by(HH,MM) %>%
  tidyr::nest() %>% 
  mutate(ffp = purrr::map(data,~calc_footprint_FFP(lat=32.6,
                                                   zm= .x$zm,
                                                   z0=.x$z0,
                                                   umean=.x$u_mean, # if z0 is given, it will be used instead of umean
                                                   ol=.x$L, 
                                                   sigmav=.x$sigma_v, 
                                                   ustar=.x$u_star, 
                                                   wind_dir=.x$wind_dir,
                                                   r=NULL,
                                                   nx = NULL,
                                                   rslayer = NULL,
                                                   crop = NULL)))
