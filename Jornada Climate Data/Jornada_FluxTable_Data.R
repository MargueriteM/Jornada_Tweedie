###################################################
# This code processes pre-calc Flux Data from JER #
#                                                 #
# Data come from:                                 #
#                * dataL1_flux                    #
#  Get data from SEL Drive, view time series,     #
# identify sensor gaps, remove bad data           #
# extract data for Eddy Pro:                      #
#                     lws_2_Avg	mV                #
#                     Rs_downwell_Avg	W/m^2       #
#                     Rs_upwell_Avg	W/m^2         #
#                     Rl_downwell_Avg	W/m^2       #
#                     Rl_upwell_Avg	W/m^2         #
#                     Rn_nr_Avg W/m^2             #
#                     hfp01_1_Avg	W/m^2           #
#                     hfp01_2_Avg	W/m^2           #
#                     hfp01_3_Avg	W/m^2           #
#                     hfp01_4_Avg	W/m^2           #
#                                                 #
#    written by: Marguerite Mauritz               #
#                8 May, 2019                      #
###################################################

# 26 Dec 2020 update: add Ameriflux QA/QC to rescale LWS between 0-100
# 6 Apr 2020 update: update to March 24 and add timestamp correction due to daylight savings changes
# JER_Tower_SN_TimestampMismatches.xlsx
# make the timestamp adjustments at the end, after filtering data. 


# load libraries
library(data.table)
library(ggplot2)
library(lubridate)
library(gridExtra)
library(lattice)


# copied files from server to computer
# server source: smb://prodgis04.utep.edu/SEL_Data_Archive/Research Data/Desert/Jornada/UtepJrnBahadaSite/Tower/Flux/

# mainly want to get
# get lws_1, soil heat flux data, Rs and Rl components

# according to notes in Aline's logs 2011-06-03 radiometer was upgraded from CNR1 to CNR4
# in the data there is a break and pattern change ~2012-08-25 Rl swtiched from low values to high values


# Units of data:
# timestamp: yyyy/mm/dd HH:MM:SS
# RECORD	RN
# Hs	W/m^2 Sensible heat flux using sonic temperature.
# H	W/m^2 Sensible heat flux using the fine wire thermocouple.
# Fc_wpl	mg/(m^2 s) Carbon dioxide flux (LI-7500), with Webb et al. term.
# LE_wpl	W/m^2 Latent heat flux (LI-7500), with Webb et al. term.
# Hc	W/m^2 Sensible heat flux computed from Hs and LE_wpl.
# tau	kg/(m s^2) Momentum flux.
# u_star	m/s Friction velocity.
# Ts_mean	C
# stdev_Ts	C
# cov_Ts_Ux	m C/s
# cov_Ts_Uy	m C/s
# cov_Ts_Uz	m C/s
# CO2_mean	mg/m^3
# stdev_CO2	mg/m^3
# cov_CO2_Ux	mg/(m^2 s)
# cov_CO2_Uy	mg/(m^2 s)
# cov_CO2_Uz	mg/(m^2 s)
# H2O_Avg	g/m^3
# stdev_H2O	g/m^3
# cov_H2O_Ux	g/(m^2 s)
# cov_H2O_Uy	g/(m^2 s)
# cov_H2O_Uz	g/(m^2 s)
# fw_Avg	C
# stdev_fw	C
# cov_fw_Ux	m C/s
# cov_fw_Uy	m C/s
# cov_fw_Uz	m C/s
# Ux_Avg	m/s
# stdev_Ux	m/s
# cov_Ux_Uy	(m/s)^2
# cov_Ux_Uz	(m/s)^2
# Uy_Avg	m/s
# stdev_Uy	m/s
# cov_Uy_Uz	(m/s)^2
# Uz_Avg	m/s
# stdev_Uz	m/s
# press_Avg	kPa
# atm_press_mean	kPa
# t_hmp_mean	C
# H2O_hmp_mean	g/m^3 Mean HMP45C vapor density.
# rh_hmp_mean	percent Mean HMP45C relative humidity.
# rho_a_mean	kg/m^3 Mean air density.
# wnd_dir_compass	degrees
# wnd_dir_csat3	degrees
# wnd_spd	m/s
# rslt_wnd_spd	m/s
# std_wnd_dir	degrees
# Fc_irga	mg/(m^2 s) Carbon dioxide flux (LI-7500), without Webb et al. term.
# LE_irga	W/m^2 Latent heat flux (LI-7500), without Webb et al. term.
# CO2_wpl_LE	mg/(m^2 s) Carbon dioxide flux (LI-7500), Webb et al. term due to latent heat flux.
# CO2_wpl_H	mg/(m^2 s) Carbon dioxide flux (LI-7500), Webb et al. term due to sensible heat flux.
# H2O_wpl_LE	W/m^2 Latent heat flux (LI-7500), Webb et al. term due to latent heat flux.
# H2O_wpl_H	W/m^2 Latent heat flux (LI-7500), Webb et al. term due to sensible heat flux.
# n_Tot	samples
# csat_warnings	samples
# irga_warnings	samples
# del_T_f_Tot	samples
# sig_lck_f_Tot	samples
# amp_h_f_Tot	samples
# amp_l_f_Tot	samples
# chopper_f_Tot	samples
# detector_f_Tot	samples
# pll_f_Tot	samples
# sync_f_Tot	samples
# agc_Avg	unitless
# agc_thrshld_excded_Tot	samples
# lws_1_Avg	mV # in a shrub
# lws_2_Avg	mV # at ~5m height
# Rn_nr_Avg	W/m^2
# albedo_Avg	unitless
# Rs_downwell_Avg	W/m^2 # SW_OUT, this is not downwelling, it's downward pointing sensor
# Rs_upwell_Avg	W/m^2 # SW_IN, this is not upwelling, it's upwared pointing sensor
# Rl_downwell_Avg	W/m^2  # LW_OUT, this is not downwelling, it's downward pointing sensor
# Rl_upwell_Avg	W/m^2 # LW_IN, this is not upwelling, it's upwared pointing sensor
# T_nr_Avg	K
# Rl_down_meas_Avg	W/m^2 # not temperature corrected (can use for net calculations because temp term cancels)
# Rl_up_meas_Avg	W/m^2 # not temperature corrected (can use for net calculations because temp term cancels)
# par_Avg	umol/m/s
# I think depths on HFP are switched (assuming that open/bush labels are correct, I think it's:)
# hfp01_1_Avg	W/m^2 Soil heat flux (5cm/15cm) 15 open ... (channel 3H/L. Field label: 10O)
# hfp01_2_Avg	W/m^2 Soil heat flux (5cm/15cm) 10 open ... (channel 6H/L. Field label: 15O)
# hfp01_3_Avg	W/m^2 Soil heat flux (5cm/15cm) 15 bush ... (channel 10H/L. Field label: 15B)
# hfp01_4_Avg	W/m^2 Soil heat flux (5cm/15cm) 10 bush ... (channel 8L (=16 SE). Field label: 10B) *single-ended
# n_mux	samples
# precip_Tot	mm
# hor_wnd_spd_mean	m/s
# hor_wnd_spd_mean_rslt	m/s
# hor_wnd_dir_mean_rslt	Deg
# hor_wnd_dir_stdev	Deg
# panel_temp_Avg	C
# batt_volt_Avg	V


setwd("~/Desktop/TweedieLab/Projects/Jornada/Data/Tower/Climate/")

# fread imports as data table
# list all files in relevant folder
fluxfiles <- list.files(path="~/Desktop/TweedieLab/Projects/Jornada/Data/Tower/Flux",
                        full.names=TRUE,pattern="dataL1_flux_") 
# column names
fluxcolnames <- fread(file="~/Desktop/TweedieLab/Projects/Jornada/Data/Tower/Flux/flux_colnames.csv",
                      sep=",",
                      header=TRUE) 

# read files and bind them into one file. fill=TRUE because of the missing columns in 2011
flux_all <- do.call("rbind", lapply(fluxfiles, header = FALSE, fread, sep=",", skip = 4,fill=TRUE,
                                   na.strings=c(-9999,"#NAME?"),
                                   col.names=as.vector(fluxcolnames$colname)))

# convert the time stamp to a posixct format
flux_all[,date_time := parse_date_time(timestamp, c("%Y!-%m-%d %H:%M:%S",
                                                    "%m-%d-%y %H:%M"))]

# change data to long format and drop timestamp and record variables.
flux_long <- melt.data.table(flux_all[,!c("timestamp","record")],c("date_time"))

flux_long[,':=' (year=year(date_time),month=month(date_time),doy=yday(date_time))]

# check data

# heat fluxes, remove very obviously bad data
# hfp01_1_Avg	W/m^2 Soil heat flux (5cm/15cm) 15 open ... (channel 3H. Field label: 10O)
# hfp01_2_Avg	W/m^2 Soil heat flux (5cm/15cm) 10 open ... (channel 6H. Field label: 15O)
# hfp01_3_Avg	W/m^2 Soil heat flux (5cm/15cm) 15 bush ... (channel 10H. Field label: 15B)
# hfp01_4_Avg	W/m^2 Soil heat flux (5cm/15cm) 10 bush ... (channel 8L (=16 SE). Field label: 10B)

# hfp2 has loads of problems. First filter worst and most obvious data out, then remove
# based on outliers with hfp4 regression

# remove hfp2 values < -100
flux_long[variable %in% c("hfp01_2_Avg") &
            value<(-100),
          value:=NA]

# 2010: that data looks really funky prior to June. Patterns are bad, magnitudes are bad and there are 
#      breaks in the data that totally change the pattern. It looks like possible re-wiring or code changes

# 2010: remove feb
flux_long[variable %in% c("hfp01_1_Avg", "hfp01_2_Avg", "hfp01_3_Avg", "hfp01_4_Avg") &
            date_time>as.Date("2010-02-01") &  date_time<as.POSIXct("2010-03-02 00:30", tz="UTC"),value:=NA]

# hfp01_1 and hfp01_3 remove before April 20
flux_long[variable %in% c("hfp01_1_Avg", "hfp01_3_Avg") &
            date_time>as.Date("2010-03-01") & date_time<as.Date("2010-04-20"),value:=NA]

# remove all May 28 to June 2
flux_long[variable %in% c("hfp01_1_Avg", "hfp01_2_Avg", "hfp01_3_Avg", "hfp01_4_Avg") &
            date_time>as.Date("2010-05-28") & date_time<as.Date("2010-06-02"),value:=NA]

# remove all June 29
# remove all May 28 to June 2
flux_long[variable %in% c("hfp01_1_Avg", "hfp01_2_Avg", "hfp01_3_Avg", "hfp01_4_Avg") &
            date_time>as.POSIXct("2010-06-28 17:30", tz="UTC") & date_time<as.POSIXct("2010-06-29 12:00", tz="UTC"),
          value:=NA]


# 2011: mostly OK, some problems in hfp01_2_Avg
# in hfp01_2, remove:
# Jan 14 19:00 to Jan 15 4:00
flux_long <- flux_long[variable %in% c("hfp01_2_Avg") & date_time >= as.POSIXct("2011-01-14 19:00", tz="UTC") & 
                         date_time <= as.POSIXct("2011-01-15 4:00", tz="UTC"), value := NA]

# Jan 16 19:00 to Jan 17 10:00
flux_long <- flux_long[variable %in% c("hfp01_2_Avg") & date_time >= as.POSIXct("2011-01-16 19:00", tz="UTC") & 
                         date_time <= as.POSIXct("2011-01-17 10:00", tz="UTC"), value := NA]


# Jan 17 23:00 to Jan 18 13:30
flux_long <- flux_long[variable %in% c("hfp01_2_Avg") & date_time >= as.POSIXct("2011-01-17 23:00", tz="UTC") & 
                         date_time <= as.POSIXct("2011-01-18 13:30", tz="UTC"), value := NA]

# Jan 18 21:00 to Jan 19 9:00
flux_long <- flux_long[variable %in% c("hfp01_2_Avg") & date_time >= as.POSIXct("2011-01-18 21:00", tz="UTC") & 
                         date_time <= as.POSIXct("2011-01-19 9:00", tz="UTC"), value := NA]

# Jan 19 22:30 to Jan 21 23:30
flux_long <- flux_long[variable %in% c("hfp01_2_Avg") & date_time >= as.POSIXct("2011-01-19 22:30", tz="UTC") & 
                         date_time <= as.POSIXct("2011-01-21 23:30", tz="UTC"), value := NA]


# 2012: mostly OK, some problems in hfp01_2_Avg
# 2013: hfp01_2_Avg has frequent problems with data in between looking good.
# 2011-2013 remove outliers with regression between hfp4 and hfp2

# 2014: hfp01_2_Avg is bad (maybe bad. it has many problems and the magnitude of flux jumps. Not sure I trust it)
# 2015: hfp01_2_Avg is bad (maybe bad. it has many problems and the magnitude of flux jumps. Not sure I trust it)
# remove hfp2 in 2014 and 2015
flux_long[variable %in% c("hfp01_2_Avg") &
            year %in% c(2014,2015),
          value:=NA]

# 2016-2019 remove bad hfp2 data with seperate regression to hfp4 from 2016-2019
# 2016: hfp01_2_Avg is OK with lots of gaps mid-June to mid-October. Lots of data before and after
# remove April and before
flux_long[variable %in% c("hfp01_2_Avg") &
            year==2016 & month<=4,
          value:=NA]

# 2017: hfp01_2_Avg Jan to March pattern looks fine, magnitude is much higher. After August data is very patchy
# leave. might fix with regression
# 2018: hfp01_2_Avg pattern looks fine, magnitude is much higher
# 2019: hfp01_2_Avg pattern looks fine, magnitude is much higher

# heat fluxes
ggplot(flux_long[variable %in% c("hfp01_1_Avg", "hfp01_2_Avg", "hfp01_3_Avg", "hfp01_4_Avg") ,],
       aes(date_time, value))+geom_line()+
  facet_grid(variable~.,scales="free_y")

######## FILTER HFP2 WITH RESIDUALS #########
# Try to filter HFP2 data using the relationship to HFP4:

# use regression between the two deep HFPs to filter hfp01_2
# 1: for 2011-2013  do regression of hfp4 (x) and hfp2 (y) and remove stdresid +/- 1
# 2: for 2016-2019  do regression of hfp4 (x) and hfp2 (y) and remove stdresid +/- 1

# create dataframe with hfp2 and hfp4 in columns
test <- merge(flux_long[variable %in% c("hfp01_2_Avg"),.(year,month,date_time,value)],
              flux_long[variable %in% c("hfp01_4_Avg"),.(date_time,value)],by="date_time")

colnames(test) <- c("date_time","year","month","hfp2","hfp4")

# remove NA values so that residuals can be added to data
test <- test[(!is.na(hfp2)|!is.na(hfp4)) & (!is.na(hfp2) & !is.na(hfp4)),]


# 1: regression of 2011-2013 hfp2 and hfp4
hfp_lm11 <- (lm(hfp2~hfp4,test[year>=2011&year<=2013]))
summary(hfp_lm11)
# y= 2.818 + 1.474*x

# graph
# ggplot(test[year>=2011&year<=2013,], aes(hfp4,hfp2))+
#   geom_point()+
#   geom_abline(intercept=2.818, slope=1.474)

# calculate residuals
test[year>=2011&year<=2013, stdresid11 := rstandard(hfp_lm11)]

# ggplot(test[year>=2011&year<=2013,], aes(hfp2,stdresid11))+geom_point()+geom_hline(yintercept=c(-1.5,1.5))
# 
# ggplot(test[year==2011&month==1,], aes(date_time,hfp2))+geom_point()
# ggplot(test[year>=2011&year<=2013&stdresid11>(-1)&stdresid11<1,], aes(date_time,hfp2))+geom_point()

# remove 2011 stdresids +/- 1.5
test[year>=2011&year<=2013&(stdresid11<(-1.5)|stdresid11>1.5),hfp2 := NA]

# 2: repeat regression for 2016-2019
hfp_lm16 <- (lm(hfp2~hfp4,test[year>=2016&year<=2019]))
summary(hfp_lm16)
# y= 3.359 + 1.913*x

# graph
# ggplot(test[year>=2016&year<=2019,], aes(hfp4,hfp2))+
#   geom_point()+
#   geom_abline(intercept=3.359, slope=1.913)

# calculate residuals
test[year>=2016&year<=2019, stdresid16 := rstandard(hfp_lm16)]

# ggplot(test[year>=2016&year<=2019,], aes(hfp2,stdresid16))+geom_point()+geom_hline(yintercept=c(-1.5,1.5))
# 
# ggplot(test[year==2011&month==1,], aes(date_time,hfp2))+geom_point()
# ggplot(test[year>=2016&year<=2019&stdresid16>(-1.5)&stdresid16<1.5,], aes(date_time,hfp2))+geom_point()

# remove 2011 stdresids +/- 1.5
test[year>=2016&year<=2019&(stdresid16<(-1.5)|stdresid16>1.5),hfp2 := NA]

# predift hfp2 with the two regressions
test[year>=2011&year<=2013, pred:= coef(hfp_lm11)[1] + coef(hfp_lm11)[2]*hfp4]
test[year>=2016&year<=2019, pred:= coef(hfp_lm16)[1] + coef(hfp_lm16)[2]*hfp4]

# graph hfp2 and predicted value
# ggplot(test, aes(yday(date_time),hfp2))+
#   geom_point()+
#   geom_point(aes(yday(date_time),pred),colour="red")+
#   facet_grid(year~.)

# merge the residuals and predicted values back to the original and create NA with residuals are +/-1.5
# create a variable column for merging
test[,variable:= "hfp01_2_Avg"]
flux_long <- merge(flux_long,
                   test[,.(date_time,variable,stdresid11,stdresid16)],
                    by=c("variable","date_time"), all.x=TRUE)
# make hfp2 NA when stdresids are +/- 1.5
flux_long[variable== "hfp01_2_Avg" & ((stdresid11 >(1.5) | stdresid11 < (-1.5)) | 
            (stdresid16 >(1.5) | stdresid16 < (-1.5))), value := NA ]

# in flux_long predict hfp2
hfp_pred <- copy(flux_long[variable %in% c("hfp01_4_Avg"),.(date_time,year,value)])

colnames(hfp_pred) <- c("date_time","year","hfp4")

hfp_pred[year>=2011&year<=2013, pred:= coef(hfp_lm11)[1] + coef(hfp_lm11)[2]*hfp4]
hfp_pred[year>=2014&year<=2019, pred:= coef(hfp_lm16)[1] + coef(hfp_lm16)[2]*hfp4]
hfp_pred[,variable:= "hfp01_2_Avg"]

flux_long <- merge(flux_long,
                   hfp_pred[,.(date_time,variable,pred)],
                   by=c("variable","date_time"), all.x=TRUE)

# hfp2 filtered and predicted
ggplot(flux_long[variable == "hfp01_2_Avg",])+
  geom_point(aes(date_time,value))+
  geom_point(aes(date_time,pred),colour="red")

# use predicted hfp2 data to fill NA and use a column to track measured (=0), predicted (=1)
flux_long[variable == "hfp01_2_Avg" & is.na(value) & !is.na(pred), ':=' (value=pred, gapfill_id=1)]

ggplot()+
  geom_point(aes(date_time,value), data = flux_long[variable == "hfp01_2_Avg"&is.na(gapfill_id),])+
  geom_point(aes(date_time,value),data = flux_long[variable == "hfp01_2_Avg"&gapfill_id==1,], colour="red")

flux_long[!is.na(gapfill_id), gapfill_id:=0]

########################################

# look at final, filtered HFP data

# all hfp filtered
ggplot(flux_long[variable %in% c("hfp01_1_Avg", "hfp01_2_Avg", "hfp01_3_Avg", "hfp01_4_Avg") & gapfill_id==0,],
       aes(date_time, value))+geom_line()+
  facet_grid(variable~.,scales="free_y")

# all hfp filtered with predicted 
ggplot(flux_long[variable %in% c("hfp01_1_Avg", "hfp01_2_Avg", "hfp01_3_Avg", "hfp01_4_Avg"),],
       aes(date_time, value,colour=factor(gapfill_id)))+
  geom_line()+
 # geom_line(data=flux_long[variable %in% c("hfp01_2_Avg")&is.na(value),],aes(date_time,pred),colour="red")+
  facet_grid(variable~.,scales="free_y")


# LWS 
# lws_1 (shrub) and lws_2 (5m)
# (from met data filtering, remove these values from lws_2)
flux_long[variable=="lws_2_Avg"&date_time<as.Date("2011-08-25"), value := NA]
flux_long[variable=="lws_2_Avg"&date_time>as.Date("2012-02-01")&
             date_time<as.Date("2012-03-08"), value := NA]

# 2010 & 2011 remove:
#  lws_1 before April 10 2011
flux_long[variable=="lws_1_Avg"&date_time<as.Date("2011-04-10"), value := NA]
# lws_1 has a baseline shift on August 25, pattern looks OK though so keep the data. 
# program notes say that 2011-08-24 LWS instructions were changed. lws_2 looks the same, lws_1 shifted at this time
# remove lws_1 

# lws_1 after Apr 2015 values get really high and look bad. I think remove all. 
flux_long[variable=="lws_1_Avg"&
            (date_time<as.Date("2012-08-24") | date_time>as.Date("2015-03-31")), value := NA]

# lws_2 remove the really low values in Apr 2015
flux_long[variable=="lws_2_Avg"&value<225, value := NA]

# rescale LWS between 0-100
# remove values <250
flux_long[variable %in% c("lws_1_Avg","lws_2_Avg") & value<250, value := NA]
# remove values >375 since this is a very common max value and the SN LWS sensors often appear to max out
flux_long[variable %in% c("lws_1_Avg","lws_2_Avg") & value>375, value := NA]

# min val: 250.2; max val = 375

# RESCALE from 0 to 100
flux_long[variable %in% c("lws_1_Avg","lws_2_Avg"), value := ((value-250.2)/(375-250.2))*100]



# graph lws data
ggplot(flux_long[variable %in% c("lws_1_Avg", "lws_2_Avg"),],
       aes(date_time, value))+geom_line()+
  facet_grid(variable~.,scales="free_y")

# radiation data
# according to notes in Aline's logs 2011-06-03 radiometer was upgraded from CNR1 to CNR4
# in the data there is a break and pattern change ~2012-08-25 Rl swtiched from low values to high values

# Rs_downwell_Avg	W/m^2
# Rs_upwell_Avg	W/m^2
# Rl_downwell_Avg	W/m^2
# Rl_upwell_Avg	W/m^2
# Rn_nr_Avg

# range of Rs up: 

# radiation

# 2010 - 2012
# remove all prior to July 13 2011
flux_long[variable %in% c("Rs_downwell_Avg", "Rs_upwell_Avg", "Rl_downwell_Avg", "Rl_upwell_Avg","Rn_nr_Avg")&
            date_time<as.POSIXct("2011-07-13 12:00", tx="UTC"), value := NA]


# remove all Rl prior to 2012-08-25
flux_long[variable %in% c("Rl_downwell_Avg", "Rl_upwell_Avg")&
            date_time<as.POSIXct("2012-08-25 23:30", tx="UTC"), value := NA]

# remove >August 20 2012 and < August 25 12:00 (flat-line)
flux_long[variable %in% c("Rs_downwell_Avg", "Rs_upwell_Avg", "Rl_downwell_Avg", "Rl_upwell_Avg","Rn_nr_Avg")&
            date_time>as.POSIXct("2012-08-20 12:00", tx="UTC")&
            date_time<as.POSIXct("2012-08-25 12:00", tz="UTC"), value := NA]

# radiation remove > Feb 1 2012 and < Mar 8 2012 (flat-line)
flux_long[variable %in% c("Rs_downwell_Avg", "Rs_upwell_Avg", "Rl_downwell_Avg", "Rl_upwell_Avg","Rn_nr_Avg")&
            date_time>as.Date("2012-02-01")&
            date_time<as.Date("2012-03-08"), value := NA]


# 2013: looks good 
# 2014: looks good 
# 2015: some high values to remove in Jan/Feb and Dec for Rs_downwell
# radiation remove >Oct 20 2015 and < Nov 7 2015 (flat-line)
flux_long[variable %in% c("Rs_downwell_Avg", "Rs_upwell_Avg", "Rl_downwell_Avg", "Rl_upwell_Avg","Rn_nr_Avg")&
            date_time>as.Date("2015-10-20")&
            date_time<as.Date("2015-11-07"), value := NA]

# Jan/Feb when zoomed in the high  values look like they are in a regular diurnal pattern. Leave.
# Dec when zoomed in the high values look like they are in a regular diurnal patter. Leave.

# 2016: looks good except for Rl values with opposite sign as before as before August 2012
# 2017: looks good except for Rl values with opposite sign as before as before August 2012
# 2018: looks good except for Rl values with opposite sign as before as before August 2012
# 2019: looks good except for Rl values with opposite sign as before as before August 2012

ggplot(flux_long[variable %in% c("Rs_upwell_Avg","Rs_downwell_Avg","Rl_upwell_Avg","Rl_downwell_Avg","Rn_nr_Avg"),],
       aes(date_time, value))+geom_line()+
  facet_grid(variable~.,scales="free_y")


######## look in more detail at Rs and Rl to understand the data #########
flux_Rs_Rl <- copy(flux_long)
flux_Rs_Rl[,variable:= factor(variable, levels=c("Rs_upwell_Avg","Rs_downwell_Avg","Rl_upwell_Avg","Rl_downwell_Avg"))]

testRs <- merge(flux_long[variable %in% c("Rs_upwell_Avg"),.(date_time,value)],
              flux_long[variable %in% c("Rs_downwell_Avg"),.(date_time,value)],by="date_time")
colnames(testRs) <- c("date_time","Rs_upwell_Avg","Rs_downwell_Avg")

testRl <- merge(flux_long[variable %in% c("Rl_upwell_Avg"),.(date_time,value)],
                flux_long[variable %in% c("Rl_downwell_Avg"),.(date_time,value)],by="date_time")
colnames(testRl) <- c("date_time","Rl_upwell_Avg","Rl_downwell_Avg")

testRl_meas <- merge(flux_long[variable %in% c("Rl_up_meas_Avg"),.(date_time,value)],
                flux_long[variable %in% c("Rl_down_meas_Avg"),.(date_time,value)],by="date_time")
colnames(testRl_meas) <- c("date_time","Rl_up_meas_Avg","Rl_down_meas_Avg")

testRl <- merge(testRl, testRl_meas, by="date_time")

testRs_Rl <- merge(testRs, testRl, by="date_time")

ggplot(flux_Rs_Rl[variable %in% c("Rs_upwell_Avg","Rs_downwell_Avg","Rl_upwell_Avg","Rl_downwell_Avg") &
                   date_time>as.Date("2010-02-01")&date_time<as.Date("2010-03-31"),],
       aes(date_time, value))+geom_point()+
  facet_grid(variable~.,scales="free_y")

ggplot(flux_Rs_Rl[variable %in% c("Rs_upwell_Avg","Rs_downwell_Avg","Rl_upwell_Avg","Rl_downwell_Avg"),],
       aes(date_time, value,colour=variable))+geom_line()+
  facet_grid(variable~.,scales="free_y")

ggplot(flux_Rs_Rl[variable %in% c("Rs_upwell_Avg","Rs_downwell_Avg","Rl_upwell_Avg","Rl_downwell_Avg")&
                   date_time>as.Date("2010-02-01"),],
       aes(date_time, value,colour=variable))+geom_line()+
  scale_colour_manual(values=c("Rs_upwell_Avg"="blue",
                               "Rs_downwell_Avg"="green",
                               "Rl_upwell_Avg"="yellow",
                               "Rl_downwell_Avg"="red"))

ggplot(flux_Rs_Rl[variable %in% c("Rs_upwell_Avg","Rs_downwell_Avg","Rl_upwell_Avg","Rl_downwell_Avg")&
                   date_time>as.Date("2012-08-25"),],
       aes(date_time, value,colour=variable))+geom_line()+
  scale_colour_manual(values=c("Rs_upwell_Avg"="blue",
                               "Rs_downwell_Avg"="green",
                               "Rl_upwell_Avg"="yellow",
                               "Rl_downwell_Avg"="red"))

# net Rs
ggplot(testRs_Rl[date_time>as.Date("2012-08-25"),],
       aes(date_time, Rs_downwell_Avg-Rs_upwell_Avg))+geom_line()

# net Rl
ggplot(testRs_Rl[date_time>as.Date("2012-08-25"),],
       aes(date_time, Rl_upwell_Avg-Rl_downwell_Avg))+geom_line()

# net Rad 
ggplot(testRs_Rl[date_time>as.Date("2012-08-25"),],
      aes(date_time,(Rs_upwell_Avg-Rs_downwell_Avg) + (Rl_upwell_Avg-Rl_downwell_Avg)))+
  geom_point()

ggplot(testRs_Rl,
       aes(date_time,(Rs_upwell_Avg-Rs_downwell_Avg) + (Rl_upwell_Avg-Rl_downwell_Avg)))+
  geom_point()
#####################################################################

# plot all variables
ggplot(flux_long[variable %in% c("Rs_upwell_Avg","Rs_downwell_Avg","Rl_upwell_Avg","Rl_downwell_Avg",
           "Rn_nr_Avg", "lws_1_Avg","hfp01_1_Avg", "hfp01_2_Avg", "hfp01_3_Avg", "hfp01_4_Avg"),], 
       aes(date_time,value))+
  geom_line()+
  facet_grid(variable~., scales="free_y")


# correct the timestamps
# keep the original timestamp and fix date_time column to make all times MST
# (use tz=UTC to prevent convervsion of data)
flux_long_corrected <- copy(flux_long)
##rm(flux_long)
##flux_long <- copy(flux_long_corrected)


flux_long[,date_time_orig := date_time][,date_time:=NULL]

# do nothing before 2011-03-21
flux_long[date_time_orig<as.POSIXct("2011-03-21 16:00:00",tz="UTC"),
                date_time := date_time_orig]

# minus 1 hour
flux_long[(date_time_orig>=as.POSIXct("2011-03-21 17:00:00",tz="UTC") & 
                   date_time_orig<=as.POSIXct("2011-11-12 12:00:00",tz="UTC")),
                date_time := date_time_orig - hours(1)]

# do nothing
flux_long[date_time_orig>=as.POSIXct("2011-11-12 12:30:00",tz="UTC") & 
                  date_time_orig<as.POSIXct("2012-05-17 16:00:00",tz="UTC"),
                date_time := date_time_orig]

# minus 1 hour
flux_long[date_time_orig>=as.POSIXct("2012-05-17 17:00:00",tz="UTC") & 
                  date_time_orig<=as.POSIXct("2013-01-11 13:00:00",tz="UTC"),
                date_time := date_time_orig - hours(1)]

# do nothing
flux_long[date_time_orig>=as.POSIXct("2013-01-11 13:30:00",tz="UTC") & 
                  date_time_orig<as.POSIXct("2013-08-02 12:00:00",tz="UTC"),
                date_time := date_time_orig]

# minus 1 hour
flux_long[date_time_orig>=as.POSIXct("2013-08-02 13:00:00",tz="UTC") & 
                  date_time_orig<=as.POSIXct("2015-10-19 12:00:00",tz="UTC"),
                date_time := date_time_orig-hours(1)]


# do nothing
flux_long[date_time_orig>=as.POSIXct("2015-10-19 12:30:00",tz="UTC"),
                date_time := date_time_orig]


# remake year, month, doy
flux_long[,':=' (year=year(date_time),
                       month=month(date_time),
                       doy=yday(date_time))]

# import SW potential to compare with adjusted timestamp
sw.pot <- fread("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/Ameriflux/QA_QC_Report_Ameriflux/US-Jo1_HH_2010_2019_SW_IN_pot.csv",
                sep=",",header=TRUE, na.strings=c("-9999"))

sw.pot[,date_time := parse_date_time(TIMESTAMP_END, "YmdHM",tz="UTC")]

sw.pot[,':=' (location="potential",
              variable="sw_pot",
              value=SW_IN_POT,
              date_time_orig=date_time,
              year=year(date_time),
              month=month(date_time),
              doy=yday(date_time))][,SW_IN_POT:=NULL]


flux_long_comp <- rbind(flux_long, sw.pot, fill=TRUE)

# get rid of the NA timestamps from date_time
flux_long_comp <- flux_long_comp[!is.na(date_time),]

# look at adjusment
# non adjusted, original
daycheck <- as.Date("2015-10-19")

ggplot(flux_long_comp[variable%in% c("Rs_upwell_Avg", "sw_pot")&
                        as.Date(date_time)==daycheck])+
  geom_line(aes(date_time_orig,value, colour=variable))

# adjusted timesstamp
ggplot(flux_long_comp[variable%in% c("Rs_upwell_Avg", "sw_pot")&
                        as.Date(date_time)==daycheck])+
  geom_line(aes(date_time,value, colour=variable))



# save 30min filtered HFP, Rs, Rl, Rn, LWS from flux table 
setwd("~/Desktop/TweedieLab/Projects/Jornada/Data/Tower/Flux/Compiled_forJoining")
# write.table(flux_long[variable %in% c("Rs_upwell_Avg","Rs_downwell_Avg","Rl_upwell_Avg","Rl_downwell_Avg",
# "Rn_nr_Avg", "lws_1_Avg","hfp01_1_Avg", "hfp01_2_Avg", "hfp01_3_Avg", "hfp01_4_Avg","gapfill_id"),.(date_time,variable,value)],
# file="FluxTable_L1_2010_2019_30min.csv", sep=",", row.names = FALSE)

# save data updated to May 31 2019
# write.table(flux_long[variable %in% c("Rs_upwell_Avg","Rs_downwell_Avg","Rl_upwell_Avg","Rl_downwell_Avg",
# "Rn_nr_Avg", "lws_1_Avg","hfp01_1_Avg", "hfp01_2_Avg", "hfp01_3_Avg", "hfp01_4_Avg","gapfill_id"),.(date_time,variable,value)],
# file="FluxTable_L1_2010_20190531_30min.csv", sep=",", row.names = FALSE)

# save data updated to Jan 12 2020
# write.table(flux_long[variable %in% c("Rs_upwell_Avg","Rs_downwell_Avg","Rl_upwell_Avg","Rl_downwell_Avg",
# "Rn_nr_Avg", "lws_1_Avg","hfp01_1_Avg", "hfp01_2_Avg", "hfp01_3_Avg", "hfp01_4_Avg","gapfill_id"),.(date_time,variable,value)],
# file="FluxTable_L1_2010_20200112_30min.csv", sep=",", row.names = FALSE)

# save updated to 24 March 2020 and with timestamps corrected
# CHANGE NAME TO L2!!!!
# save flux_long_comp instead just flux_long because for some reason that fixes the timestamp 'reversion'!!! 
# write.table(flux_long_comp[variable %in% c("Rs_upwell_Avg","Rs_downwell_Avg","Rl_upwell_Avg","Rl_downwell_Avg",
#  "Rn_nr_Avg", "lws_1_Avg","hfp01_1_Avg", "hfp01_2_Avg", "hfp01_3_Avg", "hfp01_4_Avg"),.(date_time,date_time_orig,variable,gapfill_id,value)],
#  file="FluxTable_L2_2010_2020324_30min.csv", sep=",", dec=".", row.names = FALSE)

# save updated 26 Dec 2020 to rescale LWS from 0-100
# save flux_long_comp instead just flux_long because for some reason that fixes the timestamp 'reversion'!!! 
# write.table(flux_long_comp[variable %in% c("Rs_upwell_Avg","Rs_downwell_Avg","Rl_upwell_Avg","Rl_downwell_Avg",
# "Rn_nr_Avg", "lws_1_Avg","hfp01_1_Avg", "hfp01_2_Avg", "hfp01_3_Avg", "hfp01_4_Avg"),.(date_time,date_time_orig,variable,gapfill_id,value)],
# file="FluxTable_L2_2010_20201226_30min.csv", sep=",", dec=".", row.names = FALSE)

# # AND save in wide format
# save by year

flux_wide_save_vars <- data.table:: dcast(flux_long[!is.na(date_time)&variable %in% c("Rs_upwell_Avg","Rs_downwell_Avg",
                                                                                 "Rl_upwell_Avg","Rl_downwell_Avg",
                                                  "Rn_nr_Avg", "lws_1_Avg","hfp01_1_Avg", "hfp01_2_Avg", "hfp01_3_Avg",
                                                  "hfp01_4_Avg"),
                                               .(date_time, date_time_orig, variable,value)],
                                      date_time+date_time_orig~variable,
                                      value.var="value")

flux_wide_save_gapID <- data.table:: dcast(flux_long[!is.na(date_time)&variable %in% c("Rs_upwell_Avg","Rs_downwell_Avg",
                                                                                 "Rl_upwell_Avg","Rl_downwell_Avg",
                                                                                 "Rn_nr_Avg", "lws_1_Avg","hfp01_1_Avg", "hfp01_2_Avg", "hfp01_3_Avg",
                                                                                 "hfp01_4_Avg"),
                                               .(date_time, variable,gapfill_id)],
                                     date_time~paste(variable,"gapfillID",sep="_"),
                                     value.var="gapfill_id")

flux_wide_save <- merge(flux_wide_save_vars,flux_wide_save_gapID,by="date_time")


setnames(flux_wide_save,c("date_time","date_time_orig"),
         c("timestamp","timestamp_orig"))

ggplot(flux_wide_save, aes(x=timestamp))+
  geom_line(aes(y=Rl_upwell_Avg)) # with NA

# save by year!
setwd("~/Desktop/TweedieLab/Projects/Jornada/Data/Tower/Flux/Yearly_QAQC_timestamp")

saveyears <- function(data,startyear,endyear) {
  data[is.na(data)] <- NA
  
  for (i in startyear:endyear) {
    data_save <- data[year(timestamp)==i,]
    
    write.table (data_save,
                 file=paste("dataL2_flux",i, ".csv",sep="_"),
                 sep =',', dec='.', row.names=FALSE,quote=FALSE)
  }}

# saveyears(flux_wide_save,2010,2020)

# look at fluxes:
ggplot(flux_long[variable == "Fc_wpl",])+
  geom_point(aes(date_time,value))

ggplot(flux_long[variable == "Fc_wpl"&year==2014&month==7,])+
  geom_point(aes(date_time,value))+
  ylim(-0.5,0.5)+
  facet_grid(.~year)

# plot in umol?
# 1mg/44 = 1mmol*1000 = umol
ggplot(flux_long[variable == "Fc_wpl"&year>2010,])+
  geom_line(aes(date_time,((value)/44)*1000))+
  ylim(-10,10)

ggplot(flux_long[variable == "Fc_wpl"&year==2014,])+
  geom_line(aes(date_time,((value)/44)*1000))+
  ylim(-10,10)


# look at H
ggplot(flux_long[variable == "Hc",])+
  geom_point(aes(date_time,value))

ggplot(flux_long[variable == "Hc",])+
  geom_point(aes(date_time,value))+
  ylim(-1000,1000)

ggplot(flux_long[variable == "Hc"&year==2014,])+
  geom_point(aes(date_time,value))+
  ylim(-1000,1000)


