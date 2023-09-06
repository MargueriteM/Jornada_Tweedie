###################################################
# This code processes pre-calc Flux Data from JER #
# Get the most recent data to QA/QC and save      #
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
#                17 Nov, 2021                     #
###################################################

# This code will:
# 1. allow data to be checked
# 2. run standard range filters determined from 2010-2019 and input from Ameriflux
# 3. allow year-specific data removal based on visual checks
# 4. save data with date/time in file name of Flux/year/QAQC folder on server
# 5. save a full year of data to Flux/Combined with only year in filename
# 6. save a csv file of this code as Data_QAQC_Code_yyyy.csv (or with date/time) to Flux/year/QAQC folder to record data filter steps


# load libraries
library(data.table)
library(ggplot2)
library(lubridate)
library(gridExtra)
library(lattice)


# server source: smb://prodgis04.utep.edu/SEL_Data_Archive/Research Data/Desert/Jornada/UtepJrnBahadaSite/Tower/Flux/

# mainly want to get
# get lws_1, soil heat flux data, Rs and Rl components

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

year_file <- 2023

# import most recent file
flux.loggerinfo <-fread(paste("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Bahada/Tower/Flux/",year_file,"/Raw_Data/ASCII/dataL1_flux_",year_file,".csv",sep=""),
                           header = FALSE, sep=",", skip = 0,fill=TRUE,
                           na.strings=c(-9999,"#NAME?"))[1,]


flux.colnames <-colnames(fread(paste("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Bahada/Tower/Flux/",year_file,"/Raw_Data/ASCII/dataL1_flux_",year_file,".csv",sep=""),
                         header = TRUE, sep=","))

flux <- fread(paste("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Bahada/Tower/Flux/",year_file,"/Raw_Data/ASCII/dataL1_flux_",year_file,".csv",sep=""),
                 header = FALSE, sep=",", skip = 4,fill=TRUE,
                 na.strings=c(-9999,"#NAME?"),
              col.names=flux.colnames)

# convert the time stamp to a posixct format
flux[,date_time := parse_date_time(TIMESTAMP, c("%Y!-%m-%d %H:%M:%S",
                                                    "%m-%d-%y %H:%M"))]

# change data to long format and drop timestamp and record variables.
flux_long <- melt.data.table(flux[,!c("TIMESTAMP","RECORD")],c("date_time"))

flux_long[,':=' (year=year(date_time),month=month(date_time),doy=yday(date_time))]

# check data

# check file start and end date
startdate.check <- (min(flux_long$date_time))
enddate.check <- (max(flux_long$date_time))

# heat fluxes, remove very obviously bad data
# hfp01_1_Avg	W/m^2 Soil heat flux (5cm/15cm) 15 open ... (channel 3H. Field label: 10O)
# hfp01_2_Avg	W/m^2 Soil heat flux (5cm/15cm) 10 open ... (channel 6H. Field label: 15O)
# hfp01_3_Avg	W/m^2 Soil heat flux (5cm/15cm) 15 bush ... (channel 10H. Field label: 15B)
# hfp01_4_Avg	W/m^2 Soil heat flux (5cm/15cm) 10 bush ... (channel 8L (=16 SE). Field label: 10B)

# all hfp 
ggplot(flux_long[variable %in% c("hfp01_1_Avg", "hfp01_2_Avg", "hfp01_3_Avg", "hfp01_4_Avg"),],#&
                  # date_time>as.Date("2021-05-28") &date_time<as.Date("2021-06-04"),],
       aes(date_time, value))+
  geom_line()+
  facet_grid(variable~.,scales="free_y")

# in 2021 something is wrong end of May/Early Jun. All plates have vvalues offset low. 
# was checking and messsing with wiring in data logger. 
# Remove all HFP between > 2021-05-28 and < 2021-06-04
# flux_long[variable %in% c("hfp01_1_Avg", "hfp01_2_Avg", "hfp01_3_Avg", "hfp01_4_Avg")&
#           date_time>as.Date("2021-05-28") &date_time<as.Date("2021-06-04"), value := NA]

# all hfp have a low spike on 2021-08-27 around 18:00.
# Remove 17:30 - 28th 00:00

# flux_long[variable %in% c("hfp01_1_Avg", "hfp01_2_Avg", "hfp01_3_Avg", "hfp01_4_Avg")&
#            date_time>as.POSIXct("2021-08-27 17:00", tz="UTC")&
#            date_time<as.POSIXct("2021-08-28 00:00", tz="UTC"), value := NA]

# 2022: looks good until "2022-06-01 11:30:00 UTC"
# 2022 low value in August in HFP 3 and 4: both under shrub
# align with a rain event - leave these values in. 

#zoom to understand low point date_time>as.Date("2022-08-04") &date_time<as.Date("2022-08-06")
# general code for zooming
ggplot(flux_long[variable %in% c("hfp01_1_Avg", "hfp01_2_Avg", "hfp01_3_Avg", "hfp01_4_Avg") &
                    date_time>as.Date("2023-05-04") &date_time<as.Date("2023-07-06"),],
       aes(date_time, value))+
  geom_point()+
  facet_grid(variable~.,scales="free_y")

# graph with rain to see if change in HFP aligns with rain
  ggplot(flux_long[variable %in% c("hfp01_1_Avg", "hfp01_2_Avg", "hfp01_3_Avg", "hfp01_4_Avg","precip_Tot") &
                     date_time>as.Date("2023-05-04") &date_time<as.Date("2023-07-06"),],
         aes(date_time, value))+
    geom_point()+
    facet_grid(variable~.,scales="free_y")

# align with rain and HFP 3 and 4: both under shrub. Leave these in. 
  
# LWS 
# lws_1 (shrub) and lws_2 (5m)
# lws_2 is also in climate data
# graph lws data
ggplot(flux_long[variable %in% c("lws_1_Avg", "lws_2_Avg"),],
       aes(date_time, value))+geom_line()+
  facet_grid(variable~.,scales="free_y")

# lws_1 appears to be working only intermitently
# rescale LWS between 0-100
# remove values <250
flux_long[variable %in% c("lws_1_Avg","lws_2_Avg") & value<250, value := NA]
# remove values >375 since this is a very common max value and the SN LWS sensors often appear to max out
flux_long[variable %in% c("lws_1_Avg","lws_2_Avg") & value>375, value := NA]

# min val: 250; max val = 375

# RESCALE from 0 to 100
flux_long[variable %in% c("lws_1_Avg","lws_2_Avg"), value := ((value-250)/(375-250))*100]

# graph lws data
ggplot(flux_long[variable %in% c("lws_1_Avg", "lws_2_Avg"),],
       aes(date_time, value))+geom_line()+
  facet_grid(variable~.,scales="free_y")

# 2021, 2022: remove lws_1
flux_long[variable %in% c("lws_1_Avg"), value := NA]

# radiation data
# Rs_downwell_Avg	W/m^2
# Rs_upwell_Avg	W/m^2
# Rl_downwell_Avg	W/m^2
# Rl_upwell_Avg	W/m^2
# Rn_nr_Avg

# radiation

ggplot(flux_long[variable %in% c("Rs_upwell_Avg","Rs_downwell_Avg","Rl_upwell_Avg","Rl_downwell_Avg","Rn_nr_Avg"),],
       aes(date_time, value))+geom_line()+
  facet_grid(variable~.,scales="free_y")

# 2021 Rl_upwell and Rn is bad from 27 Aug 2021 to 29 Oct 2021 19:30 due to broken upward looking Rl sensor	
#f lux_long[variable%in% c("Rl_upwell_Avg","Rn_nr_Avg") &
#              (date_time > as.POSIXct("2021-08-27 00:00",	 tz="UTC") &
#                 date_time < as.POSIXct("2021-10-29 19:30",	 tz="UTC")),
#            value := NA]	

# 2022 looks good until "2022-08-25 13:00:00 UTC"

# plot all radiation variables
ggplot(flux_long[variable %in% c("Rs_upwell_Avg","Rs_downwell_Avg","Rl_upwell_Avg","Rl_downwell_Avg",
           "Rn_nr_Avg","hfp01_1_Avg", "hfp01_2_Avg", "hfp01_3_Avg", "hfp01_4_Avg"),], 
       aes(date_time,value))+
  geom_line()+
  facet_grid(variable~., scales="free_y")


# plot all  variables that will be taken formward to merge with remianing data
ggplot(flux_long[variable %in% c("Rs_upwell_Avg","Rs_downwell_Avg","Rl_upwell_Avg","Rl_downwell_Avg",
                                 "Rn_nr_Avg","hfp01_1_Avg", "hfp01_2_Avg", "hfp01_3_Avg", "hfp01_4_Avg","lws_1_Avg"),], 
       aes(date_time,value))+
  geom_line()+
  facet_grid(variable~., scales="free_y")

# save 30min filtered HFP, Rs, Rl, Rn, LWS_1 from flux table 
# setwd("~/Desktop/TweedieLab/Projects/Jornada/Data/Tower/Flux/Compiled_forJoining")
# write.table(flux_long[variable %in% c("Rs_upwell_Avg","Rs_downwell_Avg","Rl_upwell_Avg","Rl_downwell_Avg",
# "Rn_nr_Avg", "lws_1_Avg","hfp01_1_Avg", "hfp01_2_Avg", "hfp01_3_Avg", "hfp01_4_Avg","gapfill_id"),.(date_time,variable,value)],
# file="FluxTable_L1_2010_2019_30min.csv", sep=",", row.names = FALSE)


# # AND save in wide format
flux_wide_save <- data.table:: dcast(flux_long[!is.na(date_time)&variable %in% c("Rs_upwell_Avg","Rs_downwell_Avg",
                                                                                 "Rl_upwell_Avg","Rl_downwell_Avg",
                                                  "Rn_nr_Avg", "lws_1_Avg","hfp01_1_Avg", "hfp01_2_Avg", "hfp01_3_Avg",
                                                  "hfp01_4_Avg"),
                                               .(date_time, variable,value)],
                                      date_time~variable,
                                      value.var="value")

# quick graph just to make sure nothing silly happened
ggplot(flux_wide_save, aes(x=date_time))+
  geom_line(aes(y=Rn_nr_Avg)) # with NA

# save to QAQC folder on data archive
startdate <- (min(flux_wide_save$date_time))
enddate <- (max(flux_wide_save$date_time))

# add comment about processing
print(paste("#",year(enddate), "data processed until",enddate,sep=" "))
# 2023 data processed until 2023-08-25 07:30:00

# # Save to Qa/QC and Combined folder with only year name
qaqc.path<- paste("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Bahada/Tower/Flux/",year_file,"/QAQC/", sep="")
setwd(qaqc.path)

###################### with start and end date in the file name ##################
# write.table(flux_wide_save,
#             paste("dataL2_flux_",year(startdate),sprintf("%02d",(month(startdate))),sprintf("%02d",(day(startdate))),
#                   sprintf("%02d",(hour(startdate))),sprintf("%02d",(minute(startdate))),
#                   sprintf("%02d",(second(startdate))),
#                   "_",
#                   year(enddate),sprintf("%02d",(month(enddate))),sprintf("%02d",(day(enddate))),
#                   sprintf("%02d",(hour(enddate))),sprintf("%02d",(minute(enddate))),
#                   sprintf("%02d",(second(enddate))), ".csv",sep=""),
#             sep=",", dec=".", row.names=FALSE)
##########################################################################################

# Save to QAQC folder with only year name
# difftime(startdate,enddate)

write.table(flux_wide_save,
            paste("dataL2_flux_",year_file, ".csv",sep=""),
            sep=",", dec=".", row.names=FALSE)
# save a text file that says date that code was run (system time), start and end date of data
run.info <- data.frame(info=c("Data_start","Data_end","Date_processed"),
                       date_time=c(startdate,enddate,ymd_hms(Sys.time(),tz="UTC")))

write.table(run.info, "dataL2_flux_DateRange.csv",
            sep=",", dec=".", row.names=FALSE)

# don't save single years 
# save to combined folder
# setwd("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Bahada/Tower/Flux/Combined")
# 
# write.table(flux_wide_save,
#             paste("dataL2_flux_",year_file, ".csv",sep=""),
#             sep=",", dec=".", row.names=FALSE)

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




