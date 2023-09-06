#############################################
#  This code gets data output by Eddy Pro   #
#           written by: M. Mauritz          #
#                  Jan 2020                 #
#############################################

# Jan 2020: rewrite code to use same files at Jornada_EddyPro_Output_Fluxnet_2010_2019.R
# Feb 2020: update with 2010 data processed in batches of files with 14 and 15 data rows (see Jornada_EC_2010_diagnosing.R)

# load libraries
library(ggplot2) # library for making figures in ggplot package
library(lubridate) # library for easier date manipulation 
library(data.table) # library for data table which is more efficient with large data sets
library(reader)
library(tidyr)
library(lsr) # contains quantileCut function
library(gridExtra)
library(viridis)
#############
# IMPORT DATA
#############
# get header and unit info from the first rows of data files
# file info
fileinfo <- scan("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_2010/eddypro_JER_2010_full_output_2019-08-08T131210_adv.csv",
                  what='',sep=",",nlines=1)
fileinfo <- data.table(t(fileinfo))
# read only the first row to get the units
flux.units <- (fread("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_2010/eddypro_JER_2010_full_output_2019-08-08T131210_adv.csv",header=TRUE,skip=1))[1,]

# read the data, skippping the units row
flux1a <- fread("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_2010/length14/eddypro_JER_2010_length14_full_output_2020-02-04T105358_adv.csv", sep=",",skip=3,
               header=FALSE, na.strings=c("-9999","-9999.0","NAN","#NAME?"),col.names=colnames(flux.units))

# flux1b <- fread("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_2010/length14_2/eddypro_JER_2010_length14_2_full_output_2020-02-04T183325_adv.csv", sep=",",skip=3,
#                 header=FALSE, na.strings=c("-9999","-9999.0","NAN","#NAME?"),col.names=colnames(flux.units))

flux1b <- fread("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_2010/length14_2/eddypro_JER_2010_length14_2_full_output_2020-02-06T164032_adv.csv", sep=",",skip=3,
                header=FALSE, na.strings=c("-9999","-9999.0","NAN","#NAME?"),col.names=colnames(flux.units))


flux1c <- fread("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_2010/length15/eddypro_JER_2010_length15_full_output_2020-02-04T160313_adv.csv", sep=",",skip=3,
                header=FALSE, na.strings=c("-9999","-9999.0","NAN","#NAME?"),col.names=colnames(flux.units))


flux2 <- fread("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_2011/eddypro_JER_2011_full_output_2019-08-08T131507_adv.csv", sep=",",skip=3,
               header=FALSE, na.strings=c("-9999","-9999.0","NAN","#NAME?"),col.names=colnames(flux.units))

flux3 <- fread("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_2012/eddypro_JER_2012_full_output_2019-08-08T131951_adv.csv", sep=",",skip=3,
               header=FALSE, na.strings=c("-9999","-9999.0","NAN","#NAME?"),col.names=colnames(flux.units))

flux4 <- fread("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_2013/eddypro_JER_2013_full_output_2019-08-08T105511_adv.csv", sep=",",skip=3,
               header=FALSE, na.strings=c("-9999","-9999.0","NAN","#NAME?"),col.names=colnames(flux.units))

flux5 <- fread("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_2014/eddypro_JER_2014_full_output_2019-08-07T231114_adv.csv", sep=",",skip=3,
               header=FALSE, na.strings=c("-9999","-9999.0","NAN","#NAME?"),col.names=colnames(flux.units))

flux6 <- fread("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_2015/20190806/eddypro_JER_2015_full_output_2019-08-07T131630_adv.csv", sep=",",skip=3,
               header=FALSE, na.strings=c("-9999","-9999.0","NAN","#NAME?"),col.names=colnames(flux.units))

flux7 <- fread("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_2016/20190806/eddypro_JER_2016_full_output_2019-08-07T131944_adv.csv", sep=",",skip=3,
               header=FALSE, na.strings=c("-9999","-9999.0","NAN","#NAME?"),col.names=colnames(flux.units))

flux8 <- fread("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_2017/20190806/eddypro_JER_2017_full_output_2019-08-07T114629_adv.csv", sep=",",skip=3,
               header=FALSE, na.strings=c("-9999","-9999.0","NAN","#NAME?"),col.names=colnames(flux.units))

flux8.1 <- fread("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_2018/eddypro_JER_2018_full_output_2019-08-02T101350_adv.csv", sep=",",skip=3,
               header=FALSE, na.strings=c("-9999","-9999.0","NAN","#NAME?"),col.names=colnames(flux.units))


flux9 <- fread("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_2019/eddypro_JER_2019_full_output_2020-02-11T163749_adv.csv", sep=",",skip=3,
               header=FALSE, na.strings=c("-9999","-9999.0","NAN","#NAME?"),col.names=colnames(flux.units))

flux10a <- fread("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_2020/eddypro_JER_2020_Jan_May_full_output_2021-12-09T170831_adv.csv", sep=",",skip=3,
                header=FALSE, na.strings=c("-9999","-9999.0","NAN","#NAME?"),col.names=colnames(flux.units))

flux10b <- fread("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_2020/eddypro_JER_2020_May_Dec_full_output_2021-12-08T174700_adv.csv", sep=",",skip=3,
                 header=FALSE, na.strings=c("-9999","-9999.0","NAN","#NAME?"),col.names=colnames(flux.units))


# combine all individual years of flux runs
flux <- rbind(flux1a, flux1b, flux1c, flux2, flux3, flux4,
              flux5, flux6, flux7, flux8, flux8.1, flux9, flux10a, flux10b)

# format date
flux[,date_time := paste(date,time,sep=" ")]
flux[,':=' (date=as.Date(date),
            date_time = as.POSIXct(date_time, format="%Y-%m-%d %H:%M"),
            year=year(date_time))]


# import the biomet data

# read only the first row to get the units
biomet.units1 <- (fread("eddypro_2010_2012_biomet_2019-07-02T063739_exp.csv",header=TRUE))[1,]
# read the data, skippping the units row
biomet1 <- fread("eddypro_2010_2012_biomet_2019-07-02T063739_exp.csv", sep=",",skip=2,
                 header=FALSE, na.strings=c("-9999","-9999.0","NAN","#NAME?"),col.names=colnames(biomet.units1))


# read only the first row to get the units
biomet.units2 <- (fread("eddypro_2013_2014_biomet_2019-07-03T164639_exp.csv",header=TRUE))[1,]
# read the data, skippping the units row
biomet2 <- fread("eddypro_2013_2014_biomet_2019-07-03T164639_exp.csv", sep=",",skip=2,
                 header=FALSE, na.strings=c("-9999","-9999.0","NAN","#NAME?"),col.names=colnames(biomet.units2))


# read only the first row to get the units
biomet.units3 <- (fread("eddypro_2015_2018_biomet_2019-06-27T221717_exp.csv",header=TRUE))[1,]
# read the data, skippping the units row
biomet3 <- fread("eddypro_2015_2018_biomet_2019-06-27T221717_exp.csv", sep=",",skip=2,
              header=FALSE, na.strings=c("-9999","-9999.0","NAN","#NAME?"),col.names=colnames(biomet.units3))

# file info

biomet.units4 <- (fread("eddypro_2019_biomet_2019-07-03T075822_exp.csv",header=TRUE))[1,]
# read the data, skippping the units row
biomet4 <- fread("eddypro_2019_biomet_2019-07-03T075822_exp.csv", sep=",",skip=2,
                 header=FALSE, na.strings=c("-9999","-9999.0","NAN","#NAME?"),col.names=colnames(biomet.units4))


biomet.units5 <- (fread("eddypro_2019_Apr_May_biomet_2019-07-09T173159_exp.csv",header=TRUE))[1,]
# read the data, skippping the units row
biomet5 <- fread("eddypro_2019_Apr_May_biomet_2019-07-09T173159_exp.csv", sep=",",skip=2,
                 header=FALSE, na.strings=c("-9999","-9999.0","NAN","#NAME?"),col.names=colnames(biomet.units5))

# merge all years of biomet
biomet <- rbind(biomet1, biomet2, biomet3, biomet4,biomet5)

# format date
biomet[,date_time := paste(date,time,sep=" ")]
biomet[,':=' (date=as.Date(date),
              date_time = as.POSIXct(date_time, format="%Y-%m-%d %H:%M"),
              year=year(date_time))]

# combine flux with biomet
flux <- merge(flux,biomet[,!c("date","time","date_time")],by=c("year","DOY"),all=TRUE)

# following Reichstein et al 2005 create 6 temperature bins for night and day
# Reichstein used 20W/m2 cut-off for night/day
flux[PPFD_1_1_1<90,temp.bin := quantileCut(air_temperature,6)]
flux[PPFD_1_1_1>=91,temp.bin := quantileCut(air_temperature,6)]

# create 20 u* bins within each temperature bin
flux[PPFD_1_1_1<90,u.bin := quantileCut(`u*`,20),by="temp.bin"]
flux[PPFD_1_1_1>=91,u.bin := quantileCut(`u*`,20),by="temp.bin"]


# full output
# from Gerardo:
# Select variables needed
#eddy <- eddy[, .(date, time, qc_co2_flux, co2_flux, qc_Tau, Tau, qc_H, H,
# qc_LE, LE, qc_h2o_flux, h2o_flux, qc_ch4_flux, ch4_flux, co2_strg,
# ch4_strg, co2_mole_fraction, model, x_peak, `x_30%`, `x_50%`, `x_70%`, `x_90%`,
# `u*`, wind_speed, wind_dir, sonic_temperature, air_temperature, air_pressure, 
# air_p_mean, RH, VPD, co2_signal_strength_7500_mean, rssi_77_mean, `top_heater_on_LI-7700`, 
# `bottom_heater_on_LI-7700`, `motor_spinning_LI-7700`, `pump_on_LI-7700`, `motor_failure_LI-7700`)]



# [1] "filename"                       "date"                           "time"                          
# [4] "DOY"                            "daytime"                        "file_records"                  
# [7] "used_records"                   "Tau"                            "qc_Tau"                        
# [10] "rand_err_Tau"                   "H"                              "qc_H"                          
# [13] "rand_err_H"                     "LE"                             "qc_LE"                         
# [16] "rand_err_LE"                    "co2_flux"                       "qc_co2_flux"                   
# [19] "rand_err_co2_flux"              "h2o_flux"                       "qc_h2o_flux"                   
# [22] "rand_err_h2o_flux"              "ch4_flux"                       "qc_ch4_flux"                   
# [25] "rand_err_ch4_flux"              "none_flux"                      "qc_none_flux"                  
# [28] "rand_err_none_flux"             "H_strg"                         "LE_strg"                       
# [31] "co2_strg"                       "h2o_strg"                       "ch4_strg"                      
# [34] "none_strg"                      "co2_v-adv"                      "h2o_v-adv"                     
# [37] "ch4_v-adv"                      "none_v-adv"                     "co2_molar_density"             
# [40] "co2_mole_fraction"              "co2_mixing_ratio"               "co2_time_lag"                  
# [43] "co2_def_timelag"                "h2o_molar_density"              "h2o_mole_fraction"             
# [46] "h2o_mixing_ratio"               "h2o_time_lag"                   "h2o_def_timelag"               
# [49] "ch4_molar_density"              "ch4_mole_fraction"              "ch4_mixing_ratio"              
# [52] "ch4_time_lag"                   "ch4_def_timelag"                "none_molar_density"            
# [55] "none_mole_fraction"             "none_mixing_ratio"              "none_time_lag"                 
# [58] "none_def_timelag"               "sonic_temperature"              "air_temperature"               
# [61] "air_pressure"                   "air_density"                    "air_heat_capacity"             
# [64] "air_molar_volume"               "ET"                             "water_vapor_density"           
# [67] "e"                              "es"                             "specific_humidity"             
# [70] "RH"                             "VPD"                            "Tdew"                          
# [73] "u_unrot"                        "v_unrot"                        "w_unrot"                       
# [76] "u_rot"                          "v_rot"                          "w_rot"                         
# [79] "wind_speed"                     "max_wind_speed"                 "wind_dir"                      
# [82] "yaw"                            "pitch"                          "roll"                          
# [85] "u*"                             "TKE"                            "L"                             
# [88] "(z-d)/L"                        "bowen_ratio"                    "T*"                            
# [91] "model"                          "x_peak"                         "x_offset"                      
# [94] "x_10%"                          "x_30%"                          "x_50%"                         
# [97] "x_70%"                          "x_90%"                          "un_Tau"                        
# [100] "Tau_scf"                        "un_H"                           "H_scf"                         
# [103] "un_LE"                          "LE_scf"                         "un_co2_flux"                   
# [106] "co2_scf"                        "un_h2o_flux"                    "h2o_scf"                       
# [109] "un_ch4_flux"                    "ch4_scf"                        "un_none_flux"                  
# [112] "un_none_scf"                    "spikes_hf"                      "amplitude_resolution_hf"       
# [115] "drop_out_hf"                    "absolute_limits_hf"             "skewness_kurtosis_hf"          
# [118] "skewness_kurtosis_sf"           "discontinuities_hf"             "discontinuities_sf"            
# [121] "timelag_hf"                     "timelag_sf"                     "attack_angle_hf"               
# [124] "non_steady_wind_hf"             "u_spikes"                       "v_spikes"                      
# [127] "w_spikes"                       "ts_spikes"                      "co2_spikes"                    
# [130] "h2o_spikes"                     "ch4_spikes"                     "none_spikes"                   
# [133] "head_detect_LI-7200"            "t_out_LI-7200"                  "t_in_LI-7200"                  
# [136] "aux_in_LI-7200"                 "delta_p_LI-7200"                "chopper_LI-7200"               
# [139] "detector_LI-7200"               "pll_LI-7200"                    "sync_LI-7200"                  
# [142] "chopper_LI-7500"                "detector_LI-7500"               "pll_LI-7500"                   
# [145] "sync_LI-7500"                   "not_ready_LI-7700"              "no_signal_LI-7700"             
# [148] "re_unlocked_LI-7700"            "bad_temp_LI-7700"               "laser_temp_unregulated_LI-7700"
# [151] "block_temp_unregulated_LI-7700" "motor_spinning_LI-7700"         "pump_on_LI-7700"               
# [154] "top_heater_on_LI-7700"          "bottom_heater_on_LI-7700"       "calibrating_LI-7700"           
# [157] "motor_failure_LI-7700"          "bad_aux_tc1_LI-7700"            "bad_aux_tc2_LI-7700"           
# [160] "bad_aux_tc3_LI-7700"            "box_connected_LI-7700"          "mean_value_RSSI_LI-7200"       
# [163] "mean_value_LI-7500"             "u_var"                          "v_var"                         
# [166] "w_var"                          "ts_var"                         "co2_var"                       
# [169] "h2o_var"                        "ch4_var"                        "none_var"                      
# [172] "w/ts_cov"                       "w/co2_cov"                      "w/h2o_cov"                     
# [175] "w/ch4_cov"                      "w/none_cov"                     "ts_mean"                       
# [178] "int_p_mean"                     "flag_2_mean"                    "air_t_mean"                    
# [181] "vapor_pressure_mean"            "air_p_mean"                     "date_time" 

# make some plots
# graph precipitation
ggplot(flux,aes(date_time,P_rain_1_1_1))+geom_line()
# graph soil moisture
ggplot(flux,aes(date_time,SWC_1_1_1))+geom_line()

# graph air temp
ggplot(flux,aes(date_time,air_temperature))+geom_line()

# co2 flux
ggplot(flux, aes(date_time,co2_flux,colour=factor(qc_co2_flux)))+
  geom_point(size=0.2)+
  ylim(c(-20,20))+
  facet_grid(.~year, scales="free_x")


ggplot(flux[month(date_time)==12&qc_co2_flux<2,],aes(DOY,co2_flux))+
  geom_line()+
  ylim(c(-5,5))+
  facet_grid(.~year)

# plot storage flux
ggplot(flux,aes(date_time,co2_strg,colour=factor(qc_co2_flux)))+
  geom_point()+
  ylim(c(-25,25))

# plot advection flux
ggplot(flux[`co2_v-adv`<2,],aes(date_time,`co2_v-adv`,colour=factor(qc_co2_flux)))+
  geom_point()

# look at CO2 and H2O variance
ggplot(flux[co2_flux>(-1000) & co2_flux<1000 & co2_var>(-1000)],aes(co2_var,co2_flux,colour=factor(qc_co2_flux)))+
  geom_point()

ggplot(flux[h2o_var>(-9000) & h2o_var<200000,],aes(date_time,h2o_var,colour=factor(qc_h2o_flux)))+
  geom_point()


ggplot(flux[co2_molar_density<1000],aes(date_time,co2_mole_fraction,colour=factor(qc_co2_flux)))+geom_point()

ggplot(flux,aes(date_time,h2o_mole_fraction,colour=factor(qc_h2o_flux)))+geom_point()
ggplot(flux,aes(date_time,h2o_var,colour=factor(qc_h2o_flux)))+geom_point()+ylim(c(0,7000))
ggplot(flux[month(date_time)==7&h2o_strg<5&h2o_strg>(-5)],aes(date_time,h2o_strg))+geom_point()

ggplot(flux,aes(date_time,H,colour=factor(qc_H)))+geom_point()+ylim(c(-100,100))
ggplot(flux,aes(date_time,LE,colour=factor(qc_LE)))+geom_point()+ylim(c(-1000,1000))
ggplot(flux,aes(date_time,ET,colour=factor(qc_h2o_flux)))+geom_point()+ylim(c(-5,5))

ggplot(flux,aes(date_time,`u*`))+geom_point()

# plot u* and CO2 flux in temperature bins

# first plot PPFD_1_1_1 by hour
ggplot(flux[PPFD_1_1_1<90,], aes(date_time, PPFD_1_1_1))+geom_line()+
  facet_wrap(~hour(date_time))

# look at fluxes during low PAR
ggplot(flux[PPFD_1_1_1<90,],aes(date_time,co2_flux,colour=factor(temp.bin)))+
  geom_point()+
  ylim(-2.5,2.5)+
  facet_wrap(~temp.bin)

# plot u* against driving variables (temperature,precip)
# exponential relationship?
ggplot(flux[PPFD_1_1_1<90,],aes(air_temperature,`u*`))+
  geom_point()

ggplot(flux[PPFD_1_1_1<90,],aes(air_temperature,`u*`))+
  geom_point()+facet_wrap(~temp.bin)
# none with rain.
ggplot(flux[PPFD_1_1_1<90,],aes(P_rain_1_1_1,`u*`))+
  geom_point()

# plot distribution of temperature data
ggplot(flux[PPFD_1_1_1<90,], aes(air_temperature))+geom_histogram()

# ddistributon of u* by temperature bins 
ggplot(flux[PPFD_1_1_1<90,], aes(`u*`))+geom_histogram()+facet_wrap(~temp.bin)

# look at co2 flux in u* and temp bins
ggplot(flux[PPFD_1_1_1<90,],aes(u.bin,co2_flux,colour=factor(temp.bin)))+
  geom_point()+
  ylim(-5,5)+
  facet_wrap(~temp.bin)

ggplot(flux[PPFD_1_1_1<10&qc_co2_flux<2&month(date_time)==6,],
       aes(`u*`, co2_flux, colour=temp.bin))+
  geom_point()+
  ylim(-1,2.5)+
  geom_vline(xintercept=0.15)

min(flux$date_time, na.rm=TRUE)


# qc flags, keep 0 and 1
# https://www.licor.com/env/support/EddyPro/topics/flux-quality-flags.html
# u* filter (Aline eliminated data with friction velocity threshold < 0.1)

# question: delete all fluxes or only the specific flux?
ggplot(flux[qc_co2_flux<2&`u*`>0.4,],aes(date_time,co2_flux,colour=factor(qc_co2_flux)))+
  geom_point()+
  ylim(c(-100,100))

ggplot(flux[qc_h2o_flux<2,],aes(date_time,h2o_flux,colour=factor(qc_h2o_flux)))+geom_point()

ggplot(flux,aes(date_time,H,colour=factor(qc_H)))+geom_point()+ylim(c(-100,100))

# plot radiation
ggplot(flux,aes(x=date_time))+
  geom_line(aes(y=Rn_1_1_1, colour="Rn"))+
  geom_point(aes(y=Rn_1_1_1, colour="Rn"))+
  geom_line(aes(y=Rg_1_1_1, colour="Rg"))+
  geom_point(aes(y=Rg_1_1_1, colour="Rg"))+
  geom_line(aes(y=((SHF_1_1_1+SHF_1_2_1)/2), colour="SHF"))+
  geom_point(aes(y=((SHF_1_1_1+SHF_1_2_1)/2), colour="SHF"))+
  geom_line(aes(y=LWin_1_1_1, colour="LWin"))+
  geom_point(aes(y=LWin_1_1_1, colour="LWin"))+
  geom_line(aes(y=LWout_1_1_1, colour="LWout"))+
  geom_point(aes(y=LWout_1_1_1, colour="LWout"))+
  geom_line(aes(y=SWout_1_1_1, colour="SWout"))+
  geom_point(aes(y=SWout_1_1_1, colour="SWout"))

# look at energy balance closure
# first calculate daily sums 

# residual = Rn - G - H - LE
# closure fration = (H + LE) / (Rn + G)
flux[,':=' (eb.res=(Rn_1_1_1 - ((SHF_1_1_1+SHF_1_2_1+SHF_2_1_1 + SHF_2_2_1)/4) - H - LE),
            cf = (H + LE)/(Rn_1_1_1 + ((SHF_1_1_1+SHF_1_2_1+SHF_2_1_1 + SHF_2_2_1)/4)))]


ggplot(flux[eb.res<4000 & eb.res>(-4000)  &
              qc_H<2 & qc_LE<2 & month(date_time)==5,], aes(x=DOY))+
 # geom_line(aes(y=eb.res, colour="eb.res"))+
 # geom_point(aes(y=eb.res, colour="eb.res"))+
  geom_line(aes(y=Rn_1_1_1, colour="Rn_1_1_1"))+
  #geom_point(aes(y=-Rn_1_1_1, colour="Rn_1_1_1"))+
  geom_line(aes(y=((SHF_1_1_1+SHF_1_2_1+SHF_2_1_1+SHF_2_2_1)/4), colour="SHF"))+
 # geom_point(aes(y=((SHF_1_1_1+SHF_1_2_1)/2), colour="SHF"))+
  geom_line(aes(y=H, colour="H"))+
 # geom_point(aes(y=H, colour="H"))+ 
  geom_line(aes(y=LE, colour="LE"))+
  geom_line(aes(y=H+LE+(SHF_1_1_1+SHF_1_2_1+SHF_2_1_1+SHF_2_2_1)/4, colour="H+LE+SHF"))+
  #geom_point(aes(y=LE, colour="LE"))
  facet_grid(.~year)

ggplot(flux[qc_H<2 & qc_LE<2 ,], aes(x=date_time))+
  geom_line(aes(y=cf, colour="closure fraction"))+
  geom_point(aes(y=cf, colour="closure fraction"))+
  ylim(c(-100,100))


# Filter data

# not sure whether to add flux + storage + advection
# book and Cove say to add flux + storage
# eddy pro software says storage is only indicative and has too many assumptions and shouldn't be used

flux1 <- copy(flux)

# remove qc_co2_flux<1
flux[qc_co2_flux>2 | is.na(qc_co2_flux),':=' (co2_flux = NA, 
                                              co2_mole_fraction = NA)]

# remove unreasonable flux values
flux[co2_flux<(-28) | co2_flux>5, ':=' (co2_flux = NA, 
                                             co2_mole_fraction = NA)]

# look at agc: IRGA signal strength
ggplot(flux, aes(date_time,agc_mean))+
  geom_point()+
  ylim(c(25,100))

# remove flux when AGC < 40 (baseline is at ~42)
# Li-7500 manual says that typical values are 55-65% but baselines vary by instrument
# values that approach 100% should be removed because 100% indicates complete sensor blockage
# remove less than 40 and greater than 52
flux[agc_mean<40 | agc_mean>52, ':=' (co2_flux = NA, 
                                      co2_mole_fraction = NA)]


# remove u*<0.15 for low turbulence. (later, don't do this for 2017/2018)
flux[`u*`<0.15, ':=' (co2_flux = NA, 
                      co2_mole_fraction = NA)]

# remove when mole fraction is unreasonable, I think anything over 1000 is really unlikely?
flux[co2_mole_fraction>1000, ':=' (co2_flux = NA, 
                      co2_mole_fraction = NA)]

# there are some bigger flux pulses that remain, not quite sure what to do with them.
ggplot(flux, aes(date_time, co2_mole_fraction))+
  geom_line()

# look at the data by month
ggplot(flux[month(date_time)==9,], aes(DOY,co2_flux, colour=factor(year)))+
  geom_line()


# h20 flux
# remove qc_h2o_flux<1
flux[qc_h2o_flux>2 | is.na(qc_h2o_flux),':=' (h2o_flux = NA, 
                                              h2o_mole_fraction = NA)]

# remove unreasonable flux values
flux[h2o_flux<(-400) | h2o_flux>400, ':=' (h2o_flux = NA, 
                                           h2o_mole_fraction = NA)]

# remove flux when AGC < 40 (baseline is at ~42)
# Li-7500 manual says that typical values are 55-65% but baselines vary by instrument
# values that approach 100% should be removed because 100% indicates complete sensor blockage
# remove less than 40 and greater than 52
flux[agc_mean<40 | agc_mean>52, ':=' (h2o_flux = NA, 
                                      h2o_mole_fraction = NA)]


# remove u*<0.15 for low turbulence. (later, don't do this for 2017/2018)
flux[`u*`<0.15, ':=' (h2o_flux = NA, 
                      h2o_mole_fraction = NA)]

# look at data
ggplot(flux, aes(DOY,h2o_flux, colour=factor(year)))+
  geom_line()

ggplot(flux, aes(DOY,h2o_mole_fraction, colour=factor(year)))+
  geom_line()

# filter H
# remove qc code < 1
flux[qc_H>2 | is.na(qc_H),H := NA]

# remove flux when AGC < 40 (baseline is at ~42)
# Li-7500 manual says that typical values are 55-65% but baselines vary by instrument
# values that approach 100% should be removed because 100% indicates complete sensor blockage
# remove less than 40 and greater than 52
flux[agc_mean<40 | agc_mean>52, H := NA]

# remove unreasonable values 
flux[H<(-1000) | H>1000, H := NA]


# remove u*<0.15 for low turbulence. (later, don't do this for 2017/2018)
flux[`u*`<0.15, H := NA]

# look at data
ggplot(flux[year==2015,], aes(date_time,H))+
  geom_line()


# variance of w and others
ggplot(flux[year==2015,], aes(date_time,w_var))+
  geom_line()

ggplot(flux[year==2015,], aes(date_time,ts_var))+
  geom_line()

ggplot(flux[year==2015,], aes(date_time,co2_var))+
  geom_line()+ylim(-0.05,0.05)

ggplot(flux[year==2015,], aes(date_time,h2o_var))+
  geom_line()+ylim(-5,1000)

ggplot(flux[year==2018,], aes(date_time,`w/ts_cov`))+
  geom_line()
+ylim(-5,1000)



# look by month
# look at the data by month
ggplot(flux[month(date_time)==1,], aes(DOY,H, colour=factor(year)))+
  geom_line()

# filter LE
# remove qc code < 1
flux[qc_LE>2 | is.na(qc_LE),LE := NA]

# remove flux when AGC < 40 (baseline is at ~42)
# Li-7500 manual says that typical values are 55-65% but baselines vary by instrument
# values that approach 100% should be removed because 100% indicates complete sensor blockage
# remove less than 40 and greater than 52
flux[agc_mean<40 | agc_mean>52, LE := NA]

# remove unreasonable values 
flux[LE<(-1000) | LE>30000 , LE := NA]

# remove u*<0.15 for low turbulence. (later, don't do this for 2017/2018)
flux[`u*`<0.15, LE := NA]

# look at data
ggplot(flux, aes(date_time,LE))+
  geom_line()

# look at the data by month
ggplot(flux[month(date_time)==12,], aes(DOY,LE, colour=factor(year)))+
  geom_line()


# look at the data in 20 day increments
ggplot(flux[DOY>=350 & DOY<=370,], aes(DOY,co2_flux, colour=factor(year)))+
  geom_line()
  facet_grid(year~., scales="free_y")


ggplot(flux, aes(date_time,co2_flux, colour=factor(year)))+
  geom_line()

ggplot(flux, aes(DOY,co2_flux, colour=factor(year)))+
  geom_line()

# heatmap plots, flux cut from 5 to -28
f <-ggplot(flux[!is.na(time)&!is.na(DOY),],
           aes(yday(date),hour(date_time)*10+minute(date_time),fill=co2_flux))+
  geom_tile(color= "white",size=0.1) + 
  scale_fill_viridis(name=expression(paste('C',O[2],' flux',sep='')))+
  facet_grid(year~.)+
 scale_y_continuous(breaks=c(0,120,230),
                    labels=c("00:00","12:00","23:00"),
                    expand=c(0,0))+
  scale_x_continuous(breaks =c(31,61,91,121,151,181,211,241,271,301,331,361),limits=c(1,367),
                     labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
                     expand=c(0,0))+
  labs(title= expression("Half-hourly flux (μmol C" *O[2]* m^-2* "se" *c^-1*")"), x="Day", y="Half-hour") +
  theme(
        plot.title=element_text(size = 14),
        axis.text.x=element_text(size=10),
        axis.text.y=element_text(size=8),
        axis.title=element_text(size=11),
        strip.background = element_blank(),
        strip.text=element_text(size=10),
        axis.ticks=element_blank(),
        legend.title=element_text(size=10),
        legend.text=element_text(size=9),
        panel.grid=element_blank(),
        panel.background=element_rect(fill="white"))

f

# time series of rain and temperature
p <- ggplot(flux,aes(date_time,P_rain_1_1_1,colour=factor(year)))+geom_line()

t <- ggplot(flux,aes(date_time,Ta_1_1_1-275.3, colour=factor(year)))+geom_line()

grid.arrange(p,t, nrow=2)

# plot all energy fluxes looking at daily sums
eb_daily <- flux[,lapply(.SD,function (x) {sum(x, na.rm=TRUE)}),by="date",.SDcols=c("Rn_1_1_1",
                                                      "H", "LE",
                                                      "SHF_1_1_1", "SHF_1_2_1",
                                                      "SHF_2_1_1", "SHF_2_2_1")]


ggplot(eb_daily[month(date)==12,], aes(x=yday(date)))+
  # geom_line(aes(y=eb.res, colour="eb.res"))+
  # geom_point(aes(y=eb.res, colour="eb.res"))+
  geom_line(aes(y=Rn_1_1_1, colour="Rn_1_1_1"))+
  #geom_point(aes(y=-Rn_1_1_1, colour="Rn_1_1_1"))+
  geom_line(aes(y=((SHF_1_1_1+SHF_1_2_1+SHF_2_1_1+SHF_2_2_1)/4), colour="SHF"))+
  # geom_point(aes(y=((SHF_1_1_1+SHF_1_2_1)/2), colour="SHF"))+
  geom_line(aes(y=H, colour="H"))+
  # geom_point(aes(y=H, colour="H"))+ 
  geom_line(aes(y=LE, colour="LE"))+
  geom_line(aes(y=H+LE+(SHF_1_1_1+SHF_1_2_1+SHF_2_1_1+SHF_2_2_1)/4, colour="H+LE+SHF"))+
  #geom_point(aes(y=LE, colour="LE"))
  facet_grid(.~year(date))




