############################################
#  This code gets data output by Eddy Pro  #
#           written by: M. Mauritz         #
#                  May 2019                #
############################################

# load libraries
library(ggplot2) # library for making figures in ggplot package
library(lubridate) # library for easier date manipulation 
library(data.table) # library for data table which is more efficient with large data sets
library(reader)
library(tidyr)
library(lsr) # contains quantileCut function
#############
# IMPORT DATA
#############
# Full Eddy Covariance output:
setwd("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/Test_20190626")

# file info
fileinfo2 <- scan("eddypro_2015_2018_full_output_2019-06-28T144511_adv.csv",
                  what='',sep=",",nlines=1)
fileinfo2 <- data.table(t(fileinfo2))
# read only the first row to get the units
flux.units <- (fread("eddypro_2015_2018_full_output_2019-06-28T144511_adv.csv",header=TRUE,skip=1))[1,]
# read the data, skippping the units row
flux <- fread("eddypro_2015_2018_full_output_2019-06-28T144511_adv.csv", sep=",",skip=3,
             header=FALSE, na.strings=c("-9999","-9999.0","NAN","#NAME?"),col.names=colnames(flux.units))

# format date
flux[,date_time := paste(date,time,sep=" ")]
flux[,':=' (date=as.Date(date),
            date_time = as.POSIXct(date_time, format="%Y-%m-%d %H:%M"),
            year=year(date_time))]


# import the biomet data

# read only the first row to get the units
biomet.units <- (fread("eddypro_2015_2018_biomet_2019-06-27T221717_exp.csv",header=TRUE))[1,]
# read the data, skippping the units row
biomet <- fread("eddypro_2015_2018_biomet_2019-06-27T221717_exp.csv", sep=",",skip=2,
              header=FALSE, na.strings=c("-9999","-9999.0","NAN","#NAME?"),col.names=colnames(biomet.units))

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
  geom_point()+
  ylim(c(-5,5))


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
  ylim(-2.5,2.5)+
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


# Preliminary data filtering for Anthony (with u*) in 2015
# and for Dawn Browning (without u*) in 2017, 2018

# not sure whether to add flux + storage + advection
# book and Cove say to add flux + storage
# eddy pro software says storage is only indicative and has too many assumptions and shouldn't be used

flux1 <- copy(flux)

# remove qc_co2_flux<1
flux[qc_co2_flux>2 | is.na(qc_co2_flux),co2_flux := NA]

# remove unreasonable flux values
flux[co2_flux<(-4000) | co2_flux>4000, co2_flux := NA]

# look at agc: IRGA signal strength
ggplot(flux, aes(date_time,AGC_mean))+
  geom_point()+
  ylim(c(25,100))

# remove flux when AGC < 40 (baseline is at ~42)
# Li-7500 manual says that typical values are 55-65% but baselines vary by instrument
# values that approach 100% should be removed because 100% indicates complete sensor blockage
# remove less than 40 and greater than 52
flux[AGC_mean<40 | AGC_mean>52, co2_flux := NA]


# remove u*<0.15 for low turbulence. (later, don't do this for 2017/2018)
flux[`u*`<0.15, co2_flux := NA]

# there are some bigger pulses that remain, not quite sure what to do with them.

# look at the data by month
ggplot(flux[month(date_time)==12,], aes(DOY,co2_flux, colour=factor(year)))+
  geom_line()

# look at data without spikes removed and aligned with rain
fluxplot <- ggplot(flux[year==2016,], aes(date_time,co2_flux))+
  geom_line()

rainplot <- ggplot(flux[year==2016,], aes(date_time,P_rain_1_1_1))+
  geom_line()

windplot <- ggplot(flux[year==2016,], aes(date_time,MWS_1_1_1))+
  geom_line()


grid.arrange(fluxplot,rainplot,windplot,nrow=3)

# filter H
# remove qc code < 1
flux[qc_H>2 | is.na(qc_H),H := NA]

# remove flux when AGC < 40 (baseline is at ~42)
# Li-7500 manual says that typical values are 55-65% but baselines vary by instrument
# values that approach 100% should be removed because 100% indicates complete sensor blockage
# remove less than 40 and greater than 52
flux[AGC_mean<40 | AGC_mean>52, H := NA]

# remove unreasonable values 
flux[H<(-1000) | H>1000, H := NA]


# remove u*<0.15 for low turbulence. (later, don't do this for 2017/2018)
flux[`u*`<0.15, H := NA]

# look at data
ggplot(flux, aes(date_time,H))+
  geom_line()

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
flux[AGC_mean<40 | AGC_mean>52, LE := NA]

# remove unreasonable values 
flux[LE<(-1000) | LE>30000 , LE := NA]


# remove u*<0.15 for low turbulence. (later, don't do this for 2017/2018)
flux[`u*`<0.15, LE := NA]

# look at data
ggplot(flux, aes(date_time,LE))+
  geom_line()

#


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
