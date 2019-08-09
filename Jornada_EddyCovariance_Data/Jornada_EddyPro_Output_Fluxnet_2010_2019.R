############################################
#  This code gets data output by Eddy Pro  #
#           written by: M. Mauritz         #
#             May 2019                     #
#    update: August 2019                   #
############################################

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
# EddyPro 7.0.4 has column name slip problems, using Fluxnet output!

# read the data for 2015-2018
flux2015 <- fread("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_2015/eddypro_JER_2015_fluxnet_2019-08-06T050846_adv.csv",
               sep=",", header=TRUE, na.strings=c("-9999"))

flux2016 <- fread("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_2016/eddypro_JER_2016_fluxnet_2019-08-01T191226_adv.csv",
               sep=",", header=TRUE, na.strings=c("-9999"))

flux2017 <- fread("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_2017/eddypro_JER_2017_fluxnet_2019-08-02T022734_adv.csv",
                  sep=",", header=TRUE, na.strings=c("-9999"))

flux2018 <- fread("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_2018/eddypro_JER_2018_fluxnet_2019-08-02T101350_adv.csv",
                  sep=",", header=TRUE, na.strings=c("-9999"))

# combine all individual years of flux runs
flux <- rbind(flux2015, flux2016, flux2017, flux2018)

# format date
flux[,':=' (date_time = parse_date_time(TIMESTAMP_END,"YmdHM",tz="UTC"))][
  ,':='(date=as.Date.POSIXct(date_time),month=month(date_time),year=year(date_time))]


# make some plots
# graph precipitation
ggplot(flux,aes(date_time,P_RAIN_1_1_1))+geom_line()
# graph soil moisture
ggplot(flux,aes(date_time,SWC_1_1_1))+geom_line()

# graph air temp
ggplot(flux,aes(date_time,TA_1_1_1))+geom_line()

# co2 flux
ggplot(flux[FC>(-10)&FC<10,], aes(date_time,FC))+
  geom_line()+
  ylim(c(-10,30))

# H
ggplot(flux[H>(-400)&FC<800,], aes(date_time,H))+
  geom_line()

# LE
ggplot(flux[LE>(-400)&FC<800,], aes(date_time,LE))+
  geom_line()


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




