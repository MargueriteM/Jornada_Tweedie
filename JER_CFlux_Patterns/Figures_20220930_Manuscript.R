# Create R script to make manuscript figures for Bajada C Flux data

# use filtered and gap-filled NEE data

# load libraries
library(REddyProc)
library(data.table)
library(lubridate)
library(gridExtra)
library(dplyr)
library(ggplot2)
library(ggh4x) # hacks for ggplot
library(gtable)
library(grid)
library(zoo)
library(bit64)
library(viridis)
library(cowplot)
library(scales)

# import filtered flux data file from Eddy Pro as data table
# filtered in: Jornada_EddyPro_Output_Fluxnext_2010_2019.R
ep.units <- (fread("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/ReddyProc/20200427/REddyResults_Us-Jo1_20200428_586625386/output.txt",
                   header=TRUE))[1,]

# import 2010-2019 ReddyProc Data
flux.ep <- fread("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/ReddyProc/20200427/REddyResults_Us-Jo1_20200428_586625386/output.txt",
                 header=FALSE, skip=2,na.strings=c("-9999", "NA","-"),
                 col.names = colnames(ep.units))

# for some reason na.strings won't recognize the -9999
flux.ep[flux.ep == -9999] <- NA

# add the reddyproc data for 2020
# headers changed in 2020 batch
ep.units.2020 <-fread("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/ReddyProc/2020/REddyResults_US-Jo1_20220914_552711071/output.txt",
                    header=TRUE)[1,]


flux.ep2020 <-fread("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/ReddyProc/2020/REddyResults_US-Jo1_20220914_552711071/output.txt",
                     header=FALSE, skip=2,na.strings=c("-9999", "NA","-"),
                     col.names = colnames(ep.units.2020))

# for some reason na.strings won't recognize the -9999
flux.ep2020[flux.ep2020 == -9999] <- NA


# add the reddyproc data for 2021
# headers changed in 2020 batch
ep.units.2021 <-fread("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/ReddyProc/2021/REddyResults_US-Jo1_20221208_443485002/output.txt",
                      header=TRUE)[1,]


flux.ep2021 <-fread("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/ReddyProc/2021/REddyResults_US-Jo1_20221208_443485002/output.txt",
                    header=FALSE, skip=2,na.strings=c("-9999", "NA","-"),
                    col.names = colnames(ep.units.2020))

# for some reason na.strings won't recognize the -9999
flux.ep2021[flux.ep2021 == -9999] <- NA

# add the reddyproc data for 2022
# headers changed in 2020 batch
ep.units.2022 <-fread("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/ReddyProc/2022/REddyResults_US-Jo1_20221208_504774810/output.txt",
                      header=TRUE)[1,]


flux.ep2022 <-fread("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/ReddyProc/2022/REddyResults_US-Jo1_20221208_504774810/output.txt",
                    header=FALSE, skip=2,na.strings=c("-9999", "NA","-"),
                    col.names = colnames(ep.units.2020))

# for some reason na.strings won't recognize the -9999
flux.ep2022[flux.ep2022 == -9999] <- NA


# get the 'edata' to add 2010 to the timeseries eventhough 2010 won't gap fill.... 
setwd("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_EddyPro_filtered")

# import data that was filtered by 3SD filter
load("JER_flux_2010_EddyPro_Output_filtered_SD_20221023.Rdata")

# convert date to POSIXct and get a year, day, hour column
# if this step doesn't work, make sure bit64 library is loaded otherwise the timestamps importa in a non-sensical format
flux_filter_sd_all[,':=' (date_time = parse_date_time(TIMESTAMP_START,"YmdHM",tz="UTC"),
                          date_time_end = parse_date_time(TIMESTAMP_END,"YmdHM",tz="UTC"))][
                            ,':='(Year_end = year(date_time_end),Year=year(date_time),DoY=yday(date_time),
                                  hours = hour(date_time), mins = minute(date_time))]

# there's duplicated data in 2012 DOY 138
flux_filter <- (flux_filter_sd_all[!(duplicated(flux_filter_sd_all, by=c("TIMESTAMP_START")))])

# format data columns for ReddyProc
# Year	DoY	Hour	NEE	LE	H	Rg	Tair	Tsoil	rH	VPD	Ustar 
flux_filter[mins==0, Hour := hours+0.0]
flux_filter[mins==30, Hour := hours+0.5]

edata <- flux_filter[,.(Year,
                        DoY,
                        Hour,
                        FC,
                        LE,
                        H,
                        SW_IN_1_1_1,
                        TA_1_1_1,
                        RH_1_1_1,
                        USTAR)]

# set names to match Reddyproc (don't match _orig columns... they appear the same as the input columns, 
# and 2020 does not have NEE_orig and instead has NEE and NEE_uStar_orig)
setnames(edata,c("FC","LE","H","SW_IN_1_1_1","TA_1_1_1","RH_1_1_1","USTAR"),
         c("NEE","LE","H","Rg","Tair","rH","Ustar"))

# make all Rg<0 equal to 0 becuase ReddyProc won't accept values <0
edata[Rg<0, Rg:=0]

# create a grid of full dates and times
filled <- expand.grid(date=seq(as.Date("2010-01-01"),as.Date("2020-12-31"), "days"),
                      Hour=seq(0,23.5, by=0.5))
filled$Year <- year(filled$date)
filled$DoY <- yday(filled$date)

filled$date <- NULL

edata <- merge(edata,filled,by=c("Year","DoY","Hour"), all=TRUE)

# online tool says hours must be between 0.5 and 24.0 
# therefore add 0.5 to each hour
edata[,Hour := Hour+0.5]

# convert edata to data frame for ReddyProc
edata <- as.data.frame(edata)

# calculate VPD from rH and Tair in hPa (mbar), at > 10 hPa the light response curve parameters change
edata$VPD <- fCalcVPDfromRHandTair(edata$rH, edata$Tair)

# get only 2010 and go back to data table
edata2010 <- as.data.table(subset(edata,Year==2010))
#edata2020 <- as.data.table(subset(edata,Year==2020))
# edata2021 <- as.data.table(subset(edata,Year==2021))

flux.ep <- rbind(edata2010,flux.ep[Year>2010&Year<2020,],
                 flux.ep2020,flux.ep2021,flux.ep2022, fill=TRUE)

# plot to check
# NEE_U95_f graph should have 2010 missing
ggplot(flux.ep, aes(DoY, NEE_U95_f))+geom_line()+facet_grid(.~Year)
# NEE graph should have all years present
ggplot(flux.ep, aes(DoY, NEE))+geom_line()+facet_grid(.~Year)

# graph time-series of NEE, Tair, Precip

# Plot the measured and U50 gap-filled C flux data
plot.nee.hh <- ggplot(flux.ep, aes(DoY,NEE))+
  geom_point(colour="#000000", size=0.17)+
  geom_point(aes(y=NEE_U50_f),data=subset(flux.ep, is.na(NEE)),colour="#808080",size=0.1)+
  facet_grid(.~Year)+
  ylim(c(-15,15))+
  scale_x_continuous(breaks =c(31,211,361),limits=c(1,367),
                     labels=c("Jan","Jul","D"),
                     minor_breaks =c(31,61,91,121,151,181,211,241,271,301,331,361),
                     guide="axis_minor",
                     expand=c(0,0))+
  labs(y=expression("NEE (μmol C" *O[2]*" "*m^-2* "se" *c^-1*")"),
       x="Month")+
  facet_grid(.~Year)+
  theme_bw(base_size=14)+
  theme(strip.background = element_blank(),
       # panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.length =  unit(-0.2,"cm"),
        ggh4x.axis.ticks.length.minor = rel(0.7))

plot.TA.hh <- ggplot(flux_filter_sd_all, aes(DoY, TA_1_1_1))+
  geom_hline(yintercept=0, color="darkgrey")+
  geom_line(linewidth=0.4)+
  facet_grid(.~Year)+
  scale_x_continuous(breaks =c(31,211,361),limits=c(1,367),
                     labels=c("Jan","Jul","D"),
                     minor_breaks =c(31,61,91,121,151,181,211,241,271,301,331,361),
                     guide="axis_minor",
                     expand=c(0,0))+
  labs(y=expression("Temperature ("~degree~"C)"))+
  facet_grid(.~Year)+
  theme_bw(base_size=14)+
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.length =  unit(-0.2,"cm"),
        ggh4x.axis.ticks.length.minor = rel(0.7))


plot.precip.hh <- ggplot(flux_filter_sd_all[P_RAIN_1_1_1<40], aes(DoY, P_RAIN_1_1_1))+
  geom_hline(yintercept=0, color="darkgrey")+
  geom_line()+
  facet_grid(.~Year)+
  scale_x_continuous(breaks =c(31,211,361),limits=c(1,367),
                     labels=c("Jan","Jul","D"),
                     minor_breaks =c(31,61,91,121,151,181,211,241,271,301,331,361),
                     guide="axis_minor",
                     expand=c(0,0))+
  labs(y="Precipitation (mm)",
       x="Month")+
  facet_grid(.~Year)+
  theme_bw(base_size=14)+
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.length =  unit(-0.2,"cm"),
        ggh4x.axis.ticks.length.minor = rel(0.7),
        axis.text.x = element_text(vjust=-0.7))

plot_grid(plot.nee.hh,
             plot.TA.hh,
             plot.precip.hh, nrow=3,
          labels="auto",
          align="v")

# Plot the measured and gap-filled LE data in mm
plot.et.d <- ggplot(subset(flux.ep, Year<2023), aes(DoY,(LE/2454000)*1800))+
  geom_point(colour="#000000", size=0.17)+
  geom_point(aes(y=(LE_f/2454000)*1800),data=subset(flux.ep, is.na((LE/2454000)*1800)),
             colour="#808080",size=0.1)+
  scale_x_continuous(breaks =c(31,211,361),limits=c(1,367),
                     labels=c("Jan","Jul","D"),
                     expand=c(0,0))+
  labs(y=expression("Daily total ET (mm)"),
       x = "Month")+
  facet_grid(.~Year)+
  #ylim(c(-10,10))+
  theme_bw()+
  theme(strip.background = element_blank())

plot_grid(plot.et.d,
          plot.precip.hh, nrow=2,
          labels="auto",
          align="v")

# calculate daily means and 7-day running means from gap-filled data
# Daily cummulative amount of carbon exchange (gC) and 7-day running mean

# calculate daily sums of Co2 flux in umol/m2/sec converted to gC/m2/day
# include 2010 data by calculating a daily mean and multiplying by seconds/day
daily_sum_dt <- as.data.table((flux.ep))
daily_sum_ec <- daily_sum_dt[,list(NEE_daily = sum(NEE_U50_f*1800*1*10^-6*12.01),
                                NEE_daily_mean = mean(NEE, na.rm=TRUE)*86400*1*10^-6*12.01, # scale the mean to daily
                                   GPP_daily = sum(GPP_U50_f*1800*1*10^-6*12.01),
                                Reco_daily = sum(Reco_U50*1800*1*10^-6*12.01),
                                ET_daily = sum((LE_f/2454000)*1800), # (amount of energy to evaporate a unit weight of water; 2454000 J kg-1).
                                ET_daily_mean = mean((LE/2454000),na.rm=TRUE)*86400,
                                Tair_mean = mean(Tair),
                                Tair_max=max(Tair),
                                Tair_min=min(Tair)), 
                          by="Year,DoY"][,list(DoY,
                                               NEE_daily, 
                                               NEE_daily_mean,
                                               GPP_daily,
                                               Reco_daily,
                                               ET_daily,
                                               ET_daily_mean,
                                               Tair_mean,
                                               Tair_max,
                                               Tair_min,
                                               NEE_cum = cumsum(NEE_daily),
                                         GPP_cum = cumsum(GPP_daily),
                                         Reco_cum = cumsum(Reco_daily),
                                         ET_cum = cumsum(ET_daily)),by="Year"]

# create a running mean 
daily_sum_ec[,':=' (NEE_daily_roll = rollmean(x=NEE_daily,
                                           k=7,
                                           fill=NA),
                 ET_daily_roll = rollmean(x=ET_daily,
                                          k=7,
                                          fill=NA))]

# look at the gap-filled daily sums vs the mean calculation for all years
ggplot(daily_sum_ec)+geom_point(aes(x=NEE_daily, y=NEE_daily_mean))+geom_abline(slope=1,intercept=0)
ggplot(daily_sum_ec)+geom_point(aes(x=ET_daily, y=ET_daily_mean))+geom_abline(slope=1,intercept=0)

# timeseries
ggplot(daily_sum_ec)+geom_point(aes(x=DoY, y=NEE_daily),colour="darkgrey")+
  geom_point(aes(x=DoY, y=NEE_daily_mean),colour="lightblue",size=0.5)+
  facet_grid(.~Year)

ggplot(daily_sum_ec)+geom_point(aes(x=DoY, y=ET_daily),colour="darkgrey")+
  geom_point(aes(x=DoY, y=ET_daily_mean),colour="lightblue",size=0.5)+
  facet_grid(.~Year)


# add a date variable to daily_sum_ec
daily_sum_ec[Year==2010,date:= as.Date(DoY-1, origin = "2010-01-01")]
daily_sum_ec[Year==2011,date:= as.Date(DoY-1, origin = "2011-01-01")]
daily_sum_ec[Year==2012,date:= as.Date(DoY-1, origin = "2012-01-01")]
daily_sum_ec[Year==2013,date:= as.Date(DoY-1, origin = "2013-01-01")]
daily_sum_ec[Year==2014,date:= as.Date(DoY-1, origin = "2014-01-01")]
daily_sum_ec[Year==2015,date:= as.Date(DoY-1, origin = "2015-01-01")]
daily_sum_ec[Year==2016,date:= as.Date(DoY-1, origin = "2016-01-01")]
daily_sum_ec[Year==2017,date:= as.Date(DoY-1, origin = "2017-01-01")]
daily_sum_ec[Year==2018,date:= as.Date(DoY-1, origin = "2018-01-01")]
daily_sum_ec[Year==2019,date:= as.Date(DoY-1, origin = "2019-01-01")]
daily_sum_ec[Year==2020,date:= as.Date(DoY-1, origin = "2020-01-01")]
daily_sum_ec[Year==2021,date:= as.Date(DoY-1, origin = "2021-01-01")]
daily_sum_ec[Year==2022,date:= as.Date(DoY-1, origin = "2022-01-01")]


# calculate daily and cumulative precip
precip_daily <- flux_filter[,date:=as.Date(date_time)][!is.na(P_RAIN_1_1_1),
                                      list(precip.tot = sum(P_RAIN_1_1_1)),
                                      by="date,Year"][Year>2010,precip.cum:=cumsum(precip.tot),by="Year"]



# combine daily fluxes with daily precip
daily_sum <- full_join(daily_sum_ec,precip_daily)

# calculate cumulative sums
annual_cum <- daily_sum[,list(NEE_cum = sum(NEE_daily),
                              GPP_cum = sum(GPP_daily),
                              Reco_cum = sum(Reco_daily),
                              ET_cum = sum(ET_daily),
                              precip_cum = sum(precip.tot)),
                        by="Year"]

# plot annnual cumulative
plot.nee.cum <- ggplot(annual_cum, aes(factor(Year), NEE_cum))+
  geom_bar(stat="identity")+
  labs(y="Annual NEE (gC/m2)")+
  theme_bw()

plot.gpp.cum <- ggplot(annual_cum, aes(factor(Year), GPP_cum))+
  geom_bar(stat="identity")+
  labs(y="Annual GPP (gC/m2)")+
  theme_bw()

plot.reco.cum <- ggplot(annual_cum, aes(factor(Year), Reco_cum))+
  geom_bar(stat="identity")+
  labs(y="Annual Reco (gC/m2)",x="Year")+
  theme_bw()


plot.et.cum <- ggplot(annual_cum, aes(factor(Year), ET_cum))+
  geom_bar(stat="identity")+
  labs(y="Annual ET (mm/m2)", x="Year")+
  theme_bw()

plot_grid(plot.nee.cum+theme(axis.text.x =element_blank(), axis.title.x=element_blank()),
          plot.gpp.cum+theme(axis.text.x=element_blank(), axis.title.x=element_blank()),
          plot.reco.cum+theme(axis.text.x=element_blank(), axis.title.x=element_blank()),
          plot.et.cum, nrow=4)

# Plot cumulative sum with 7 day running mean and 2010 data based on mean daily multiplied to day
plot.nee.daily <- ggplot(daily_sum, aes(DoY, NEE_daily))+
  geom_line(colour="#000000", size=0.8)+
  #geom_line(aes(yday(date),NEE_daily_roll),colour="#A0A0A0",size=0.4)+
  geom_line(data=daily_sum[Year==2010,],aes(yday(date),NEE_daily_mean),colour="#A0A0A0",size=0.4)+
  geom_line(data=daily_sum[Year!=2017 & Year!=2021,],aes(x=yday(date),y=NEE_cum/100),colour="forestgreen",size=0.5)+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=c(166,273))+
  facet_grid(.~Year)+
  scale_x_continuous(breaks =c(31,211,361),limits=c(1,367),
                     labels=c("Jan","Jul","D"),
                     minor_breaks =c(31,61,91,121,151,181,211,241,271,301,331,361),
                     guide="axis_minor",
                     expand=c(0,0))+
  scale_y_continuous(name=expression("Daily NEE (gC" *m^-2*")"),
                                     sec.axis=sec_axis(~.*50, name="Cummulative"))+
  facet_grid(.~Year)+
  theme_bw(base_size=14)+
  theme(strip.background = element_blank(),
        # panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.length =  unit(-0.2,"cm"),
        ggh4x.axis.ticks.length.minor = rel(0.7))


# graph GPP and Reco
plot.gpp.daily <- ggplot(daily_sum[Year>2010,], aes(DoY, GPP_daily))+
  geom_line(colour="#000000")+
  geom_hline(yintercept=0)+
  ylim(c(0,3.5))+
 labs(y=expression("Daily cumulative GPP (gC" *m^-2*")"),
       x="Month")+
  facet_grid(.~Year)+
  scale_x_continuous(breaks =c(31,211,361),limits=c(1,367),
                     labels=c("Jan","Jul","D"),
                     minor_breaks =c(31,61,91,121,151,181,211,241,271,301,331,361),
                     guide="axis_minor",
                     expand=c(0,0))+
  facet_grid(.~Year)+
  theme_bw(base_size=14)+
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.length =  unit(-0.2,"cm"),
        ggh4x.axis.ticks.length.minor = rel(0.7))

# Plot cumulative sum with 7 day running mean
plot.reco.daily <- ggplot(daily_sum[Year>2010,], aes(DoY, Reco_daily))+
  geom_line(colour="#000000")+
  geom_hline(yintercept=0)+ylim(c(0,3.5))+
  labs(y=expression("Daily Reco (gC" *m^-2*")"),
       x="Month")+
  facet_grid(.~Year)+
  scale_x_continuous(breaks =c(31,211,361),limits=c(1,367),
                     labels=c("Jan","Jul","D"),
                     minor_breaks =c(31,61,91,121,151,181,211,241,271,301,331,361),
                     guide="axis_minor",
                     expand=c(0,0))+
  theme_bw(base_size=14)+
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.length =  unit(-0.2,"cm"),
        ggh4x.axis.ticks.length.minor = rel(0.7),
        axis.text.x = element_text(vjust=-0.7))

# combine daily graphs
plot_grid(plot.nee.daily,
          plot.gpp.daily,
          plot.reco.daily, nrow=3,
          labels=c("a","b","c"),
          align="v")

# Plot daily ET sum with 7 day running mean
pET <- ggplot(daily_sum[Year>2010,], aes(DoY, ET_daily))+
  geom_line(colour="royalblue2")+
  #geom_line(aes(DoY,ET_daily_roll),colour="navyblue")+
  geom_line(data=daily_sum[Year==2010,],aes(yday(date),ET_daily_mean),colour="navyblue",size=0.4)+
  geom_line(aes(x=yday(date),y=ET_cum/50),colour="#000000",size=0.5)+
    geom_hline(yintercept=0)+
  scale_x_continuous(breaks =c(31,211,361),limits=c(1,367),
                     labels=c("Jan","Jul","D"),
                     minor_breaks =c(31,61,91,121,151,181,211,241,271,301,331,361),
                     guide="axis_minor",
                     expand=c(0,0))+
  scale_y_continuous(sec.axis=sec_axis(~.*50, name="Cummulative"))+
  labs(#title="Daily ET and weekly running mean from 2011-2019",
       y=expression("Daily ET (mm)"),
       x="Month")+
  facet_grid(.~Year)+
  theme_bw(base_size=14)+
  theme(strip.background = element_blank(),
        # panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.length =  unit(-0.2,"cm"),
        ggh4x.axis.ticks.length.minor = rel(0.7))

# graph NEE with cumulative rain 
# daily rainfall and cumulative rain
plot.precip.daily <- ggplot(daily_sum)+
 geom_col(aes(x=yday(date), y=precip.tot), fill="#A0A0A0",colour="#808080")+
  geom_line(aes(x=yday(date),y=precip.cum/5),colour="#000000",size=0.5)+
  scale_x_continuous(breaks =c(31,211,361),limits=c(1,367),
                     labels=c("Jan","Jul","D"),
                     minor_breaks =c(31,61,91,121,151,181,211,241,271,301,331,361),
                     guide="axis_minor",
                     expand=c(0,0))+
  facet_grid(.~Year)+
  scale_y_continuous(sec.axis=sec_axis(~.*5, name="Cummulative"))+
  labs(y="Daily Rain (mm)",x="Month")+
  theme_bw(base_size=14)+
  theme(strip.background = element_blank(),
        # panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.length =  unit(-0.2,"cm"),
        ggh4x.axis.ticks.length.minor = rel(0.7))


# graph time-series of daily NEE, ET, rain
plot_grid(plot.nee.daily+theme(axis.title.x=element_blank(), axis.text.x = element_blank(),plot.margin = unit(c(0, 0, 0, 0), "cm")),
          pET+theme(axis.title.x=element_blank(), axis.text.x = element_blank(), strip.text.x=element_blank(),plot.margin = unit(c(0.1, 0, 0, 0), "cm")),
          plot.precip.daily+theme(axis.text.x = element_text(vjust = -0.5),strip.text.x=element_blank(),plot.margin = unit(c(t = 0.1, r = 0, b = 0, l = 0), "cm")),
          nrow=3,
          align="v")


# graph cumulative NEE and cumulative rain
ggplot(daily_sum, aes(DoY))+
  geom_line(aes(y=-NEE_cum),colour="green")+
  geom_line(aes(y=precip.cum/2),colour="blue")+
  facet_wrap(.~Year)

# graph cumulatives for multiple years in one panel
plot.nee.an.cum <- ggplot(daily_sum[Year!=2010,], aes(DoY, NEE_cum,color=factor(Year)))+
  geom_line()+
  labs(x="Day of Year",title="Cumulative NEE")+
  scale_y_continuous(position="right",name=expression("gC" *m^-2*""))+
  scale_color_viridis_d()+
  theme_bw(base_size = 14)

plot.precip.an.cum <- ggplot(daily_sum[Year!=2010,], aes(DoY, precip.cum,color=factor(Year)))+
  geom_line()+
  labs(x="Day of Year",title="Cumulative Rainfall")+
 scale_y_continuous(position="right",name="mm")+
  scale_color_viridis_d(name="Year")+
  theme_bw(base_size = 14)

plot.leg <- get_legend(plot.precip.an.cum)

plot_grid(plot_grid(plot.nee.an.cum+theme(legend.position = "none"),
                           plot.precip.an.cum+theme(legend.position = "none"),
          ncol=2),
          plot.leg,
          rel_widths = c(1,0.1))

# graph cumulative rainfall and cumulative ET
ggplot(annual_cum, aes(precip_cum, ET_cum))+geom_point()+geom_abline(intercept=0,slope=1)

# look at seasonality
# calculate DoY mean, min, max, se, and sd
daily_stats <- daily_sum[, unlist(lapply(.SD,
                  function(x) list(mean = mean(x,na.rm=TRUE),
                                   max = max(x, na.rm=TRUE),
                                   min = min(x, na.rm=TRUE),
                                   var = var(x, na.rm=TRUE),
                                   sd = sd(x, na.rm=TRUE))),
           recursive = FALSE),
           by="DoY",
           .SDcols = c('NEE_daily', 'GPP_daily',"Reco_daily","ET_daily",
                       "NEE_cum","Reco_cum","GPP_cum","ET_cum")]


# graph daily mean and sd
ggplot(daily_stats, aes(DoY,NEE_daily.mean))+
  geom_point(size=0.5)+
  geom_line(size=0.2)+
  geom_hline(yintercept=0)+
  geom_ribbon(aes(max=(NEE_daily.mean+NEE_daily.sd),min=(NEE_daily.mean-NEE_daily.sd)),alpha=0.3)+
  # scale_x_continuous(breaks =c(31,211,361),limits=c(1,367),
  #                    labels=c("Jan","Jul","Dec"),
  #                    minor_breaks =c(31,61,91,121,151,181,211,241,271,301,331,361),
  #                    guide="axis_minor",
  #                    expand=c(0,0))+
  labs(y = expression("Daily NEE (gC" *m^-2*")"), x="Day of Year")+
  theme_bw(base_size=14)+
  theme(axis.ticks.length =  unit(-0.2,"cm"),
        ggh4x.axis.ticks.length.minor = rel(0.7),
        axis.text.x = element_text(vjust = -0.5))
  
  
# graph the variance around the daily means
ggplot(daily_stats[DoY!=366,], aes(DoY,((NEE_cum.var))))+
  geom_point(size=0.5)+
  geom_line(size=0.2)+
  geom_vline(xintercept=c(166,273))+
  labs(y = expression("Variance NEE (gC" *m^-2*")"^2), x="Day of Year")+
  theme_bw(base_size=14)+
  theme(axis.ticks.length =  unit(-0.2,"cm"),
        ggh4x.axis.ticks.length.minor = rel(0.7),
        axis.text.x = element_text(vjust = -0.5))

# graph the cumulative with SD
ggplot(daily_stats, aes(DoY,NEE_cum.mean))+
  geom_point(size=0.3)+
  geom_ribbon(aes(max=(NEE_cum.mean+NEE_cum.sd),min=(NEE_cum.mean-NEE_cum.sd)),alpha=0.3)+
  theme_bw()



# graph the variance
ggplot(daily_stats, aes(DoY,NEE_cum.sd))+
  geom_point()+
 # geom_errorbar(aes(max=(NEE_cum.mean+NEE_cum.sd),min=(NEE_cum.mean-NEE_cum.sd)))
geom_point()
