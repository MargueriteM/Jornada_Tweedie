###################################################
# Make figures for SEL Lab Meeting, 11 Jan 2022   #
#      Show air temp, precip, C flux patterns     #
#    focus on:                                    #
#     * seasonal and interannual daily flux       #
#     * yearly cumulative flux                    #
###################################################

# Data to use: 
# EddyPro and EddyRe processed and gap-filled NEE data
# Biomet 1 climate data that contains site-level averages

# known issues as of 27 March 2020: 
# * timestamp shift in tower and SN data (can be fixed: "~/Desktop/R/R_programs/Tweedie/Jornada/Jornada_Tweedie/Jornada Climate Data/test_fixingTimestamps.R")
# * 2010 flux gap-fill missing because Rs data is missing. Gap-fill with NRCS data!! (can be done, got derailed by time-stamps)

# 14 May 2022: 
# added graphs for CZ presentation to carbon group

# 20 May 2022:
# added graphs for CZ All Hands

# load libraries
library(REddyProc)
library(data.table)
library(lubridate)
library(gridExtra)
library(dplyr)
library(ggplot2)
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


# get the 'edata' to add 2010 to the timeseries eventhough 2010 won't gap fill.... 
setwd("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_EddyPro_filtered")

# import data that was filtered by 3SD filter
load("JER_flux_2010_EddyPro_Output_filtered_SD_20220913.Rdata")

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

setnames(edata,c("FC","LE","H","SW_IN_1_1_1","TA_1_1_1","RH_1_1_1","USTAR"),
         c("NEE_orig","LE_orig","H_orig","Rg_orig","Tair_orig","rH_orig","Ustar"))

# make all Rg<0 equal to 0 becuase ReddyProc won't accept values <0
edata[Rg_orig<0, Rg_orig:=0]

# create a grid of full dates and times
filled <- expand.grid(date=seq(as.Date("2010-01-01"),as.Date("2021-12-31"), "days"),
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
edata2020 <- as.data.table(subset(edata,Year==2020))
edata2021 <- as.data.table(subset(edata,Year==2021))

flux.ep <- rbind(edata2010,flux.ep,edata2020, edata2021, fill=TRUE)

###Questions
# When is peak CO2 uptake?
# How much does peak CO2 uptake vary from year to year?
# What is the inter-annual variation of CO2 budgets at JER?
  
  ###
  
  # Methods {data-navmenu="Questions and Methods"}
  
  # Monthly Distribution {data-navmenu="Site Climate"}
#  Monthly air temperature and total rainfall from 2010 to 2019


temp_monthly <- flux_filter[!is.na(TA_1_1_1),.(mean.temp = mean(TA_1_1_1),
                                                  min.temp = min(TA_1_1_1),
                                                  max.temp = max(TA_1_1_1)),
                            by=c(month,Year)]
### Monthly mean Air Temperature
# Graph annual distribution of monthly data
ggplot(temp_monthly, aes(month,mean.temp,fill=factor(month)))+
  geom_boxplot()+
  geom_hline(yintercept=0)+
  ylim(c(-25,42))+
  labs(y=expression("Mean Temperature ("~degree~"C)"), x="Month")+
  scale_x_continuous(breaks=c(seq(1,12,1)),
                     labels=c("J","F","M","A","M","J","J","A","S","O","N","D"))+
  scale_fill_viridis_d()+
  theme_bw()+
  theme(legend.position="none")

### Monthly max Air Temp
# Graph annual distribution of Max monthly temp data
ggplot(temp_monthly, aes(month,max.temp,fill=factor(month)))+
  geom_boxplot()+
  geom_hline(yintercept=0)+
  ylim(c(-25,42))+
  labs(y=expression("Maximum Temperature ("~degree~"C)"), x="Month")+
  scale_x_continuous(breaks=c(seq(1,12,1)),
                     labels=c("J","F","M","A","M","J","J","A","S","O","N","D"))+
  scale_fill_viridis_d()+
  theme_bw()+
  theme(legend.position="none")

### Monthly min Air Temp
ggplot(temp_monthly, aes(month,min.temp,fill=factor(month)))+
  geom_boxplot()+
  geom_hline(yintercept=0)+
  ylim(c(-25,42))+
  labs(y=expression("Minimum Temperature ("~degree~"C)"), x="Month")+
  scale_x_continuous(breaks=c(seq(1,12,1)),
                     labels=c("J","F","M","A","M","J","J","A","S","O","N","D"))+
  scale_fill_viridis_d()+
  theme_bw()+
  theme(legend.position="none")

## Column 2 {.tabset}
flux_filter[,month:=month(date_time_end)]
precip_monthly <- flux_filter[!is.na(P_RAIN_1_1_1),list(precip.tot = sum(P_RAIN_1_1_1)),
                              by="month,Year"][Year>2010,precip.cum:=cumsum(precip.tot),by="Year"]

precip_monthly[,year_lab := ifelse(month==12, Year, NA)]

# daily precipitation
precip_daily <- flux_filter[,date:=as.Date(date_time_end)][!is.na(P_RAIN_1_1_1),list(precip.tot = sum(P_RAIN_1_1_1)),
                              by="date,Year"][Year>2010,precip.cum:=cumsum(precip.tot),by="Year"]



### Monthly Rainfall
# Graph annual distribution of monthly data
ggplot(precip_monthly, aes(month,precip.tot,fill=factor(month)))+
  geom_boxplot()+
  labs(y="Total Rainfall (mm)", x="Month")+
  scale_x_continuous(breaks=c(seq(1,12,1)),
                     labels=c("J","F","M","A","M","J","J","A","S","O","N","D"))+
  scale_fill_viridis_d(option="plasma")+
  theme_bw()+
  theme(legend.position="none")

### Cumulative Rainfall 
# graph annual cumulative rainfall
p1 <- ggplot(precip_monthly[Year!=2010,], aes(month,precip.cum,colour=factor(Year)))+
  geom_point()+
  geom_line()+
  geom_label(aes(label=year_lab), size=10, label.size=2)+
  labs(y="Cummulative Rainfall (mm)", x="Month")+
  scale_x_continuous(breaks=c(seq(1,12,1)),
                     labels=c("J","F","M","A","M","J","J","A","S","O","N","D"))+
  scale_colour_viridis_d()+
  theme_bw(base_size=26)+
  theme(legend.position="none")

#png(file="~/Desktop/TweedieLab/AGU/2020/CumulativeRain.png",
 #   width=800,height=540)
p1
dev.off()

# daily rainfall and cumulative rain
ggplot(precip_daily)+
  geom_point(aes(x=yday(date),y=precip.cum/5),colour="blue",size=0.5)+
  geom_col(aes(x=yday(date), y=precip.tot), fill="black",colour="black")+
  facet_grid(.~Year)+
  theme_bw()+
  labs(y="Total daily Rain (mm)",x="Day of Year")+
  scale_y_continuous(sec.axis=sec_axis(~.*5, name="Cummulative Rain (mm)"))


# Climate Envelope 
# graph 'climate envelopes'
monthly.p.t <- merge(precip_monthly,temp_monthly,by=c("Year","month"),all=TRUE)
monthly.p.t <- rbind(monthly.p.t,
                     data.frame(precip.tot=c(25,25,75),mean.temp=c(5,20,25),env_lab=c("Cool, Dry","Warm, Dry","Warm, Wet")),
                     fill=TRUE)

# save monthly precipitation and temperature for Mariana
# write.csv(monthly.p.t,
        #  "~/Desktop/OneDrive - University of Texas at El Paso/SEL_Phenology/MetData_Temp_Precip/JER_monthly_TempPrecip_2010_2019.csv",
        #  row.names=FALSE)

p2 <- ggplot(monthly.p.t, aes(mean.temp,precip.tot,colour=factor(month)))+
  geom_point(size=3)+
  geom_label(aes(label=env_lab),colour="black", size=10, label.size=2)+
  labs(x=expression("Mean Temperature ("~degree~"C)"), y="Rainfall (mm)")+
  scale_color_viridis_d(name="Month",
                        labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
                        option="plasma")+
  theme_bw(base_size=26)+
  guides(name="Month")

#png(file="~/Desktop/TweedieLab/AGU/2020/ClimateEnvelope.png",
#    width=800,height=540)
p2
dev.off()


# Measured & gap-filled C Flux {data-navmenu="C Flux Data"}


## Column 
# Plot the measured and U50 gap-filled C flux data
ggplot(subset(flux.ep, Year<2022), aes(DoY,NEE_orig))+
  geom_line(colour="#440154FF")+
  geom_line(aes(y=NEE_U50_f),data=subset(flux.ep, is.na(NEE_orig)&Year<2022),colour="#55C667FF",size=0.15)+
  scale_x_continuous(breaks =c(31,211,361),limits=c(1,367),
                     labels=c("J","J","D"),
                     expand=c(0,0))+
  labs(y=expression("Half-hourly NEE (μmol C" *O[2]*" "*m^-2* "se" *c^-1*")"),
       x = "Month")+
  facet_grid(.~Year)+
  ylim(c(-10,10))+
  theme_bw()+
  theme(strip.background = element_blank())

# Plot the measured and gap-filled LE data
ggplot(subset(flux.ep, Year<2022), aes(DoY,LE_orig))+
  geom_line(colour="blue")+
  geom_line(aes(y=LE_f),data=subset(flux.ep, is.na(LE_orig)&Year<2022),colour="light blue",size=0.15)+
  scale_x_continuous(breaks =c(31,211,361),limits=c(1,367),
                     labels=c("J","J","D"),
                     expand=c(0,0))+
  labs(y=expression("Half-hourly LE (W" *m^-2*")"),
       x = "Month")+
  facet_grid(.~Year)+
  #ylim(c(-10,10))+
  theme_bw()+
  theme(strip.background = element_blank())

# Plot the measured and gap-filled LE data in mm
ggplot(subset(flux.ep, Year<2022), aes(DoY,(LE_orig/2454000)*1800))+
  geom_line(colour="blue")+
  geom_line(aes(y=(LE_f/2454000)*1800),data=subset(flux.ep, is.na((LE_orig/2454000)*1800)&Year<2022),colour="light blue",size=0.15)+
  scale_x_continuous(breaks =c(31,211,361),limits=c(1,367),
                     labels=c("J","J","D"),
                     expand=c(0,0))+
  labs(y=expression("Daily total ET (mm)"),
       x = "Month")+
  facet_grid(.~Year)+
  #ylim(c(-10,10))+
  theme_bw()+
  theme(strip.background = element_blank())

# Daily C Fluxes {data-navmenu="C Flux Data"}
# Daily cummulative amount of carbon exchange (gC) and 7-day running mean

## Column 
# calculate daily sums of Co2 flux in umol/m2/sec converted to gC/m2/day
daily_sum_dt <- as.data.table(subset(flux.ep))
daily_sum <- daily_sum_dt[,list(NEE_daily = sum(NEE_U50_f*1800*1*10^-6*12.01),
                                GPP_daily = sum(GPP_U50_f*1800*1*10^-6*12.01),
                                Reco_daily = sum(Reco_U50*1800*1*10^-6*12.01),
                                ET_daily = sum((LE_f/2454000)*1800), # (amount of energy to evaporate a unit weight of water; 2454000 J kg-1).
                                Tair_mean = mean(Tair),
                                Tair_max=max(Tair),
                                Tair_min=min(Tair)), 
                          by="Year,DoY"]

# create a running mean 
daily_sum[,':=' (NEE_daily_roll = rollmean(x=NEE_daily,
                                      k=7,
                                      fill=NA),
          ET_daily_roll = rollmean(x=ET_daily,
                                     k=7,
                                     fill=NA))]

# add a date variable to daily_sum
daily_sum[Year==2010,date:= as.Date(DoY-1, origin = "2010-01-01")]
daily_sum[Year==2011,date:= as.Date(DoY-1, origin = "2011-01-01")]
daily_sum[Year==2012,date:= as.Date(DoY-1, origin = "2012-01-01")]
daily_sum[Year==2013,date:= as.Date(DoY-1, origin = "2013-01-01")]
daily_sum[Year==2014,date:= as.Date(DoY-1, origin = "2014-01-01")]
daily_sum[Year==2015,date:= as.Date(DoY-1, origin = "2015-01-01")]
daily_sum[Year==2016,date:= as.Date(DoY-1, origin = "2016-01-01")]
daily_sum[Year==2017,date:= as.Date(DoY-1, origin = "2017-01-01")]
daily_sum[Year==2018,date:= as.Date(DoY-1, origin = "2018-01-01")]
daily_sum[Year==2019,date:= as.Date(DoY-1, origin = "2019-01-01")]


# calculate cumulative sums
daily_cum <- daily_sum[,list(NEE_cum = sum(NEE_daily),
                                 GPP_cum = sum(GPP_daily),
                                 Reco_cum = sum(Reco_daily),
                                 ET_cum = sum(ET_daily)),
                           by="Year"]

# Plot cumulative sum with 7 day running mean
p3 <- ggplot(daily_sum[Year>2010&Year<2020,], aes(DoY, NEE_daily))+
  geom_line(colour="#55C667FF")+
  geom_line(aes(yday(date),NEE_daily_roll),colour="#440154FF")+
  geom_hline(yintercept=0)+
  scale_x_continuous(breaks =c(31,211,361),limits=c(1,367),
                     labels=c("Jan","Jul","D"),
                     expand=c(0,0))+
  labs(title="Daily NEE and weekly running mean from 2011-2019",
       y=expression("Daily cumulative NEE (gC" *m^-2*")"),
       x="Month")+
  facet_grid(.~Year)+
  theme_bw(base_size=26)+
  theme(strip.background = element_blank())

#png(file="~/Desktop/TweedieLab/AGU/2020/NEEdaily.png",
  #  width=1920,height=1080)
p3
dev.off()

# graph GPP and Reco
pgpp <- ggplot(daily_sum[Year>2010&Year<2020,], aes(DoY, GPP_daily))+
     geom_line(colour="forestgreen")+
    # geom_line(aes(yday(date),Reco_daily),colour="burlywood4")+
     geom_hline(yintercept=0)+ylim(c(0,3.5))+
     scale_x_continuous(breaks =c(31,211,361),limits=c(1,367),
                                               labels=c("Jan","Jul","D"),
                                               expand=c(0,0))+
     labs(y=expression("Daily cumulative GPP (gC" *m^-2*")"),
                   x="Month")+
     facet_grid(.~Year)+
     theme_bw(base_size=26)+
     theme(strip.background = element_blank())
 # Plot cumulative sum with 7 day running mean
   preco <- ggplot(daily_sum[Year>2010&Year<2020,], aes(DoY, Reco_daily))+
     geom_line(colour="burlywood4")+
    # geom_line(aes(yday(date),Reco_daily),colour="burlywood4")+
     geom_hline(yintercept=0)+ylim(c(0,3.5))+
   scale_x_continuous(breaks =c(31,211,361),limits=c(1,367),
                                              labels=c("Jan","Jul","D"),
                                               expand=c(0,0))+
     labs(y=expression("Daily Reco (gC" *m^-2*")"),
                   x="Month")+
     facet_grid(.~Year)+
     theme_bw(base_size=26)+
     theme(strip.background = element_blank())
 p.reco.gpp <- grid.arrange(pgpp, preco)
# ggsave(p.reco.gpp, file="~/Desktop/TweedieLab/AGU/2020/Reco_GPP_daily.png",width=19,height=15)

 # Plot daily ET sum with 7 day running mean
pET <- ggplot(daily_sum[Year>2010&Year<2020,], aes(DoY, ET_daily))+
   geom_line(colour="royalblue2")+
   geom_line(aes(DoY,ET_daily_roll),colour="navyblue")+
   geom_hline(yintercept=0)+
   scale_x_continuous(breaks =c(31,211,361),limits=c(1,367),
                      labels=c("Jan","Jul","D"),
                      expand=c(0,0))+
   labs(title="Daily ET and weekly running mean from 2011-2019",
        y=expression("Daily total ET (mm)"),
        x="Month")+
   facet_grid(.~Year)+
   theme_bw(base_size=26)+
   theme(strip.background = element_blank())

# ggsave(pET, file="~/Desktop/TweedieLab/AGU/2020/ET_daily.png",width=19,height=15)

 
# Seasonal C Flux profile {data-navmenu="C Flux Data"}
# Seasonal pattern of daily cumulative NEE across years 2011-2019
## Column
daily_sum[,month:=month(date)]

p4 <- ggplot(daily_sum, aes(factor(DoY), NEE_daily, colour=factor(month)))+
  geom_boxplot()+
  geom_hline(yintercept=0)+
  scale_color_viridis_d(option="plasma")+
  scale_x_discrete(breaks =c("31","61","91","121","151","181","211","241","271","301","331","361"),
                   labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
  labs(title = "Aggregated seasonal NEE from 2011-2019",
      y=expression("Daily cumulative NEE (gC" *m^-2*")"),
       x="Month")+
  theme_bw(base_size=26)+
  theme(legend.position="none")

#png(file="~/Desktop/TweedieLab/AGU/2020/NEEseasonal.png",
  #  width=1920,height=1080)
p4
dev.off()

# seasonal Reco and GPP patterns
p.seas.reco <- ggplot(daily_sum, aes(factor(DoY), Reco_daily))+
  geom_boxplot(colour="burlywood4")+
  geom_hline(yintercept=0)+
  ylim(c(0,3.5))+
  scale_color_viridis_d(option="plasma")+
  scale_x_discrete(breaks =c("31","61","91","121","151","181","211","241","271","301","331","361"),
                   labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
  labs(title = "Aggregated seasonal Reco from 2011-2019",
       y=expression("Daily cumulative Reco (gC" *m^-2*")"),
       x="Month")+
  theme_bw(base_size=26)+
  theme(legend.position="none")
# ggsave(p.seas.reco, file="~/Desktop/TweedieLab/AGU/2020/REco_Seasonal.png",width=19,height=15)

# GPP
p.seas.gpp <- ggplot(daily_sum, aes(factor(DoY), GPP_daily))+
  geom_boxplot(colour="forestgreen")+
  geom_hline(yintercept=0)+
  ylim(c(0,3.5))+
  scale_color_viridis_d(option="plasma")+
  scale_x_discrete(breaks =c("31","61","91","121","151","181","211","241","271","301","331","361"),
                   labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
  labs(title = "Aggregated seasonal GPP from 2011-2019",
       y=expression("Daily cumulative GPP (gC" *m^-2*")"),
       x="Month")+
  theme_bw(base_size=26)+
  theme(legend.position="none")


# ggsave(p.seas.gpp, file="~/Desktop/TweedieLab/AGU/2020/GPP_Seasonal.png",width=19,height=15)


# Annual C Budget {data-navmenu="C Flux Data"}
# Annual C Budget from 2011 to 2019

## Column {.tabset}

### As Daily Cumulative 
# plot the annual C flux budgets as a monthly cumulative
# calculate cumulative sums
daily_cum_sum <- daily_sum[,':='(NEE_cum = cumsum(NEE_daily),
                                 GPP_cum = cumsum(GPP_daily),
                                 Reco_cum = cumsum(Reco_daily),
                                 ET_cum = cumsum(ET_daily)),
                           by="Year"]

daily_cum_sum[,year_lab := ifelse(yday(date)==365, Year, NA)]


p5 <- ggplot(daily_cum_sum, aes(DoY,NEE_cum,colour=factor(Year)))+
  geom_line()+
  geom_point()+
  geom_label(aes(label=year_lab), size=10, label.size=2)+
  labs(y=expression("Annual cumulative NEE (gC" *m^-2*")"))+
  scale_colour_viridis_d()+
  theme_bw(base_size=26)+
  theme(legend.position="none")

#png(file="~/Desktop/TweedieLab/AGU/2020/NEEcummulative.png",
 #   width=800,height=540)
p5
dev.off()

# dailly cumsum of ET
p6 <- ggplot(daily_cum_sum, aes(DoY,ET_cum,colour=factor(Year)))+
  geom_line()+
  geom_point()+
  geom_label(aes(label=year_lab), size=10, label.size=2)+
  labs(y=expression("Annual cumulative ET (mm" *m^-2*")"),x="Month")+
  scale_colour_viridis_d()+
  scale_x_continuous(breaks =c(31,61,91,121,151,181,211,241,271,301,331,361),limits=c(1,385),
                     labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
                     expand=c(0,0))+
  theme_bw(base_size=26)+
  theme(legend.position="none")

#png(file="~/Desktop/TweedieLab/AGU/2020/ETcummulative.png",
 #  width=800,height=540)
p6
dev.off()

# cumulative sum of Reco and GPP
p7 <- ggplot(daily_cum_sum, aes(DoY,GPP_cum,colour=factor(Year)))+
  geom_line()+
  geom_point()+
  geom_label(aes(label=year_lab), size=10, label.size=2)+
  labs(y=expression("Annual cumulative GPP (gC" *m^-2*")"))+
  scale_colour_viridis_d()+
  theme_bw(base_size=26)+
  theme(legend.position="none")

p7

p8 <- ggplot(daily_cum_sum, aes(DoY,Reco_cum,colour=factor(Year)))+
  geom_line()+
  geom_point()+
  geom_label(aes(label=year_lab), size=10, label.size=2)+
  labs(y=expression("Annual cumulative Reco (gC" *m^-2*")"))+
  scale_colour_viridis_d()+
  theme_bw(base_size=26)+
  theme(legend.position="none")

p8

# Calculate monthly sums
monthly_sum <- daily_sum[,list(NEE_annual = sum(NEE_daily),
                              GPP_annual = sum(GPP_daily),
                              Reco_annual = sum(Reco_daily),
                              ET_annual = sum(ET_daily)),
                        by="month,Year"]


# NEE
ggplot(monthly_sum[Year<2020,], aes(factor(month),NEE_annual,fill=factor(Year)))+
  geom_col()+
  labs(y=expression("Monthly cumulative NEE (gC" *m^-2*")"),x="Year")+
  scale_fill_viridis_d()+
  theme_bw()+
  theme(legend.position="none")+
  facet_grid(.~Year)


# GPP
ggplot(monthly_sum[Year<2020,], aes(factor(month),GPP_annual,fill=factor(Year)))+
  geom_col()+
  labs(y=expression("Monthly cumulative GPP (gC" *m^-2*")"),x="Year")+
  scale_fill_viridis_d()+
  theme_bw()+
  theme(legend.position="none")+
  facet_grid(.~Year)

# Reco
ggplot(monthly_sum[Year<2020,], aes(factor(month),Reco_annual,fill=factor(Year)))+
  geom_col()+
  labs(y=expression("Monthly cumulative Reco (gC" *m^-2*")"),x="Year")+
  scale_fill_viridis_d()+
  theme_bw()+
  theme(legend.position="none")+
  facet_grid(.~Year)


### As Annual Sum
# calculate annual budget
annual_sum <- daily_sum[,list(NEE_annual = sum(NEE_daily),
                              GPP_annual = sum(GPP_daily),
                              Reco_annual = sum(Reco_daily),
                              ET_annual = sum(ET_daily)),
                        by="Year"]

# plot the annual budgets
# NEE
ggplot(annual_sum, aes(factor(Year),NEE_annual,fill=factor(Year)))+
  geom_col()+
  labs(y=expression("Annual cumulative NEE (gC" *m^-2*")"),x="Year")+
  scale_fill_viridis_d()+
  theme_bw()+
  theme(legend.position="none")

# ET
ggplot(annual_sum, aes(factor(Year),ET_annual,fill=factor(Year)))+
  geom_col()+
  labs(y="Annual cumulative ET (mm)",x="Year")+
  scale_fill_viridis_d()+
  theme_bw()+
  theme(legend.position="none")

# GPP
ggplot(annual_sum, aes(factor(Year),GPP_annual,fill=factor(Year)))+
  geom_col()+
  labs(y=expression("Annual cumulative GPP (gC" *m^-2*")"),x="Year")+
  scale_fill_viridis_d()+
  theme_bw()+
  theme(legend.position="none")

# Reco
ggplot(annual_sum, aes(factor(Year),Reco_annual,fill=factor(Year)))+
  geom_col()+
  labs(y=expression("Annual cumulative Reco (gC" *m^-2*")"),x="Year")+
  scale_fill_viridis_d()+
  theme_bw()+
  theme(legend.position="none")

# save daily mean (max and min) air temperature for Mariana

ggplot(daily_sum, aes(DoY,Tair_mean,colour=factor(Year)))+geom_line()


# save monthly precipitation and temperature for Mariana
#write.csv(daily_sum[,.(date,Year,DoY,Tair_mean,Tair_max,Tair_min)],
 #         "~/Desktop/OneDrive - University of Texas at El Paso/SEL_Phenology/MetData_Temp_Precip/JER_daily_Temp_2010_2019.csv",
 #         row.names=FALSE)

# merge daily temperature, precip, NEE, GPP, Reco for Will to compare with Phenocam
# precip_daily
# daily_cum_sum

# merge by date
daily_all <- merge(daily_cum_sum, precip_daily, by=c("Year","date"))

ggplot(daily_all)+
  geom_line(aes(date,NEE_daily))+
  geom_line(aes(date,precip.tot), colour="blue")

# Save daily flux, temperature, precip data 2010-2019 for Will
# write.csv(daily_all,
#          "~/Desktop/OneDrive - University of Texas at El Paso/CZ_Drylands/Jornada_REU/2021/JER_daily_Temp_Precip_Cflux_2010_2019.csv",
#          row.names=FALSE)
# 

# 2022-06-27: Look at gap-filled NEE with rainfall (gap-filling done without interruption for rain events)

# combine rainfall with gap-filled fluxes
flux.ep.rain <- flux_filter %>% 
  select(Year, DoY, Hour, P_RAIN_1_1_1) %>% 
  filter (Year>2010 & Year < 2019) %>%
  right_join(flux.ep, by=c("Year","DoY","Hour"))

flags <- flux.ep.rain  %>% 
  filter(!is.na(P_RAIN_1_1_1)) %>%
 # filter((Year==2010 & P_RAIN_1_1_1<39) | Year >=2011)%>%
  filter(Year >=2011)%>%
  # Set a rain flag if there is rain registered on the gauge
  mutate(rainflag = ifelse(P_RAIN_1_1_1 > 0, 1, 0)) %>% 
  # Create a column that contains the number of consecutive times there was rain or not.
  # Use `rle`` which indicates how many times consecutive values happen, and `rep`` to repeat it for each row.
  ##mutate(rainlength = rep(rle(rainflag)$lengths, rle(rainflag)$lengths)) %>% 
  # MM modify: sequence counts number of days with no rain fore ach individual day rather than the total days of a rain event for all rows
  # https://predictivehacks.com/count-the-consecutive-events-in-r/
  mutate(rainlength = sequence(rle(rainflag)$lengths)) %>%  
  # Set a flag for an event happening, when there is rain there is a rain event, 
  # when it is 0 but not for six consecutive times, it is still a rain event
  mutate(
    eventflag = ifelse(
      rainflag == 1, 
      1, 
      ifelse(
        rainflag == 0 & rainlength < 12, 
        1, 
        0
      )
    )
  ) %>% 
  # Correct for the case when the dataset starts with no rain for less than six consecutive times
  # If within the first six rows there is no rain registered, then the event flag should change to 0
  mutate(eventflag = ifelse(row_number() < 12 & rainflag == 0, 0, eventflag)) %>% 
  # assign event ids by year
  group_by(Year)%>%
  # Add an id to each event (rain or not), to group by on the pivot table
  mutate(eventid = rep(seq(1,length(rle(eventflag)$lengths)), rle(eventflag)$lengths))


# create a date_time variable for flags
# alternative to year, DoY, hour: 
flags <- flags %>% mutate(datetime = case_when(
  Year == 2011 ~ ymd_hm(paste(as.Date(DoY-1, origin = "2011-01-01"),
                                        trunc(Hour), (Hour-trunc(Hour))*60), tz = "MST"),
  Year == 2012 ~ ymd_hm(paste(as.Date(DoY-1, origin = "2012-01-01"),
                              trunc(Hour), (Hour-trunc(Hour))*60), tz = "MST"),
  Year == 2013 ~ ymd_hm(paste(as.Date(DoY-1, origin = "2013-01-01"),
                                trunc(Hour), (Hour-trunc(Hour))*60), tz = "MST"),
  Year == 2014 ~ ymd_hm(paste(as.Date(DoY-1, origin = "2014-01-01"),
                              trunc(Hour), (Hour-trunc(Hour))*60), tz = "MST"),
  Year == 2015 ~ ymd_hm(paste(as.Date(DoY-1, origin = "2015-01-01"),
                              trunc(Hour), (Hour-trunc(Hour))*60), tz = "MST"),
  Year == 2016 ~ ymd_hm(paste(as.Date(DoY-1, origin = "2016-01-01"),
                              trunc(Hour), (Hour-trunc(Hour))*60), tz = "MST"),
  Year == 2017 ~  ymd_hm(paste(as.Date(DoY-1, origin = "2017-01-01"),
                                trunc(Hour), (Hour-trunc(Hour))*60), tz = "MST"),
  Year == 2018 ~ ymd_hm(paste(as.Date(DoY-1, origin = "2018-01-01"),
                              trunc(Hour), (Hour-trunc(Hour))*60), tz = "MST"),
  Year == 2019 ~ ymd_hm(paste(as.Date(DoY-1, origin = "2019-01-01"),
                              trunc(Hour), (Hour-trunc(Hour))*60), tz = "MST")))


# plot rain and NEE, aligned with rain events
plot_grid(flags %>% 
            ggplot(., aes(yday(datetime),P_RAIN_1_1_1,color=factor(eventid)))+
            geom_line()+
            facet_grid(Year~.)+
            labs(x="Day of Year",y="Half-Hourly Rain (mm)")+
            theme_bw(base_size=14)+
            theme(legend.position='none'),
          flags %>% 
            ggplot(., aes(datetime,NEE_U50_f,color=factor(eventid)))+
            geom_line()+
            facet_grid(Year~.)+
            theme(legend.position='none'), 
          nrow=2,ncol=1)


# calculate cumulative rain per event
# find start day of each event and days between event start days
rainevents.rain <- flags %>%
  # select only rain events
  filter(eventflag==1)%>%
  # Group by id
  group_by(Year,eventid) %>% 
  summarize(
    eventRain = sum(P_RAIN_1_1_1),
    eventStartDate = first(datetime),
    eventEndDate = last(datetime)
  )%>%
  mutate(duration.hours = as.numeric(eventEndDate - eventStartDate,units="hours"),
         btwn.events = eventStartDate - lag(eventEndDate),
         btwn.events.hours = as.numeric(btwn.events, units="hours"),
         btwn.events.days = as.numeric(btwn.events, units="days"))


# graph cumulative rain
rainevents.rain %>% 
  filter(eventRain>1) %>%
  ggplot(., aes(eventStartDate, eventRain))+
  geom_col()+
  facet_grid(Year~.)

# graph a histogram of rain event sizes
ggplot(rainevents.rain, aes(eventRain))+
  geom_histogram()+
  geom_vline(xintercept=5)+
  labs(x="Rain Event Size (mm)")+
  theme_bw()

# plot events <5mm
rainevents.rain %>%
  filter(eventRain<=5)%>%
  ggplot(., aes(eventRain))+
  geom_histogram()+
  labs(x="Small Rain Event Size (mm)")+
  theme_bw()

# plot events <5mm
rainevents.rain %>%
  filter(eventRain>5)%>%
  ggplot(., aes(eventRain))+
  geom_histogram()+
  labs(x="Large Rain Event Size (mm)")+
  theme_bw()

# histogram for 2014 and 2015
rainevents.rain %>%
  filter(eventRain>5 & (Year == 2014|Year==2015))%>%
  ggplot(., aes(eventRain))+
  geom_histogram()+
  labs(x="Rain Event Size (mm)")+
  geom_vline(xintercept=5)+
  xlim(c(0,50))+
  facet_grid(Year~.)+
  theme_bw()

# merge cumualative rain to full data
flags <- flags %>% left_join(rainevents.rain, by=c("Year","eventid"))

# calculate number of rain events (6 hours long and >1mm per year)
rainevents.count <- flags %>%
  filter((eventflag == 1  & eventRain>1)) %>%
  group_by(Year) %>%
  summarise(event.number = n_distinct(eventid))       

# graph for 2014 (high rain year) and 2015 (frequent rain events)
# look at data in 15-day intervals, ReddyProc has 7-day sliding window for partitioning
plot_grid(flags %>% 
            filter((eventflag == 0 | eventRain>1) & (Year==2014 | Year == 2015) ) %>%
            ggplot(., aes(yday(datetime),P_RAIN_1_1_1,color=factor(eventid)))+
            geom_line()+
            geom_vline(colour="darkgrey", xintercept=c(1,15,30,45,60,75,90,105,120,135,150,165,180,195,210,225,240,255,270,285,300,315,330,345,360))+
            facet_grid(Year~.)+
            labs(x="Day of Year",y="Half-Hourly Rainfall (mm)")+
            theme_bw(base_size=14)+
            theme(legend.position='none'),
          flags %>% 
            filter((eventflag == 0 | eventRain>1) & (Year==2014 | Year == 2015) ) %>%
            ggplot(., aes(datetime,NEE_U50_f,color=factor(eventid)))+
            geom_line()+
            geom_vline(xintercept=c(1,15,30,45,60,75,90,105,120,135,150,165,180,195,210,225,240,255,270,285,300,315,330,345,360))+
            facet_grid(Year~.)+
            theme(legend.position='none'), 
          nrow=2,ncol=1)

# graph for 2014 (high rain year) and 2015 (frequent rain events)
# graph with start of rain events 
start.2014 <- rainevents.rain %>%
  filter(Year==2014)

fig1 <- plot_grid(
  rainevents.rain %>% 
    filter((Year==2014 ) & eventRain>1) %>%
    #filter((yday(eventStartDate)>0 & yday(eventStartDate)<169)) %>%
    ggplot(., aes(x=eventStartDate))+
    geom_col(aes(y=eventRain),width=0.1)+
    geom_label(aes(y=eventRain,label=eventRain))+
    #geom_vline(xintercept=start.2014$startDoY)+
    facet_grid(Year~.)+
    theme(legend.position='none')+
    xlim(c(0,169))+
    ylim(c(0,40)),
  flags %>% 
    filter((eventflag == 0 | eventRain>1) & (Year==2014) & (DoY>0 & DoY<199)) %>%
    ggplot(., aes(datetime,NEE_U50_f,color=factor(eventid)))+
    geom_line()+
    #geom_vline(xintercept=start.2014$startDoY)+
    facet_grid(Year~.)+
    theme(legend.position='none')+
    ylim(c(-8,4)), 
  nrow=2,ncol=1)

fig2 <- plot_grid(
  rainevents.rain %>% 
    filter((Year==2014 ) & eventRain>1 & (eventStartDoY>168 & eventStartDoY<290) ) %>%
    ggplot(., aes(x=eventStartDoY+eventStartHour/100))+
    geom_col(aes(y=eventRain),width=0.1)+
    geom_label(aes(y=eventRain,label=eventRain))+
    #geom_vline(xintercept=start.2014$startDoY)+
    facet_grid(Year~.)+
    theme(legend.position='none')+
    xlim(c(168,290))+
    ylim(c(0,40)),
  flags %>% 
    filter((eventflag == 0 | eventRain>1) & (Year==2014) & (DoY>168 & DoY<290)) %>%
    ggplot(., aes(DoY+Hour/100,NEE_U50_f,color=factor(eventid)))+
    geom_line()+
    #geom_vline(xintercept=start.2014$startDoY)+
    facet_grid(Year~.)+
    theme(legend.position='none')+
    ylim(c(-8,4)), 
  nrow=2,ncol=1)

fig3 <- plot_grid(
  rainevents.rain %>% 
    filter((Year==2014 ) & eventRain>1 & (eventStartDoY>290 & eventStartDoY<366) ) %>%
    ggplot(., aes(x=eventStartDoY+eventStartHour/100))+
    geom_col(aes(y=eventRain),width=0.1)+
    geom_label(aes(y=eventRain,label=eventRain))+
    #geom_vline(xintercept=start.2014$startDoY)+
    facet_grid(Year~.)+
    theme(legend.position='none')+
    xlim(c(291,365))+
    ylim(c(0,40)),
  flags %>% 
    filter((eventflag == 0 | eventRain>1) & (Year==2014) & (DoY>290 & DoY<366)) %>%
    ggplot(., aes(DoY+Hour/100,NEE_U50_f,color=factor(eventid)))+
    geom_line()+
    #geom_vline(xintercept=start.2014$startDoY)+
    facet_grid(Year~.)+
    theme(legend.position='none')+
    ylim(c(-8,4)), 
  nrow=2,ncol=1)

plot_grid(fig1,fig2,fig3, ncol=3)

# graph by event ID
plot_grid(
  rainevents.rain %>% 
    filter((Year==2014 ) & eventRain>1 & eventid==2) %>%
    ggplot(., aes(x=eventStartDate))+
    geom_col(aes(y=eventRain),width=0.1)+
    geom_label(aes(y=eventRain,label=eventRain))+
    #geom_vline(xintercept=start.2014$startDoY)+
    facet_grid(Year~eventid,scales="free_x")+
    theme(legend.position='none'),
  flags %>% 
    filter((eventflag == 0 | eventRain>1) & (Year==2014)& eventid==2) %>%
    ggplot(., aes(datetime,NEE_U50_f,color=factor(eventid)))+
    geom_line()+
    #geom_vline(xintercept=start.2014$startDoY)+
    facet_grid(Year~eventid, scales="free_x")+
    theme(legend.position='none'), 
  nrow=2,ncol=1)

# graph NEE around a rain event "2014-07-31 22:30:00"

startdate <- ymd_hms("2014-09-04 13:00:00",tz="MST")
enddate <- ymd_hms("2014-09-05 07:30:00",tz="MST")

startdate.window <- startdate-days(5)
enddate.window <- enddate+days(5)

plot_grid(
  flags %>%
    filter((eventflag == 0 |eventRain > 1) & (datetime > startdate.window & datetime < enddate.window) ) %>%
    ggplot(., aes(x=datetime))+
    geom_line(aes(y=Tair_f))+
    geom_vline(xintercept=c(startdate,enddate))+
    labs(x="Day", y="Temperature")+
    scale_x_datetime(breaks="3 days",date_labels="%D")+
    scale_y_continuous(labels = label_number(accuracy = 1))+
    theme_bw()+
    theme(legend.position='none',
          axis.title.x=element_blank(),
          axis.text.x=element_blank()),
  flags %>%
    filter((eventflag == 0 |eventRain > 1) & (datetime > startdate.window & datetime < enddate.window)) %>%
    ggplot(., aes(x=datetime))+
    geom_col(aes(y=P_RAIN_1_1_1))+
    geom_label(aes(x=enddate, y=20,label="42 mm"))+
    geom_vline(xintercept=c(startdate,enddate))+
    labs(x="Day", y="Precipitation (mm)")+
    scale_x_datetime(breaks="3 days",date_labels="%D")+
    scale_y_continuous(labels = label_number(accuracy = 1))+
    theme_bw()+
    theme(legend.position='none',
          axis.title.x=element_blank(),
          axis.text.x=element_blank()),
  flags %>%
    filter((eventflag == 0 | eventRain>1) & (datetime > startdate.window & datetime < enddate.window )) %>%
    ggplot(., aes(x=datetime)) +
    geom_line(aes(y=NEE_U50_f),colour="blue")+
    geom_line(aes(y=-GPP_U50_f),colour="green")+
    #geom_vline(xintercept=c(startdate,enddate))+
    labs(x="Day", y="NEE and GPP")+
    scale_x_datetime(breaks="3 days",date_labels="%D")+
    scale_y_continuous(labels = label_number(accuracy = 1))+
    theme_bw()+
    theme(legend.position='none',
          axis.title.x=element_blank(),
          axis.text.x=element_blank()),
  flags %>%
    filter((eventflag == 0 | eventRain>1) & (datetime > startdate.window & datetime < enddate.window)) %>%
    ggplot(., aes(x=datetime)) +
    geom_line(aes(y=Reco_U50),colour="brown")+
    #geom_line(aes(y=GPP_U50_f),colour="green")+
    #geom_vline(xintercept=c(startdate,enddate))+
    labs(x="Day", y="Reco")+
    scale_x_datetime(breaks="3 days")+
    scale_y_continuous(labels = label_number(accuracy = 1))+
    theme_bw()+
    theme(legend.position='none'),
  nrow=4,ncol=1)


# graph NEE around a rain event "2014-09-04 13:00:00"
plot_grid(
  flags %>% 
    filter((eventflag == 0 |eventRain > 1) & (Year==2014 ) & (DoY > 240 & DoY < 250) ) %>%
    ggplot(., aes(x=datetime))+
    geom_line(aes(y=Tair_f))+
    geom_vline(xintercept=c(ymd_hms("2014-09-04 13:00:00",tz="MST"),ymd_hms("2014-09-05 07:30:00",tz="MST")))+
    theme_bw()+
    theme(legend.position='none'),
  flags %>% 
    filter((eventflag == 0 |eventRain > 1) & (Year==2014 ) & (DoY > 240 & DoY < 250) ) %>%
    ggplot(., aes(x=datetime))+
    geom_col(aes(y=P_RAIN_1_1_1))+
    geom_label(aes(x=ymd_hms("2014-09-05 07:30:00",tz="MST"), y=25,label="42 mm"))+
    geom_vline(xintercept=c(ymd_hms("2014-09-04 13:00:00",tz="MST"),ymd_hms("2014-09-05 07:30:00",tz="MST")))+
    theme_bw()+
    theme(legend.position='none'),
  flags %>% 
    filter((eventflag == 0 | eventRain>1) & (Year==2014) & (DoY > 240 & DoY < 250) ) %>%
    ggplot(., aes(x=datetime)) + 
    geom_line(aes(y=NEE_U50_f),colour="blue")+
    geom_line(aes(y=-GPP_U50_f),colour="green")+
    geom_vline(xintercept=c(ymd_hms("2014-09-04 13:00:00",tz="MST"),ymd_hms("2014-09-05 07:30:00",tz="MST")))+
    theme_bw()+
    theme(legend.position='none'),
  flags %>% 
    filter((eventflag == 0 | eventRain>1) & (Year==2014) & (DoY > 240 & DoY < 250) ) %>%
    ggplot(., aes(x=datetime)) + 
    geom_line(aes(y=Reco_U50),colour="brown")+
    #geom_line(aes(y=GPP_U50_f),colour="green")+
    geom_vline(xintercept=c(ymd_hms("2014-09-04 13:00:00",tz="MST"),ymd_hms("2014-09-05 07:30:00",tz="MST")))+
    theme_bw()+
    theme(legend.position='none'), 
  nrow=4,ncol=1)



