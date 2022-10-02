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

flux.ep <- rbind(edata2010,flux.ep[Year>2010&Year<2020,],flux.ep2020, fill=TRUE)

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
  geom_line(size=0.4)+
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
ggplot(subset(flux.ep, Year<2022), aes(DoY,(LE/2454000)*1800))+
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

# calculate daily means and 7-day running means from gap-filled data
# Daily cummulative amount of carbon exchange (gC) and 7-day running mean

# calculate daily sums of Co2 flux in umol/m2/sec converted to gC/m2/day
daily_sum_dt <- as.data.table((flux.ep))
daily_sum_ec <- daily_sum_dt[,list(NEE_daily = sum(NEE_U50_f*1800*1*10^-6*12.01),
                                GPP_daily = sum(GPP_U50_f*1800*1*10^-6*12.01),
                                Reco_daily = sum(Reco_U50*1800*1*10^-6*12.01),
                                ET_daily = sum((LE_f/2454000)*1800), # (amount of energy to evaporate a unit weight of water; 2454000 J kg-1).
                                Tair_mean = mean(Tair),
                                Tair_max=max(Tair),
                                Tair_min=min(Tair)), 
                          by="Year,DoY"][,list(DoY,
                                               NEE_daily, 
                                               GPP_daily,
                                               Reco_daily,
                                               ET_daily,
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

# Plot cumulative sum with 7 day running mean
plot.nee.daily <- ggplot(daily_sum[Year>2010,], aes(DoY, NEE_daily))+
  geom_line(colour="#000000", size=1)+
  geom_line(aes(yday(date),NEE_daily_roll),colour="#A0A0A0",size=0.4)+
  geom_hline(yintercept=0)+
  facet_grid(.~Year)+
  scale_x_continuous(breaks =c(31,211,361),limits=c(1,367),
                     labels=c("Jan","Jul","D"),
                     minor_breaks =c(31,61,91,121,151,181,211,241,271,301,331,361),
                     guide="axis_minor",
                     expand=c(0,0))+
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
  geom_hline(yintercept=0)+ylim(c(0,3.5))+
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
  geom_line(aes(DoY,ET_daily_roll),colour="navyblue")+
  geom_hline(yintercept=0)+
  scale_x_continuous(breaks =c(31,211,361),limits=c(1,367),
                     labels=c("Jan","Jul","D"),
                     expand=c(0,0))+
  labs(title="Daily ET and weekly running mean from 2011-2019",
       y=expression("Daily total ET (mm)"),
       x="Month")+
  facet_grid(.~Year)+
  theme_bw(base_size=14)+
  theme(strip.background = element_blank())

# graph NEE with cumulative rain 
# daily rainfall and cumulative rain
ggplot(daily_sum)+
 geom_col(aes(x=yday(date), y=precip.tot), fill="#A0A0A0",colour="#808080")+
  geom_line(aes(x=yday(date),y=precip.cum/5),colour="#000000",size=0.5)+
  facet_grid(.~Year)+
  theme_bw()+
  labs(y="Total daily Rain (mm)",x="Day of Year")+
  scale_y_continuous(sec.axis=sec_axis(~.*5, name="Cummulative Rain (mm)"))


# graph cumulative NEE and cumulative rain
ggplot(test, aes(DoY))+
  geom_line(aes(y=-NEE_cum),colour="green")+
  geom_line(aes(y=precip.cum),colour="blue")+facet_wrap(.~Year)


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
           .SDcols = c('NEE_daily', 'GPP_daily',"Reco_daily","ET_daily")]

# calculate the daily cumulative variance
daily_stats_cumvar <- daily_sum[,unlist(lapply(.SD,
                                               function(x) list(sd=sd(x,na.rm=TRUE))),
                                        recursive=FALSE),
                                        by="DoY",.SDcols=c("NEE_cum","Reco_cum","GPP_cum","ET_cum")]

# graph daily mean and sd
ggplot(daily_stats, aes(DoY,NEE_daily.mean))+
  geom_point()+
  geom_errorbar(aes(max=(NEE_daily.mean+NEE_daily.sd),min=(NEE_daily.mean-NEE_daily.sd)))

# graph the variance
ggplot(daily_stats_cumvar, aes(DoY,NEE_cum.sd))+
  geom_point()

