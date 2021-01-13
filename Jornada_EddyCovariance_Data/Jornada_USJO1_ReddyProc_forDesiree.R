# Make figures with SD filterred data and webtool ReddyProc gap-filled, partitioned data
# code to append data

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

# IMPORT FILES FROM TEAMS HERE: 
#eg: USJo1_partitioned_201001010000_201012312330_20200427.csv  

# make sure that there are no -9999 values and all -9999 is recognized as NA!! 


# import filtered flux data file from Eddy Pro as data table
# update with timestamp corrected data
# filtered in: Jornada_EddyPro_Output_Fluxnext_2010_2019.R
ep.units <- (fread("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/ReddyProc/20200427/REddyResults_Us-Jo1_20200428_586625386/output.txt",
                   header=TRUE))[1,]

flux.ep <- fread("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/ReddyProc/20200427/REddyResults_Us-Jo1_20200428_586625386/output.txt",
                      header=FALSE, skip=2,na.strings=c("-9999", "NA","-"),
                 col.names = colnames(ep.units))

# for some reason na.strings won't recognize the -9999
flux.ep[flux.ep == -9999] <- NA

# get the 'edata' to add 2010 to the timeseries eventhough 2010 won't gap fill.... 
setwd("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_EddyPro_filtered")

# import data that was filtered by 3SD filter
load("JER_flux_2010_2019_EddyPro_Output_filtered_SD_TIMEcorr_20200427.Rdata")

# convert date to POSIXct and get a year, day, hour column
# if this step doesn't work, make sure bit64 library is loaded otherwise the timestamps importa in a non-sensical format
flux_filter_sd[,':=' (date_time = parse_date_time(TIMESTAMP_END_correct,"YmdHM",tz="UTC"),
                      date_time_start = parse_date_time(TIMESTAMP_START_correct,"YmdHM",tz="UTC"))][
                        ,':='(Year=year(date_time),DoY=yday(date_time),
                              hours = hour(date_time), mins = minute(date_time))]

# there's duplicated data in 2012 DOY 138
flux_filter <- (flux_filter_sd[!(duplicated(flux_filter_sd, by=c("TIMESTAMP_END_correct")))])

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
edata[Rg<0, Rg:=0]

# create a grid of full dates and times
filled <- expand.grid(date=seq(as.Date("2010-01-01"),as.Date("2019-12-31"), "days"),
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

flux.ep <- rbind(edata2010,flux.ep, fill=TRUE)

# substract 0.5 from the hour again to match TIMESTAMP_END
flux.ep[,Hour := Hour-0.5]

# plot with no U* filter or gapfill
ggplot(flux.ep[Year==2019,], aes(DoY,NEE_orig))+
  geom_line()+
  facet_grid(Year~.)

fig_nee <- ggplot(subset(flux.ep), aes(DoY,NEE_orig))+
  geom_line()+
  geom_point(aes(y=NEE_U50_f),data=subset(flux.ep, is.na(NEE_orig)),colour="red",size=0.25)+
  facet_grid(.~Year)#+
ylim(c(-10,10))

fig_nee_fill <- ggplot((flux.ep))+
  geom_line(aes(DoY,NEE_U50_f))+
  geom_line(aes(DoY,NEE_orig), data = subset(flux.ep,Year==2010))+
  
  facet_grid(.~Year)+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())#+
ylim(c(-10,10))

fig_reco <- ggplot(subset(flux.ep), aes(DoY,Reco_U50))+geom_line()+
  geom_line(aes(y=Reco_DT_U50),colour="blue")+
  facet_grid(.~Year)+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())#+
ylim(c(-10,10))

# plot daytime Reco with qc code
ggplot(subset(flux.ep), aes(DoY,Reco_DT_U50, colour=factor(FP_qc)))+
  geom_point(size=1)+
  facet_grid(.~Year)

fig_gpp <- ggplot(subset(flux.ep), aes(DoY,GPP_U50_f))+geom_line()+
  geom_line(aes(y=GPP_DT_U50),colour="blue")+
  facet_grid(.~Year)+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())#+
ylim(c(-10,10))

fig_vpd <- ggplot(subset(flux.ep), aes(DoY,VPD))+geom_line()+
  geom_hline(yintercept=10, colour="red")+
  facet_grid(.~Year)

grid.arrange(fig_nee, fig_reco, fig_gpp, fig_vpd, nrow=4)

grid.arrange(fig_reco, fig_gpp, fig_nee, nrow=3)

grid.arrange(fig_nee_fill, fig_reco, fig_gpp, nrow=3)

grid.arrange(fig_nee, fig_gpp, fig_vpd, nrow=3)
grid.arrange(fig_nee, fig_reco, fig_vpd, nrow=3)

# plot measured vs modeled NEE at U50 (NEE_orig is unfiltered, I can't get unfilled U* filtered from ReddyProc online tool)
ggplot(subset(flux.ep), aes(NEE_orig, NEE_U50_f))+geom_point()+
  geom_abline(intercept=0, slope=1)+
  facet_grid(.~Year)

# plot NEE with different U* quantiles
ggplot(subset(flux.ep), aes(x=DoY))+
  geom_line(aes(y=NEE_U05_f, colour="NEE_U05_f"))+
  geom_line(aes(y=NEE_U50_f, colour="NEE_U50_f"))+
  geom_line(aes(y=NEE_U95_f, colour="NEE_U95_f"))+
  facet_grid(Year~.)


# heat map of gap-filled NEE data
ggplot(flux.ep,
       aes(DoY,Hour,fill=NEE_U50_f))+
  geom_tile(color= "white",size=0.1) + 
  scale_fill_viridis(name=expression(paste('C',O[2],' flux',sep='')))+
  facet_grid(Year~.)+
  scale_y_continuous(breaks=c(0,12,23),
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


# calculate daily sums of Co2 flux in umol/m2/sec converted to gC/m2/day

daily_sum_dt <- as.data.table(subset(flux.ep))
daily_sum <- daily_sum_dt[,list(NEE_daily = sum(NEE_U50_f*1800*1*10^-6*12.01),
                                GPP_daily = sum(GPP_U50_f*1800*1*10^-6*12.01),
                                Reco_daily = sum(Reco_U50*1800*1*10^-6*12.01),
                                Tair_mean = mean(Tair)), 
                          by="Year,DoY"]

# create a running mean 
daily_sum[,NEE_daily_roll := rollmean(x=NEE_daily,
                                      k=7,
                                      fill=NA)]

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

# plot to see what it looks like
fig_runmean <- ggplot()+
  #geom_line(data=daily_sum, aes(yday(date), NEE_daily),colour="red")+
  geom_line(data=daily_sum, aes(yday(date),NEE_daily_roll))+
  geom_hline(yintercept=0)+
  facet_grid(.~Year)

grid.arrange(fig_runmean+geom_vline(xintercept=200, colour="pink",size=1),
             fig_daily_rain+geom_vline(xintercept=200,colour="pink",size=1), nrow=2)


# calculate cumulative sums
daily_cum_sum <- daily_sum[,list(NEE_cum = cumsum(NEE_daily),
                                 GPP_cum = cumsum(GPP_daily),
                                 Reco_cum = cumsum(Reco_daily)),
                           by="Year"]
# plot the cumulative daily sums 

# calculate annual budget
annual_sum <- daily_sum[,list(NEE_annual = sum(NEE_daily)),
                        by="Year"]

# plot the annual budgets
ggplot(annual_sum, aes(factor(Year),NEE_annual))+
  geom_bar(stat="identity")+
  labs(y="Annual cumulative NEE gC/m2")+
  theme_bw()

# save daily and annual sums
#setwd("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/ReddyProc/")
#write.table(daily_sum, "JER_ReddyProc_daily_sum_CO2_2011_2018.csv", sep=",", dec=".", row.names=FALSE)
#write.table(annual_sum, "JER_ReddyProc_annual_sum_CO2_2011_2018.csv", sep=",", dec=".", row.names=FALSE)


fig_daily_NEE <- ggplot(daily_sum, aes(DoY, NEE_daily))+
  geom_line(colour="green")+
  geom_line(aes(yday(date),NEE_daily_roll))+
  geom_hline(yintercept=0)+
  labs(y="Daily cumulative NEE gC/m2")+
  facet_grid(.~Year)+
  theme_bw()

