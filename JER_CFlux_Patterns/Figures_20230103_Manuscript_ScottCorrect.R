# Create R script to make manuscript figures for Bajada C Flux data
# add Scott et al 2015 corrected data from ReddyProc - gapfill & partition seperately


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
library(tidyr)
library(egg) # for tag_facet function to add a,b,c etc to individual facets in ggplot
library(bigleaf) # for ET conversion

# create a path for saving figures
figpath <- "/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/MauritzLab_Personal/Manuscripts/JRN_Patterns_Controls/Figures"
tablepath <- "/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/MauritzLab_Personal/Manuscripts/JRN_Patterns_Controls/Tables"

######
# custom function from tag_facet to add a,b,c plot labels to individual facets
# without removing the strip text
tag_facet_custom <- function (p, open = "(", close = ")", tag_pool = letters, x = -Inf, 
                              y = Inf, hjust = -0.5, vjust = 1.5, fontface = 2, family = "", 
                              ...) 
{
  gb <- ggplot_build(p)
  lay <- gb$layout$layout
  tags <- cbind(lay, label = paste0(open, tag_pool[lay$PANEL], 
                                    close), x = x, y = y)
  p + geom_text(data = tags, aes_string(x = "x", y = "y", label = "label"), 
                ..., hjust = hjust, vjust = vjust, fontface = fontface, 
                family = family, inherit.aes = FALSE)
}
######

# import 
# Read Reddyproc output data and compile into one file
# gapfill has qc==1 included in flux columns

# fread imports as data table
# list all files in relevant folder
REgapfiles <- list.files(path="~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/ReddyProc/20230115_ScottCorrect/Results_Gapfill",
                         full.names=TRUE, pattern = "output_USJo1_Gapfill") 

# read in column names and units
units.gap <-fread(REgapfiles[1],
                  header=TRUE)[1,]

# read files and bind them into one file. fill=TRUE because of the missing columns in 2011
flux.gap <- do.call("rbind", lapply(REgapfiles, header = FALSE, fread, skip = 2,
                                   na.strings=c("-9999", "NA","-"),
                                   col.names=colnames(units.gap)))

# for some reason na.strings won't recognize the -9999
flux.gap[flux.gap == -9999] <- NA

# quick graph to check import
ggplot(flux.gap, aes(`Date Time`, NEE_U50_f))+geom_line()

# get the 'edata' to add 2010 to the timeseries eventhough 2010 won't gap fill.... 
setwd("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_EddyPro_filtered")

# import data that was filtered by 3SD filter
# with Scott corrected column included 
load("JER_flux_2010_2022_EddyPro_FullOutput_filterSD_20230115.Rdata")

# convert date to POSIXct and get a year, day, hour column
# if this step doesn't work, make sure bit64 library is loaded otherwise the timestamps importa in a non-sensical format
flux_filter_sd[,':='(Year=year(date_time),DoY=yday(date_time),
                                  hours = hour(date_time), mins = minute(date_time))]

# check there's no duplicated data
flux_filter <- (flux_filter_sd[!(duplicated(flux_filter_sd, by=c("date_time")))])

# format data columns for ReddyProc to match & merge
edata <- copy(flux_filter)

# Year	DoY	Hour	NEE	LE	H	Rg	Tair	Tsoil	rH	VPD	Ustar 
edata[mins==0, Hour := hours+0.0]
edata[mins==30, Hour := hours+0.5]

##### USE Scott et al 2015 corrected co2 flux !!! #######
edata[mins==0, Hour := hours+0.0]
edata[mins==30, Hour := hours+0.5]

edata <- edata[,.(Year,
                  DoY,
                  Hour,
                  fc_wpl_adjust,
                  LE,
                  H,
                  Rg_1_1_1,
                  Ta_1_1_1,
                  RH_1_1_1,
                  `u*`,
                  P_rain_1_1_1,
                  qc_co2_flux,
                  qc_LE,
                  qc_H)] # 23 June 2022: include P_RAIN to allow data processing with/without rain event split

setnames(edata,c("fc_wpl_adjust","Rg_1_1_1","Ta_1_1_1","RH_1_1_1","u*"),
         c("NEE","Rg","Tair","rH","Ustar"))

# make all Rg<0 equal to 0 becuase ReddyProc won't accept values <0
edata[Rg<0, Rg:=0]

# When running only 1 year & the complete year, remove max year because that belongs to the following year
#  edata <- edata[Year!=max(edata$Year),]

# create a grid of full dates and times
filled <- expand.grid(date=seq(as.Date("2010-01-01"),as.Date("2022-12-31"), "days"),
                      Hour=seq(0,23.5, by=0.5))
filled$Year <- year(filled$date)
filled$DoY <- yday(filled$date)

filled$date <- NULL

edata <- merge(edata,filled,by=c("Year","DoY","Hour"), all=TRUE)

# graph for quick check
ggplot(edata, aes(DoY,NEE))+
  geom_line()+
  facet_grid(Year~.)

# online tool says hours must be between 0.5 and 24.0 
# therefore add 0.5 to each hour
edata[,Hour := Hour+0.5]

# check that all days have 48 points
daylength <- edata[,list(daylength=length(Hour)),by="Year,DoY"]

ggplot(daylength, aes(DoY, daylength))+geom_point()+facet_wrap(Year~.)

# convert edata to data frame for ReddyProc
edata <- as.data.frame(edata)

# calculate VPD from rH and Tair in hPa (mbar), at > 10 hPa the light response curve parameters change
edata$VPD <- fCalcVPDfromRHandTair(edata$rH, edata$Tair)

# get only 2010 and go back to data table
edata2010 <- as.data.table(subset(edata,Year==2010))
#edata2020 <- as.data.table(subset(edata,Year==2020))
# edata2021 <- as.data.table(subset(edata,Year==2021))

flux.ep <- rbind(edata2010,flux.gap, fill=TRUE)

# save the data to have a compiled file easy to access
# setwd("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/ReddyProc/20230115_ScottCorrect/")
# save(file="REddyResults_2010_2022_Compiled_ScottCorrect_Gap.Rdata",flux.ep)

# plot to check
# NEE_U95_f graph should have 2010 missing
ggplot(flux.ep, aes(DoY, NEE_U50_f))+geom_line()+facet_grid(.~Year)
# NEE graph should have all years present
ggplot(flux.ep, aes(DoY, NEE))+geom_line()+facet_grid(.~Year)


# Don't do this for now. For the re-calculated data I did this in the 
# EddyPro filtering step but went only up to DOY 291
# # exclude 1 April - Dec 4 2013 NEE and LE until processing can be figured out
# # H2O values are suspect & have a few things to try with EddyPro but no definite solution yet
# # see Diagnose_Flux_LE_Issues_2013.Rmd
# flux.ep.complete <- copy(flux.ep)
# flux.ep <- flux.ep.complete[((Year==2013 & DoY >90) & (Year==2013 & DoY<338)),':='(NEE=NA, 
#                                                                            NEE_U50_f=NA,
#                                                                            LE=NA,
#                                                                            LE_f=NA)]
# ggplot(flux.ep[Year==2013,], aes(DoY, NEE))+
#   geom_line()+
#   facet_grid(.~Year)
# 

# calculate the percent of data gap-filled for NEE and ET
gapfill.perc.f.nee <- flux.ep[!is.na(NEE_U50_f),list(NEE_U50_count = length(NEE_U50_f)), 
                        by="Year"]
gapfill.perc.nee <- flux.ep[!is.na(NEE), list(NEE_count = length(NEE)), 
                                   by="Year"]

gapfill.perc.f.et <- flux.ep[!is.na(LE_f),list(LE_f_count = length(LE_f)), 
                          by="Year"]
gapfill.perc.et <- flux.ep[!is.na(LE), list(LE_count = length(LE)), 
                        by="Year"]
                        
gapfill.perc <- merge(gapfill.perc.nee,gapfill.perc.f.nee,all.x=TRUE)
gapfill.perc <- merge(gapfill.perc,gapfill.perc.et,all.x=TRUE)
gapfill.perc <- merge(gapfill.perc,gapfill.perc.f.et,all.x=TRUE)

# create column for total half hours in the entire year
gapfill.perc[,total_count := c(17520,17520,17568,17520,17520,17520,17568,17520,17520,17520,17568,17520,13152)]

gapfill.perc <- gapfill.perc[,':='(NEE_meas_gap_perc = round(NEE_count/NEE_U50_count,2)*100,
                                   LE_meas_gap_perc = round(LE_count/LE_f_count,2)*100,
                                   NEE_unfilled_gap_perc = round(NEE_U50_count/total_count,2)*100,
                                   LE_unfilled_gap_perc = round(LE_f_count/total_count,2)*100)]

# calculate ET using big leaf function
flux.ep <- as.data.table(flux.ep)
flux.ep[,':='(ET.bl= LE.to.ET(LE,Tair))]


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

plot.TA.hh <- ggplot(flux_filter_sd, aes(DoY, Ta_1_1_1))+
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


plot.precip.hh <- ggplot(flux_filter_sd, aes(DoY, P_rain_1_1_1))+
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
                                 #  GPP_daily = sum(GPP_U50_f*1800*1*10^-6*12.01),
                               # Reco_daily = sum(Reco_U50*1800*1*10^-6*12.01),
                                ET_daily = sum((LE_f/2454000)*1800), # (amount of energy to evaporate a unit weight of water; 2454000 J kg-1).
                                ET_daily_mean = mean((LE/2454000),na.rm=TRUE)*86400,
                                Tair_mean = mean(Tair),
                                Tair_max=max(Tair),
                                Tair_min=min(Tair)), 
                          by="Year,DoY"][,list(DoY,
                                               NEE_daily, 
                                               NEE_daily_mean,
                                              # GPP_daily,
                                              # Reco_daily,
                                               ET_daily,
                                               ET_daily_mean,
                                               Tair_mean,
                                               Tair_max,
                                               Tair_min,
                                               NEE_cum = cumsum(NEE_daily),
                                              NEE_cum_mean = (cumsum(ifelse(is.na(NEE_daily_mean), 0, NEE_daily_mean)) + NEE_daily_mean*0),
                                        # GPP_cum = cumsum(GPP_daily),
                                        # Reco_cum = cumsum(Reco_daily),
                                         ET_cum = cumsum(ET_daily),
                                       ET_cum_mean = (cumsum(ifelse(is.na(ET_daily_mean), 0, ET_daily_mean)) + ET_daily_mean*0)),
                                        by="Year"]

# create a running mean 
daily_sum_ec[,':=' (NEE_daily_roll = rollmean(x=NEE_daily,
                                           k=7,
                                           fill=NA),
                 ET_daily_roll = rollmean(x=ET_daily,
                                          k=7,
                                          fill=NA))]

# look at the gap-filled daily sums vs the mean calculation for all years
daily.comp.co2 <- ggplot(daily_sum_ec,aes(x=NEE_daily, y=NEE_daily_mean))+
  geom_point()+
  geom_abline(slope=1,intercept=0)+
  geom_smooth(method="lm")+
  labs(x = "Daily Cummulative NEE (ReddyProc gap-filled)",
       y = "Daily Cummulative NEE (scaled from mean daily flux) ")+
  annotate("text", x=0, y=3, label = expression("y = -0.02 + 1.09x, " *R^2*" = 0.93"))+
  theme_bw()+
  theme(axis.ticks.length =  unit(-0.2,"cm"))+
  lims(x=c(-2,3),y=c(-2,3))

lm.daily.co2 <- lm(daily_sum_ec$NEE_daily_mean~daily_sum_ec$NEE_daily)
summary(lm.daily.co2)

daily.comp.et <- ggplot(daily_sum_ec,aes(x=ET_daily, y=ET_daily_mean))+
  geom_point()+
  geom_abline(slope=1,intercept=0)+
  geom_smooth(method="lm", alpha=0)+
  labs(x = "Daily Cummulative ET (ReddyProc gap-filled)",
       y = "Daily Cummulative ET (scaled from mean daily flux) ")+
  annotate("text", x=3, y=8, label = expression("y = -0.03 + 1.13x, " *R^2*" = 0.92"))+
  theme_bw()+
  theme(axis.ticks.length =  unit(-0.2,"cm"))+
  lims(x=c(-1,8),y=c(-1,8))


lm.daily.et <- lm( daily_sum_ec$ET_daily_mean~daily_sum_ec$ET_daily)
summary(lm.daily.et)

# supplemental figure showing correlation between ReddyProc daily flux
# and daily flux scaled from mean flux value
fig.S1 <- plot_grid(daily.comp.co2, daily.comp.et, ncol=2,
                    labels="auto")

# save
setwd(figpath)
# ggsave("FigureS1_MeanRERegr_ScottCorrect.pdf", fig.S1, device=pdf,path=figpath, dpi=300)

# timeseries
daily.comp.co2.ts <- ggplot(daily_sum_ec)+
  geom_point(aes(x=DoY, y=NEE_daily),colour="darkgrey",size=0.8)+
  geom_point(aes(x=DoY, y=NEE_daily_mean),colour="lightblue",size=0.5)+
  facet_grid(.~Year)+
  labs(y = "Daily Cummulative NEE", x="Day Of Year")+
  theme_bw()+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.length =  unit(-0.2,"cm"),
        strip.background = element_blank())

daily.comp.et.ts <-ggplot(daily_sum_ec)+
  geom_point(aes(x=DoY, y=ET_daily),colour="darkgrey",size=0.8)+
  geom_point(aes(x=DoY, y=ET_daily_mean),colour="lightblue",size=0.5)+
  facet_grid(.~Year)+
  labs(y = "Daily Cummulative NEE",x="Day Of Year")+
  theme_bw()+
  theme(axis.ticks.length =  unit(-0.2,"cm"),
        strip.background = element_blank())

fig.S2 <- plot_grid(daily.comp.co2.ts, daily.comp.et.ts, nrow=2)

# save
setwd(figpath)
# ggsave("FigureS2_MeanRETimeS_ScottCorrect.pdf", fig.S2, device=pdf,path=figpath, dpi=300)


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
precip_daily <- flux_filter[,date:=as.Date(date_time)][!is.na(P_rain_1_1_1),
                                      list(precip.tot = sum(P_rain_1_1_1)),
                                      by="date,Year"][
                                        Year>2010,precip.cum:=cumsum(precip.tot),by="Year"][
                                         Year==2010, precip.cum := (cumsum(ifelse(is.na(precip.tot), 0, precip.tot)) + precip.tot*0)]



# combine daily fluxes with daily precip
daily_sum <- full_join(daily_sum_ec,precip_daily)

# add a year label to day 365 of each year
daily_sum <- daily_sum[,year_lab := ifelse(yday(date)==360, Year, NA)]

# save the daily sum data to share with Habibur
# write.table(daily_sum[Year>2020&!is.na(NEE_daily),.SD,.SDcols=c("Year","DoY","date","NEE_daily","ET_daily","Tair_mean","precip.tot")],
#            "/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/US_Jo1_Share_Habibur/US_Jo1_daily_20210101_20220930",
#            na="NA", row.names=FALSE)

# calculate cumulative sums
annual_cum <- daily_sum[Year>2010,list(NEE_cum.ann = sum(NEE_daily),
                              #GPP_cum = sum(GPP_daily),
                              #Reco_cum = sum(Reco_daily),
                              ET_cum.ann = sum(ET_daily),
                              precip_cum.ann = sum(precip.tot),
                              temp_mean.ann = mean(Tair_mean, na.rm=TRUE),
                              temp_min.ann = min(Tair_min, na.rm=TRUE),
                              temp_max.ann = max(Tair_max, na.rm=TRUE)),
                        by="Year"]
# add annual cumulative based on 2010 mean dailys
annual_cum2010 <- daily_sum[Year==2010,list(NEE_cum.ann = (sum(ifelse(is.na(NEE_daily_mean), 0, NEE_daily_mean))),
                                  ET_cum.ann = (sum(ifelse(is.na(ET_daily_mean), 0, ET_daily_mean)))),
                            by="Year"]

annual_cum <- rbind(annual_cum2010, annual_cum,fill=TRUE)

# caluculate seasonal cumulatives
daily_sum[DoY<166, season:="Pre-Monsoon"]
daily_sum[DoY>=166 & DoY<=273, season:="Monsoon"]
daily_sum[DoY>273, season:="Post-Monsoon"]

daily_sum[,season := factor(season, levels=c("Pre-Monsoon","Monsoon","Post-Monsoon"))]

seasonal_cum <- daily_sum[Year > 2010,list(NEE_cum = sum(NEE_daily),
                                NEE_cum_day = sum(NEE_daily)/length(DoY),
                              #GPP_cum = sum(GPP_daily),
                             # Reco_cum = sum(Reco_daily),
                              ET_cum = sum(ET_daily), 
                             ET_cum_day = sum(ET_daily)/length(DoY),
                              precip_cum = sum(precip.tot),
                             precip_cum_day = sum(precip.tot)/length(DoY),
                              temp_mean = mean(Tair_mean, na.rm=TRUE),
                              temp_min = min(Tair_min, na.rm=TRUE),
                              temp_max = max(Tair_max, na.rm=TRUE)),
                        by="Year,season"]

seasonal_cum2010 <- daily_sum[Year==2010,list(NEE_cum = (sum(ifelse(is.na(NEE_daily_mean), 0, NEE_daily_mean))),
                                              NEE_cum_day = (sum(ifelse(is.na(NEE_daily_mean), 0, NEE_daily_mean)))/length(DoY),
                                            ET_cum = (sum(ifelse(is.na(ET_daily_mean), 0, ET_daily_mean))),
                                            ET_cum_day = (sum(ifelse(is.na(ET_daily_mean), 0, ET_daily_mean)))/length(DoY)),
                            by="Year,season"]

seasonal_cum <- rbind(seasonal_cum2010, seasonal_cum,fill=TRUE)


# graph cumulative NEE per season either total, or as 'daily rate'
ggplot(seasonal_cum, aes(factor(Year),NEE_cum))+geom_col()+facet_grid(.~season)

ggplot(seasonal_cum, aes(factor(Year),NEE_cum_daily))+geom_col()+facet_grid(.~season)


# combine seasonal and annual cumulatives
ann.seas_cum <- full_join(seasonal_cum,annual_cum)

# create a wide version of annual and seasonal cum to be able to compare annual NEE to dynamics in different seasons
ann.seas.wide <- ann.seas_cum %>% 
  pivot_wider(id_cols=Year, names_from = season, values_from = c(NEE_cum.ann, NEE_cum, precip_cum))

# add NEE_cum_Monsoon from ann.seas.wide to seasonal_cum
# this will let me do an analysis of monsoon NEE vs pre-monsoon and monsoon rain
# show_col(viridis(3)) to see the hex code for palettes
seasonal_cum <- merge(seasonal_cum,ann.seas.wide[,c("Year", "NEE_cum_Monsoon")],by="Year")

figS3 <- ggplot(seasonal_cum[season %in% c("Pre-Monsoon","Monsoon")],
       aes(precip_cum, NEE_cum_Monsoon,colour=season))+
  geom_point()+
  geom_smooth(method="lm", alpha=0.1, linetype="dotted", linewidth=0.8)+
  geom_hline(yintercept=0)+
  labs(y=expression("Monsoon cumulative NEE (gC" *m^-2*")"),
       x="Total seasonal rainfall (mm)")+
  scale_colour_manual(name="Season",values=c("#440154FF", "#21908CFF"))+
  annotate("text", x = 150, y=-2, label = expression("" *R^2* "= -0.15"),hjust=0,fontface="italic")+
  annotate("text", x = 150, y=-6, label = "Seasonal rainfall, NS",hjust=0,fontface="italic")+
  annotate("text", x = 150, y=-10, label = "Season, NS",hjust=0,fontface="italic")+
  annotate("text", x = 150, y=-14, label = "Seasonal rainfall * Season, NS",hjust=0,fontface="italic")+
  theme_bw(base_size=14)+
  theme(axis.ticks.length =  unit(-0.2,"cm"),
        ggh4x.axis.ticks.length.minor = rel(0.7),
        legend.position=c(0.87,0.855))

figS3

lm.monsoonNEE.precip <- lm(`NEE_cum_Monsoon`~`precip_cum`*season,data=seasonal_cum[season %in% c("Pre-Monsoon","Monsoon")])

# check residuals
op<- par(mfrow=c(2,2))
plot(lm.monsoonNEE.precip)
par(op)

summary(lm.monsoonNEE.precip)
anova(lm.monsoonNEE.precip)

confint(lm.monsoonNEE.precip)

# save figure
setwd(figpath)
ggsave("FigureS3_SeasonalRain_MonsoonNEE.pdf",figS3,device=pdf,path=figpath, dpi=300)


# plot annnual cumulative
plot.nee.cum <- ggplot(annual_cum, aes(factor(Year), NEE_cum))+
  geom_bar(stat="identity")+
  labs(y="Annual NEE (gC/m2)")+
  theme_bw()

# plot.gpp.cum <- ggplot(annual_cum, aes(factor(Year), GPP_cum))+
#   geom_bar(stat="identity")+
#   labs(y="Annual GPP (gC/m2)")+
#   theme_bw()
# 
# plot.reco.cum <- ggplot(annual_cum, aes(factor(Year), Reco_cum))+
#   geom_bar(stat="identity")+
#   labs(y="Annual Reco (gC/m2)",x="Year")+
#   theme_bw()
# 

plot.et.cum <- ggplot(annual_cum, aes(factor(Year), ET_cum))+
  geom_bar(stat="identity")+
  labs(y="Annual ET (mm/m2)", x="Year")+
  theme_bw()

plot_grid(plot.nee.cum+theme(axis.text.x =element_blank(), axis.title.x=element_blank()),
          #plot.gpp.cum+theme(axis.text.x=element_blank(), axis.title.x=element_blank()),
         # plot.reco.cum+theme(axis.text.x=element_blank(), axis.title.x=element_blank()),
          plot.et.cum, nrow=2)


# plot annual cumulatives with seasonal cumulatives
ggplot(ann.seas_cum, aes(NEE_cum_day, NEE_cum.ann, colour=season))+
  geom_point()+
  geom_smooth(method="lm",alpha=0.2)

ggplot(ann.seas_cum, aes(precip_cum, NEE_cum.ann, colour=season))+
  geom_point()+
  geom_smooth(method="lm",alpha=0.2)

# plot seasonal NEE by annual NEE
fig7a <- ggplot(ann.seas_cum, aes(NEE_cum_day, NEE_cum.ann, colour=season))+
  geom_point()+
  geom_smooth(method="lm", alpha=0.1, linetype="dashed",linewidth=0.5)+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)+
  ylim(c(-100,55))+
  annotate("text", x=0.28, y=52, label= "Net Source: \n Annual & Seasonal",fontface="bold")+
  annotate("text", x=-0.15, y=-90, label= "Net Sink: \n Annual & Seasonal",fontface="bold")+
  annotate("text", x = 0.1, y=-82, label = expression("" *R^2* "= 0.72"),hjust=0,fontface="italic")+
  annotate("text", x = 0.1, y=-88, label = "Seasonal NEE, p<0.001",hjust=0,fontface="italic")+
  annotate("text", x = 0.1, y=-94, label = "Season, p<0.001",hjust=0,fontface="italic")+
  labs(y=expression("Annual NEE (gC" *m^-2*")"),
       x=expression("Seasonal NEE (gC" *m^-2*" da"*y^-1*")"))+
  scale_colour_viridis_d(name="Season")+
  theme_bw(base_size=14)+
  theme(axis.ticks.length =  unit(-0.2,"cm"),
                   ggh4x.axis.ticks.length.minor = rel(0.7),
        legend.position=c(0.15,0.855))

fig7a

# analyse the relationship between annual NEE and seasonal NEE cum, normalised by day
lm.NEE.ann.seas <- lm(NEE_cum.ann~NEE_cum_day*season, data=ann.seas_cum)

# check residuals
op1<- par(mfrow=c(2,2))
plot(lm.NEE.ann.seas)
par(op1)

lm.NEE.ann.seas1 <- lm(NEE_cum.ann~NEE_cum_day+season, data=ann.seas_cum)
lm.NEE.ann.seas2 <- lm(NEE_cum.ann~NEE_cum_day, data=ann.seas_cum)

AICc(lm.NEE.ann.seas,lm.NEE.ann.seas1)
AICc(lm.NEE.ann.seas1,lm.NEE.ann.seas2)

summary(lm.NEE.ann.seas1)

anova(lm.NEE.ann.seas1)

# seasonal cumulative ET and precipitation
fig8 <- ggplot(ann.seas_cum, aes(precip_cum, ET_cum, colour=season))+
  geom_point()+
  geom_smooth(method="lm", alpha=0.1, linetype="dashed",linewidth=0.5)+
 ylim(c(0,200))+
  xlim(c(0,200))+
  annotate("text", x = 145, y=20, label = expression("" *R^2* "= 0.58"),hjust=0,fontface="italic")+
  annotate("text", x = 145, y=12, label = "Seasonal rainfall, p<0.001",hjust=0,fontface="italic")+
  annotate("text", x = 145, y=5, label = "Season, p<0.01",hjust=0,fontface="italic")+
 # annotate("text", x = 0.1, y=-100, label = "Seasonal NEE * Season, NS",hjust=0,fontface="italic")+
  labs(y="Seasonal ET (mm)",
       x="Seasonal rainfall (mm)")+
  scale_colour_viridis_d(name="Season")+
  theme_bw(base_size=14)+
  theme(axis.ticks.length =  unit(-0.2,"cm"),
        ggh4x.axis.ticks.length.minor = rel(0.7),
        legend.position=c(0.15,0.855))


fig8

# check regression between ET and rainfall 
lm.precip.seas <- lm(ET_cum~precip_cum*season, data=ann.seas_cum)
# check residuals
op2<- par(mfrow=c(2,2))
plot(lm.precip.seas)
par(op2)

lm.precip.seas1 <- lm(ET_cum~precip_cum+season, data=ann.seas_cum)
lm.precip.seas2 <- lm(ET_cum~precip_cum, data=ann.seas_cum)

AICc(lm.precip.seas,lm.precip.seas1)
AICc(lm.precip.seas1,lm.precip.seas2)

anova(lm.precip.seas1)

summary(lm.precip.seas1)

# save
setwd(figpath)
ggsave("Figure8_SeasonalPrecipET.pdf",fig8,device=pdf,path=figpath, dpi=300)

# Plot cumulative sum with 7 day running mean and 2010 data based on mean daily multiplied to day
plot.nee.daily.cum <- ggplot(daily_sum, aes(DoY, NEE_daily))+
  geom_line(colour="#000000", linewidth=0.8)+
  #geom_line(aes(yday(date),NEE_daily_roll),colour="#A0A0A0",size=0.4)+
  geom_line(data=daily_sum[Year==2010,],aes(yday(date),NEE_daily_mean),colour="#A0A0A0",linewidth=0.4)+
  geom_line(data=daily_sum[Year!=2017 & Year!=2021,],aes(x=yday(date),y=NEE_cum/100),colour="forestgreen",linewidth=0.5)+
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

# without cumulative
plot.nee.daily <- ggplot(daily_sum, aes(DoY, NEE_daily))+
  geom_line(colour="#000000", linewidth=0.8)+
  #geom_line(aes(yday(date),NEE_daily_roll),colour="#A0A0A0",size=0.4)+
  geom_line(data=daily_sum[Year==2010,],aes(yday(date),NEE_daily_mean),colour="#000000",linewidth=0.4)+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=c(166,273))+
  facet_grid(.~Year)+
  scale_x_continuous(breaks =c(30,180,300),
                     minor_breaks =seq(30,360,30),
                     guide="axis_minor",
                     expand=c(0,0))+
  scale_y_continuous(name=expression("Daily NEE (gC" *m^-2*")"))+
  facet_grid(.~Year)+
  theme_bw(base_size=14)+
  theme(strip.background = element_blank(),
        # panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.spacing.x=unit(0.1, "lines"),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.length =  unit(-0.2,"cm"),
        ggh4x.axis.ticks.length.minor = rel(0.7))

# # graph GPP and Reco
# plot.gpp.daily <- ggplot(daily_sum[Year>2010,], aes(DoY, GPP_daily))+
#   geom_line(colour="#000000")+
#   geom_hline(yintercept=0)+
#   ylim(c(0,3.5))+
#  labs(y=expression("Daily GPP (gC" *m^-2*")"),
#        x="Month")+
#   facet_grid(.~Year)+
#   scale_x_continuous(breaks =c(31,211,361),limits=c(1,367),
#                      labels=c("Jan","Jul","D"),
#                      minor_breaks =c(31,61,91,121,151,181,211,241,271,301,331,361),
#                      guide="axis_minor",
#                      expand=c(0,0))+
#   facet_grid(.~Year)+
#   theme_bw(base_size=14)+
#   theme(strip.background = element_blank(),
#         #strip.text = element_blank(),
#         panel.grid.minor.x = element_blank(),
#         axis.text.x = element_blank(),
#         axis.title.x = element_blank(),
#         axis.ticks.length =  unit(-0.2,"cm"),
#         ggh4x.axis.ticks.length.minor = rel(0.7))
# 
# # Plot cumulative sum with 7 day running mean
# plot.reco.daily <- ggplot(daily_sum[Year>2010,], aes(DoY, Reco_daily))+
#   geom_line(colour="#000000")+
#   geom_hline(yintercept=0)+ylim(c(0,3.5))+
#   labs(y=expression("Daily Reco (gC" *m^-2*")"),
#        x="Month")+
#   facet_grid(.~Year)+
#   scale_x_continuous(breaks =c(31,211,361),limits=c(1,367),
#                      labels=c("Jan","Jul","D"),
#                      minor_breaks =c(31,61,91,121,151,181,211,241,271,301,331,361),
#                      guide="axis_minor",
#                      expand=c(0,0))+
#   theme_bw(base_size=14)+
#   theme(strip.background = element_blank(),
#         strip.text = element_blank(),
#         panel.grid.minor.x = element_blank(),
#         axis.ticks.length =  unit(-0.2,"cm"),
#         ggh4x.axis.ticks.length.minor = rel(0.7),
#         axis.text.x = element_text(vjust=-0.7))

# Plot daily ET sum with 7 day running mean
pET.cum <- ggplot(daily_sum[Year>2010,], aes(DoY, ET_daily))+
  geom_line(colour="royalblue2")+
  #geom_line(aes(DoY,ET_daily_roll),colour="navyblue")+
  geom_line(data=daily_sum[Year==2010,],aes(yday(date),ET_daily_mean),colour="navyblue",linewidth=0.4)+
  geom_line(aes(x=yday(date),y=ET_cum/50),colour="#000000",linewidth=0.5)+
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

# without cumulative
pET <- ggplot(daily_sum[Year>2010,], aes(DoY, ET_daily))+
  geom_line(colour="royalblue2")+
  #geom_line(aes(DoY,ET_daily_roll),colour="navyblue")+
  geom_line(data=daily_sum[Year==2010,],aes(yday(date),ET_daily_mean),colour="royalblue2",linewidth=0.4)+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=c(166,273))+
  scale_x_continuous(breaks =c(30,180,300),
                     minor_breaks =seq(30,360,30),
                     guide="axis_minor",
                     expand=c(0,0))+
  labs(#title="Daily ET and weekly running mean from 2011-2019",
    y=expression("Daily ET (mm)"),
    x="Day of Year")+
  facet_grid(.~Year)+
  theme_bw(base_size=14)+
  theme(strip.background = element_blank(),
        # panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.spacing.x=unit(0.1, "lines"),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.length =  unit(-0.2,"cm"),
        ggh4x.axis.ticks.length.minor = rel(0.7))

# daily rainfall and cumulative rain
plot.precip.daily.cum <- ggplot(daily_sum)+
 geom_col(aes(x=yday(date), y=precip.tot), fill="#A0A0A0",colour="#808080")+
  geom_line(aes(x=yday(date),y=precip.cum/5),colour="#000000",linewidth=0.5)+
  scale_x_continuous(breaks =c(31,211,361),limits=c(1,367),
                     labels=c("Jan","Jul","D"),
                     minor_breaks =c(31,61,91,121,151,181,211,241,271,301,331,361),
                     guide="axis_minor",
                     expand=c(0,0))+
  facet_grid(.~Year)+
  scale_y_continuous(sec.axis=sec_axis(~.*5, name="Cummulative"))+
  labs(y="Daily Rain (mm)",x="Day of Year")+
  theme_bw(base_size=14)+
  theme(strip.background = element_blank(),
        # panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.length =  unit(-0.2,"cm"),
        ggh4x.axis.ticks.length.minor = rel(0.7))

# without cumulative
plot.precip.daily <- ggplot(daily_sum)+
  geom_col(aes(x=yday(date), y=precip.tot), fill="#A0A0A0",colour="#808080")+
  geom_vline(xintercept=c(166,273))+
  scale_x_continuous(breaks =c(30,180,300),
                     minor_breaks =seq(30,360,30),
                     guide="axis_minor",
                     expand=c(0,0))+
  facet_grid(.~Year)+
  labs(y="Daily Rain (mm)",x="Day of Year")+
  theme_bw(base_size=14)+
  theme(strip.background = element_blank(),
        # panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.spacing.x=unit(0.1, "lines"),
        axis.ticks.length =  unit(-0.2,"cm"),
        ggh4x.axis.ticks.length.minor = rel(0.7))

# graph time-series of daily NEE, ET, rain
fig1 <- plot_grid(plot.nee.daily+theme(axis.title.x=element_blank(), axis.text.x = element_blank(),plot.margin = unit(c(0, 0, 0, 0), "cm")),
          pET+theme(axis.title.x=element_blank(), axis.text.x = element_blank(), strip.text.x=element_blank(),plot.margin = unit(c(0.1, 0, 0, 0), "cm")),
          plot.precip.daily+theme(axis.text.x = element_text(vjust = -0.5),strip.text.x=element_blank(),plot.margin = unit(c(t = 0.1, r = 0, b = 0, l = 0), "cm")),
          labels=c("a","b","c"),
          nrow=3,
          align="v")


# graph daily Reco and GPP
fig2 <- plot_grid(plot.gpp.daily,
          plot.reco.daily, nrow=2,
          labels=c("a","b"),
          align="v")

# save figures
setwd(figpath)
ggsave("Figure1_ScottCorrect.pdf",fig1,device=pdf,path=figpath, dpi=300)

ggsave("Figure2.pdf",fig2,device=pdf,path=figpath, dpi=300)




# graph cumulatives for multiple years in one panel
plot.nee.an.cum <- ggplot(daily_sum[Year!=2010,], aes(DoY, NEE_cum,color=factor(Year)))+
  geom_line()+
  geom_label(aes(label=year_lab), size=3, label.size=1, show.legend=FALSE)+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=c(166,273))+
  labs(x="Day of Year",title= "  Cumulative NEE")+
  scale_y_continuous(position="right",name=expression("gC" *m^-2*""))+
  scale_color_viridis_d()+
  theme_bw(base_size = 14)+
  xlim(c(0,370))

plot.et.an.cum <- ggplot(daily_sum[Year!=2010,], aes(DoY, ET_cum,color=factor(Year)))+
  geom_line()+
  geom_label(aes(label=year_lab), size=3, label.size=1, show.legend=FALSE)+ 
  geom_vline(xintercept=c(166,273))+
  labs(x="Day of Year",title="  Cumulative ET")+
  scale_y_continuous(position="right",name="mm")+
  scale_color_viridis_d()+
  theme_bw(base_size = 14)+
  xlim(c(0,370))

plot.precip.an.cum <- ggplot(daily_sum[Year!=2010,], aes(DoY, precip.cum,color=factor(Year)))+
  geom_line()+
  geom_label(aes(label=year_lab), size=3, label.size=1, show.legend=FALSE)+
  geom_vline(xintercept=c(166,273))+
  labs(x="Day of Year",title="  Cumulative Rainfall")+
 scale_y_continuous(position="right",name="mm")+
  scale_color_viridis_d(name="Year")+
  theme_bw(base_size = 14)+
  xlim(c(0,370))

plot.leg <- get_legend(plot.precip.an.cum)

# fig4 <- plot_grid(plot_grid(plot.nee.an.cum+theme(legend.position = "none"),
#                     plot.et.an.cum+theme(legend.position = "none"),
#                            plot.precip.an.cum+theme(legend.position = "none"),
#           ncol=3),
#           plot.leg,
#           rel_widths = c(1,0.1))

# without legend
fig4 <- plot_grid(plot.nee.an.cum+theme(legend.position = "none"),
                            plot.et.an.cum+theme(legend.position = "none"),
                            plot.precip.an.cum+theme(legend.position = "none"),
                            ncol=3,
                  labels = c("a","b","c"))

# save figure
setwd(figpath)
ggsave("Figure4_ScottCorrect.pdf",fig4,device=pdf,path=figpath, dpi=300, height=6, width=10, units="in")


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
           by="DoY",.SDcols = c('NEE_daily',"ET_daily",
                                "NEE_cum","ET_cum")]
           # .SDcols = c('NEE_daily', 'GPP_daily',"Reco_daily","ET_daily",
           #             "NEE_cum","Reco_cum","GPP_cum","ET_cum")]


# graph daily mean and sd
fig3 <- ggplot(daily_stats, aes(DoY,NEE_daily.mean))+
  geom_point(size=0.5)+
  geom_line(size=0.2)+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=c(166,273))+
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

# save figure
setwd(figpath)
ggsave("Figure3_ScottCorrect.pdf",fig3,device=pdf,path=figpath, dpi=300, height=6, width=10, units="in")

  
# graph the variance of daily cumulative sum
fig5 <- ggplot(daily_stats[DoY!=366,], aes(DoY,((NEE_cum.var))))+
  geom_point(size=0.5)+
  geom_line(size=0.2)+
  geom_vline(xintercept=c(166,273))+
  labs(y = expression("Variance NEE (gC" *m^-2*")"^2), x="Day of Year")+
  theme_bw(base_size=14)+
  theme(axis.ticks.length =  unit(-0.2,"cm"),
        ggh4x.axis.ticks.length.minor = rel(0.7),
        axis.text.x = element_text(vjust = -0.5))

# save figure
setwd(figpath)
ggsave("Figure5_ScottCorrect.pdf",fig5,device=pdf,path=figpath, dpi=300, height=6, width=10, units="in")


# graph daily NEE and rain with cumulative NEE
fig6 <- ggplot(daily_sum[Year>2010,], aes(DoY))+
  geom_col(aes(y=NEE_daily),fill="green")+
  geom_line(aes(y=NEE_cum/20),colour="dark green",linewidth=0.4)+
  # geom_line(aes(y=precip.cum/3),colour="blue")+
  geom_col(aes(y=precip.tot/20),colour="blue")+
  # geom_line(aes(y=ET_cum/3),colour="black")+
  # geom_line(data=daily_sum[Year==2010,],aes(DoY,ET_cum_mean),colour="black",linewidth=0.4)+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=c(166,273),colour="dark grey")+
  scale_x_continuous(name="Day of Year")+
  scale_y_continuous(name=expression("Daily NEE (gC" *m^-2*")"),
                     sec.axis=sec_axis(~.*20,
                           name=expression("Daily rainfall (mm) and Cumulative NEE (gC"*m^-2*")")))+
  facet_wrap(.~Year)+
  theme_bw(base_size=14)+
  theme(strip.background = element_blank(),
        axis.ticks.length =  unit(-0.2,"cm"),
        ggh4x.axis.ticks.length.minor = rel(0.7),
        axis.text.x = element_text(vjust = -0.5))

fig6.tag <- tag_facet_custom(fig6,x = -Inf, y = -Inf, 
                     vjust = -1,
                     open = "", close = "")



# save figure
setwd(figpath)
ggsave("Figure6_ScottCorrect.pdf",fig6.tag,device=pdf,path=figpath, dpi=300, height=6, width=10, units="in")


# graph cumulative annual precip with annual NEE
lm.precip.nee <- lm(NEE_cum.ann~precip_cum.ann, data=annual_cum)
# check residuals
op3<- par(mfrow=c(2,2))
plot(lm.precip.nee)
par(op3)

# try with autocorrelation structure

lm.precip.nee.ar1 <- lme(NEE_cum.ann~precip_cum.ann, 
                        random=~1|Year,
                        data=annual_cum[!is.na(precip_cum.ann)&!is.na(NEE_cum.ann),])
lm.precip.nee.ar1 <- update(lm.precip.nee.ar1, correlation = corAR1())
# check residuals
op4<- par(mfrow=c(2,2))
plot(lm.precip.nee.ar1)
par(op4)

# compare: AR1 correlation structure produces worse fit
AICc(lm.precip.nee,lm.precip.nee.ar1)

summary(lm.precip.nee)
anova(lm.precip.nee)

fig7b <- ggplot(annual_cum, aes(precip_cum.ann, NEE_cum.ann))+
  geom_point()+
  # geom_abline(intercept=lm.precip.nee$coefficients[1],slope=lm.precip.nee$coefficients[2])+
  geom_hline(yintercept=0)+
  geom_smooth(method="lm",se=TRUE,colour="dark blue", linewidth=0.8, alpha=0.1)+
  annotate("text", x=150, y=-55, label = expression("" *R^2* "= 0.37"),hjust=0)+
  annotate("text", x=150, y=-62, label = "p = 0.1",hjust=0)+
  ylim(c(-100,55))+
  labs(y = expression("Annual NEE (gC" *m^-2*")"),
       x = "Annual Rainfall (mm)")+
  theme_bw(base_size=14)+
  theme(axis.ticks.length =  unit(-0.2,"cm"),
        ggh4x.axis.ticks.length.minor = rel(0.7),
        axis.text.x = element_text(vjust = -0.5))

fig7b

## predict x-intercept and confint, but I don't know about this output.... 
# library(chemCal)
# inverse.predict(lm.precip.nee,0)

fig7 <- plot_grid(fig7a,
                  fig7b + theme(axis.title.y = element_blank()),
                  ncol=2, align = "v", labels=c("a","b"))
fig7
# save figure
setwd(figpath)
ggsave("Figure7_ScottCorrect.pdf",fig7,device=pdf,path=figpath, dpi=300, height=6, width=10, units="in")


# Tables
# annual NEE, ET, Precip
setwd(tablepath)
write.table(annual_cum, "Cumulative_annual_ScottCorrect.csv", sep=",", dec=".", row.names=FALSE)

# pre-monsoon, monsoon, post-monsoon NEE, ET, Precip
write.table(seasonal_cum, "Cumulative_seasonal_ScottCorrect.csv", sep=",", dec=".", row.names=FALSE)

# pre-monsoon, monsoon, post-monsoon NEE, Precip & annual NEE in wide format
write.table(ann.seas.wide, "Cumulative_seasonal_WIDE_ScottCorrect.csv", sep=",", dec=".", row.names=FALSE)

# percent of measured/gapfilled values
write.table(gapfill.perc, "Gapfill_Measured_ScottCorrect.csv", sep=",", dec=".", row.names=FALSE)

# air temperature daily mean, min, max, number of days below 0, sequence of days below 0

