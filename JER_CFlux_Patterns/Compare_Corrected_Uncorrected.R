
# compare Scott corrected with uncorrected

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

# figure path for annual corrected vs uncorrected
figpath <- "/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/MauritzLab_Personal/Manuscripts/JRN_Patterns_Controls/Figures"


# load Scott corrected
setwd("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/ReddyProc/20230115_ScottCorrect/")
load("REddyResults_2010_2022_Compiled_ScottCorrect_Gap.Rdata")

# rename and remove flux.ep
flux.ep.sc <- flux.ep
rm(flux.ep)

# load non-corrected data
setwd("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/ReddyProc/20221209_2010_2019/")
load("REddyResults_2010_2022_Compiled.Rdata")

# load non gap-filled data
setwd("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_EddyPro_filtered")

# import data that was filtered by 3SD filter
# with Scott corrected column included 
load("JER_flux_2010_2022_EddyPro_FullOutput_filterSD_20230115.Rdata")

# graph through time
p1 <- ggplot(flux.ep, aes(DoY, NEE_U50_f))+geom_line()+facet_grid(.~Year)+labs(Title="No Spectral Correction")

p2 <- ggplot(flux.ep.sc, aes(DoY, NEE_U50_f))+geom_line()+facet_grid(.~Year)+labs(Title="With Spectral Correction")

grid.arrange(p1,p2)

# graph into same plot
ggplot()+
  geom_line(data=flux.ep, aes(DoY, NEE_U50_f))+
  geom_line(data=flux.ep.sc, aes(DoY, NEE_U50_f),colour="lightgreen")



# calculate sums for days

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

# with scott correct
daily_sum_dt_sc <- as.data.table((flux.ep.sc))
daily_sum_ec_sc <- daily_sum_dt_sc[,list(NEE_daily_sc = sum(NEE_U50_f*1800*1*10^-6*12.01),
                                   NEE_daily_mean_sc = mean(NEE, na.rm=TRUE)*86400*1*10^-6*12.01), # scale the mean to daily), 
                             by="Year,DoY"][,list(DoY,
                                                  NEE_daily_sc, 
                                                  NEE_daily_mean_sc,
                                                  NEE_cum_sc = cumsum(NEE_daily_sc),
                                                  NEE_cum_mean_sc = (cumsum(ifelse(is.na(NEE_daily_mean_sc), 0, NEE_daily_mean_sc)) + NEE_daily_mean_sc*0)),
                                            by="Year"]

# merge daily
daily_sum <- inner_join(daily_sum_ec,daily_sum_ec_sc,by=c("Year","DoY"))

# add the daily interpolated data for 2010 to remaining years
daily_sum[Year==2010, ':=' (NEE_daily = NEE_daily_mean, 
                            NEE_daily_sc = NEE_daily_mean_sc)]

# add a date variable to daily_sum_ec
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
daily_sum[Year==2020,date:= as.Date(DoY-1, origin = "2020-01-01")]
daily_sum[Year==2021,date:= as.Date(DoY-1, origin = "2021-01-01")]
daily_sum[Year==2022,date:= as.Date(DoY-1, origin = "2022-01-01")]


# graph
ggplot(daily_sum[Year==2021,])+
  geom_line(aes(DoY, NEE_daily))+
  geom_line(aes(DoY,NEE_daily_sc),colour="lightgreen")+
  facet_grid(.~Year)+
  theme_bw()

# graph as regression
ggplot(daily_sum, aes(NEE_daily,NEE_daily_sc))+
  geom_point()+
  geom_abline(intercept=0,slope=1)+
  facet_grid(.~Year)+
  labs(x="uncorrected NEE",y="Spectral corrected NEE")+
  theme_bw()

# graph monthly pattern
p_seasonal <- ggplot(daily_sum)+
  geom_boxplot(aes(as.factor(DoY), NEE_daily))+
  geom_boxplot(aes(as.factor(DoY),NEE_daily_sc),fill="lightgreen", colour="lightgreen", alpha=0.5)+
  facet_grid(.~month(date), scales="free_x")+
  labs(y = expression("Daily NEE (gC" *m^-2*")"), x="Day of Year and Month")+
  theme_bw(base_size=14)+
  theme(strip.background = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.length =  unit(-0.2,"cm"),
        ggh4x.axis.ticks.length.minor = rel(0.7))

# save figures
setwd(figpath)
# ggsave("FigureS6_Seasonal_Corr_UnCorr.pdf",p_seasonal,device=pdf, dpi=300)


# graph daily by year
flux.comp.daily <- ggplot(daily_sum)+
  geom_line(aes(DoY, NEE_daily), linewidth=0.6)+
  geom_line(aes(DoY,NEE_daily_sc), linewidth=0.8, colour="green", alpha=0.5)+
  facet_grid(year(date)~.)+
  labs(y = expression("Daily NEE (gC" *m^-2*")"), x="Day of Year")+
  theme_bw(base_size=14)+
  theme(strip.background = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.length =  unit(-0.2,"cm"),
        ggh4x.axis.ticks.length.minor = rel(0.7))

# save figures
setwd(figpath)
# ggsave("Compare_Flux_daily_Corr_UnCorr.pdf",flux.comp.daily,device=pdf, dpi=300)


# calculate hourly mean by month and year
daily_sum_dt <- daily_sum_dt[,month:= month(`Date Time`)]
hourly_ec <- daily_sum_dt[,list(NEE_hourly = mean(NEE_U50_f*1800*1*10^-6*12.01),
                                NEE_hourly_sd = sd(NEE_U50_f*1800*1*10^-6*12.01)), 
                             by="Year,month,Hour"]



daily_sum_dt_sc <- daily_sum_dt_sc[,month:= month(`Date Time`)]
hourly_ec_sc <- daily_sum_dt_sc[,list(NEE_hourly_sc = mean(NEE_U50_f*1800*1*10^-6*12.01),
                                NEE_hourly_sd_sc = sd(NEE_U50_f*1800*1*10^-6*12.01)), 
                          by="Year,month,Hour"]


# merge
hourly_all <- inner_join(hourly_ec, hourly_ec_sc, by=c("Year","month","Hour"))

# graph
ggplot(hourly_all[!is.na(month),])+
  geom_point(aes(factor(Hour), NEE_hourly), size=0.5)+
  geom_point(aes(factor(Hour), NEE_hourly_sc), colour="lightgreen",size=0.5)+
  facet_grid(Year~month)

# calculate annual C balance
# calculate cumulative sums
annual_cum <- daily_sum[Year>2010,list(NEE_cum.ann = sum(NEE_daily),
                                       NEE_cum.ann.sc = sum(NEE_daily_sc),
                                       #GPP_cum = sum(GPP_daily),
                                       #Reco_cum = sum(Reco_daily),
                                       ET_cum.ann = sum(ET_daily),
                                       temp_mean.ann = mean(Tair_mean, na.rm=TRUE),
                                       temp_min.ann = min(Tair_min, na.rm=TRUE),
                                       temp_max.ann = max(Tair_max, na.rm=TRUE)),
                        by="Year"]
# add annual cumulative based on 2010 mean dailys
annual_cum2010 <- daily_sum[Year==2010,list(NEE_cum.ann = (sum(ifelse(is.na(NEE_daily_mean), 0, NEE_daily_mean))),
                                            NEE_cum.ann.sc = (sum(ifelse(is.na(NEE_daily_mean_sc), 0, NEE_daily_mean_sc))),
                                            ET_cum.ann = (sum(ifelse(is.na(ET_daily_mean), 0, ET_daily_mean)))),
                            by="Year"]

annual_cum <- rbind(annual_cum2010, annual_cum,fill=TRUE)

# graph as relationship
ggplot(annual_cum)+
  geom_point(aes(NEE_cum.ann, NEE_cum.ann.sc))+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)+
  annotate("text", x = -200, y=-50, label = "Net C Sink",hjust=0,fontface="italic")+
  annotate("text", x = -200, y=30, label = "Net C Source",hjust=0,fontface="italic")+
  labs(title = "Annual cumulative flux", x="uncorrected NEE",y="spectral corrected NEE")+
  theme_bw()

# graph as bars
p_annual_bar <- ggplot(annual_cum[!(Year %in% c(2013,2017,2021,2022))])+
  geom_col(aes(x=factor(Year-0.1),y=NEE_cum.ann, width=0.7))+
  geom_col(aes(x=factor(Year+0.1),y=NEE_cum.ann.sc), fill="lightgreen",width=0.7)+
  geom_hline(yintercept = 0)+
  labs(y=expression("Annual NEE (gC" *m^-2*")"))+
  facet_grid(.~Year, scales="free_x")+
  theme_bw(base_size=14)+
  theme(strip.background = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.length =  unit(-0.2,"cm"),
        ggh4x.axis.ticks.length.minor = rel(0.7))

p_annual_bar

# save figures
setwd(figpath)
#ggsave("FigureS5_Annual_Corr_UnCorr.pdf",p_annual_bar,device=pdf, dpi=300)

# make graphs from the 3SD filtered but not gap-filled data
colnames(flux_filter_sd)
# co2_flux (uncorrected)
# fc_wpl_adjust (corrected)

# graph correlation between flux and corrected
flux.comp.30min <- ggplot(flux_filter_sd, aes(co2_flux, fc_wpl_adjust))+
  geom_point()+
  geom_abline(intercept=0,slope=1)+
  labs(y="Corrected (0.9) offset 30min CO2 flux",x="Uncorrected 30 min CO2 flux")

# save figures
setwd(figpath)
# ggsave("Compare_Flux_30min_noFill_Corr_UnCorr.pdf",flux.comp.30min,device=pdf, dpi=300)

# compare diurnal data by year and month
# add an hour and minute to flux_filter_sd and average by hour
flux_filter_sd_hour <- flux_filter_sd[,':='(year=year(date_time),
                                            month=month(date_time),
                                            doy=yday(date_time),
                                            hour=hour(date_time),
                                       min = min(date_time))][,list(
                                         co2_flux = mean(co2_flux,na.rm=TRUE),
                                         fc_wpl_adjust = mean(fc_wpl_adjust,na.rm=TRUE)),
                                         by="year,month,doy,hour"
                                       ]

ggplot(flux_filter_sd_hour, aes(x=factor(hour)))+
  #geom_point(aes(y=co2_flux), colour="black", size=0.2)+
 # geom_smooth(aes(y=co2_flux), colour="black", alpha=0, linewidth=0.3)+
  geom_point(aes(y=fc_wpl_adjust), colour="green", size=0.2)+
  #geom_smooth(aes(y=fc_wpl_adjust), colour="green", alpha=0, linewidth=0.3)+
  facet_grid(year~month)+
  theme_bw()
  
# hourly correlation between both
ggplot(flux_filter_sd_hour,aes(x=co2_flux, y=fc_wpl_adjust), size=0.05)+
  geom_point()+
  geom_abline(intercept=0,slope=1)+
  labs(y="Corrected (0.9) offset 30min CO2 flux",x="Uncorrected 30 min CO2 flux")+
  facet_grid(year~month, scales="free")+
  theme_bw()

