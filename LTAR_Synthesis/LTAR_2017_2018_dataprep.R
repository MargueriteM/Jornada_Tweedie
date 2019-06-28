######################################
#     Prepare daily met data for     #
#     D.Browning LTAR Phenocam-flux  #
#      synthesis                     #
#     (Daily 2017/2018 data)         #
#       written by: M. Mauritz       #
#                                    #
#     28 June 2019                   #
######################################

# data loaded from Eddy Pro Biomet data prep in: Jornada_EnvironmentalData_Combine.R
# metadata and data templates at: ftp://ltar_phenology:ltphenar401@jornada-ftp.nmsu.edu (v2.2 templates)

library(data.table)
library(ggplot2)

# LTAR site name: jerbajada

# DATE: yyyy-mm-dd
# TMAX_DAILY_DEGC
# TMIN_DAILY_DEGC
# TMEAN_DAILY_DEGC
# PPT_DAILY_MM
# RH_DAILY_MEAN
# RH_DAILY_SD (std of daily values)
# FLAG

# import the biomet data
# air temp: is from tower met data (not ts) only
# RH: is from tower met data (not ts) only
# precip: is averaged from tower and bare areas in sensor network

setwd("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/MetDataFiles_EP")
load("Biomet_EddyPro_2010_2019_20190626.Rdata")

# create 2017-20189 subset for LTAR and calculate daily
ltar <- copy(biomet[year(date_time) %in% c(2017,2018), .(date_time,
                                                         Ta_1_1_1,
                                                         RH_1_1_1,
                                                         P_rain_1_1_1)])
# create day variable for averaging
ltar[,DATE := as.Date(date_time)]

# calculate summary:
ltar_daily <- ltar[,list(TMAX_DAILY_DEGC = max(Ta_1_1_1),
                         TMIN_DAILY_DEGC = min(Ta_1_1_1) ,
                         TMEAN_DAILY_DEGC = mean(Ta_1_1_1, na.rm=TRUE),
                         PPT_DAILY_MM = sum(P_rain_1_1_1, na.rm=TRUE),
                         RH_DAILY_MEAN = mean(RH_1_1_1, na.rm=TRUE),
                         RH_DAILY_SD = sd(RH_1_1_1, na.rm=TRUE),
                         FLAG = "measured"),
                   by="DATE"]

# look at data
ggplot(ltar_daily, aes(x=DATE))+
  geom_line(aes(y=TMAX_DAILY_DEGC),colour="red")+
  geom_line(aes(y=TMIN_DAILY_DEGC),colour="blue")+
  geom_line(aes(y=TMEAN_DAILY_DEGC))

ggplot(ltar_daily, aes(x=DATE))+
  geom_line(aes(y=PPT_DAILY_MM))

ggplot(ltar_daily, aes(x=DATE,y=RH_DAILY_MEAN))+
  geom_point()+
  geom_pointrange(aes(ymin=RH_DAILY_MEAN-RH_DAILY_SD,
                      ymax=RH_DAILY_MEAN+RH_DAILY_SD))+
  geom_line(aes(y=RH_DAILY_MEAN))

# save the data for Dawn (upload to ftp)
setwd("~/Desktop/TweedieLab/Projects/Jornada/LTAR_Synthesis_Browning")
write.table(ltar_daily,"PhenologyDataTemplateV2.2jerbajada.csv",
            sep=",", dec=".", row.names=FALSE)


