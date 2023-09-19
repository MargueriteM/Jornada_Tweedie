
# Read and check data prepped for Ameriflux submission

# load libraries
library(data.table)
library(lubridate)
library(ggplot2)
library(bit64)

# set WD to One Drive
# setwd("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Bahada/Tower/Ameriflux_USJo1")
setwd("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Tower Data/JER_Bajada/EddyCovarianceTower/Ameriflux")

# read data year by year and combine
flux.20 <- fread("US-Jo1_HH_202001010000_202101010000submit.csv",sep=",", dec=".",
                 header = TRUE, na.strings=c("na","NA","","-9999"))

flux.21 <- fread("US-Jo1_HH_202012312330_202111302330submit.csv",sep=",", dec=".",
                 header = TRUE, na.strings=c("na","NA","","-9999"))

flux.22 <- fread("US-Jo1_HH_202201010000_202210010000submit.csv",sep=",", dec=".",
                 header = TRUE, na.strings=c("na","NA","","-9999"))

flux <- rbind(flux.20, flux.21, flux.22, fill=TRUE)

flux <- flux[,date_time := parse_date_time(TIMESTAMP_END,"YmdHM",tz="UTC")]

# remove duplicate data
flux <- (flux[!(duplicated(flux, by=c("TIMESTAMP_START")))])

# graph data
# graph CO2 fluxes with a ylim
ggplot(flux, aes(date_time,FC,colour=(factor(FC_SSITC_TEST))))+
  geom_point(size=0.2)+
  ylim(c(-20,20))


## H2O fluxes with ylim, AGC<60, and SSITC<2
ggplot(flux, aes(date_time,LE,colour=(factor(LE_SSITC_TEST))))+
  geom_point(size=0.2)+
  ylim(c(-500,1000))

## AGC vs LE flux
ggplot(flux, aes(FC_SSITC_TEST, FC))+
  geom_point(size=0.2)

## AGC<60
ggplot(flux[CUSTOM_AGC_MEAN<60,], aes(date_time,CUSTOM_AGC_MEAN))+
  geom_point(size=0.2)+
  ylim(c(-500,1000))

## graph biomet vars
ggplot(flux, aes(date_time,P_RAIN_1_1_1))+
  geom_point(size=0.2)


ggplot(flux, aes(date_time,G_1_1_1))+
  geom_point(size=0.2)


