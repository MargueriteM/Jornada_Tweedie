# Ameriflux data submitted and returned from self-check QA/QC process
# Code to check flagged variables and make decisions


# load libraries
library(data.table)
library(lubridate)
library(ggplot2)
library(bit64)
library(cowplot)


# create and set WD
dir.ameriflux <- "/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Bahada/Tower/Ameriflux_USJo1"
setwd(dir.ameriflux)

# 19 Sep 2023 submitted following files and changed name to _PRELIM to submit
#US-Jo1_HH_202001010000_202101010000submit
#US-Jo1_HH_202101010000_202201010000_submit
#US-Jo1_HH_202201010000_202210010000_submit

flux.22 <- fread("US-Jo1_HH_202201010000_202210010000_submit.csv",sep=",", dec=".",
                 header = TRUE, na.strings=c("na","NA","","-9999"))

flux.22 <- flux.22[,date_time := parse_date_time(TIMESTAMP_END,"YmdHM",tz="UTC")]

# remove duplicate data
flux.22 <- (flux.22[!(duplicated(flux.22, by=c("TIMESTAMP_START")))])

# variables to check from Ameriflux QA/QC
# 2022: PPFD_IN_1_1_1, CO2, SW_IN_1_1_1, NETRAD_1_1_1, SWC probes.

# Check PPFD_IN_1_1_1
# Decision: APPROVE. some values high but are in line with pattern & only very few during peak summer
ggplot(flux.22, aes(date_time, PPFD_IN_1_1_1))+
  geom_point(size=0.5)+
  geom_hline(yintercept=c(2400,2500))


# Check CO2
# Decision: APPROVE. I typically don't filter CO2... and these spikes identified in QA/QC match rain events
# and capture a reasonable dynamic for a pulse-driven dryland system
p.co2 <- ggplot(flux.22, aes(date_time, CO2))+
  geom_point(size=0.5)

p.rain <- ggplot(flux.22, aes(date_time, P_RAIN_1_1_1))+
  geom_point(size=0.5)

plot_grid(p.co2,p.rain, nrows=2, align="v")

# put CO2 and rain on the same graph: 
p.co2+geom_col(aes(x=date_time, y=P_RAIN_1_1_1*100), colour="blue")

# graph diurnal CO2 by months
ggplot(flux.22, aes(hour(date_time), CO2))+
  geom_point(size=0.5)+
  facet_wrap(month(date_time)~.)

# diunral rain
ggplot(flux.22, aes(hour(date_time), P_RAIN_1_1_1))+
  geom_point(size=0.5)+
  facet_wrap(month(date_time)~.)

# graph CO2 when fluxes are removed or not removed
ggplot(flux.22[is.na(FC),], aes(date_time, CO2))+
      geom_point(size=0.5)

# not removed
ggplot(flux.22[!is.na(FC),], aes(date_time, CO2))+
  geom_point(size=0.5)


# Check SW_IN_1_1_1
# Decision: APPROVE. Generally looks OK. Few values are high & reaching outside expected range but these are sporadic and fit in pattern
ggplot(flux.22, aes(x=date_time))+
  geom_line(aes(y=SW_IN_1_1_1, colour="SW_IN_1_1_1"))

# look at dirunal
ggplot(flux.22, aes(x=hour(date_time)))+
  geom_point(aes(y=SW_IN_1_1_1, colour="SW_IN_1_1_1"))+
  facet_wrap(month(date_time)~.)

# Check NETRAD_1_1_1
# Decision: APPROVE. Dynamics make sense in time-series and with SW_IN. Few values are out of range in 
# diunral check but not cause for concern. Not systemtic anc fit in pattern. 
ggplot(flux.22, aes(x=date_time))+
  geom_line(aes(y=NETRAD_1_1_1, colour="NETRAD_1_1_1"))+
  geom_col(aes( y=P_RAIN_1_1_1*100), colour="blue")

# look at dirunal with SW IN
ggplot(flux.22, aes(x=hour(date_time)))+
  geom_point(aes(y=NETRAD_1_1_1, colour="NETRAD_1_1_1"))+
  geom_line(aes( y=SW_IN_1_1_1), colour="blue")+
  facet_wrap(yday(date_time)~.)


# Check SWC
# Decision: Request to remove all soil moisture data and submit as seperate file?
# as complete update?

