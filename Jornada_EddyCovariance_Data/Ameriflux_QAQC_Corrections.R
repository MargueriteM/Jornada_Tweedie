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

flux.20 <- fread("US-Jo1_HH_202001010000_202101010000submit.csv",sep=",", dec=".",
                 header = TRUE, na.strings=c("na","NA","","-9999"))

flux.20 <- flux.20[,date_time := parse_date_time(TIMESTAMP_END,"YmdHM",tz="UTC")]

# remove duplicate data
flux.20 <- (flux.20[!(duplicated(flux.20, by=c("TIMESTAMP_START")))])

# variables to check from Ameriflux QA/QC
# 2020: PPFD_IN_1_1_1, CO2, G_1_1_1, G_1_2_1, LE, SWC_2_3_1

# Check PPFD_IN_1_1_1
# Decision: APPROVE. some values high but are in line with pattern & only very few during peak summer
ggplot(flux.20, aes(date_time, PPFD_IN_1_1_1))+
  geom_point(size=0.5)+
  geom_hline(yintercept=c(2400,2500))


# Check CO2
# Decision: APPROVE. I typically don't filter CO2... and these spikes identified in QA/QC match rain events
# and capture a reasonable dynamic for a pulse-driven dryland system
p.co2 <- ggplot(flux.20, aes(date_time, CO2))+
  geom_point(size=0.5)

p.rain <- ggplot(flux.20, aes(hour(date_time), P_RAIN_1_1_1))+
  geom_point(size=0.5)

plot_grid(p.co2,p.rain, nrows=2, align="v")

# put CO2 and rain on the same graph: 
p.co2+geom_col(aes(x=date_time, y=P_RAIN_1_1_1*100), colour="blue")

# graph diurnal CO2 by months
ggplot(flux.20, aes(hour(date_time), CO2))+
  geom_point(size=0.5)+
  facet_wrap(month(date_time)~.)

# diunral rain
ggplot(flux.20, aes(hour(date_time), P_RAIN_1_1_1))+
  geom_point(size=0.5)+
  facet_wrap(month(date_time)~.)

# graph CO2 when fluxes are removed or not removed
ggplot(flux.20[is.na(FC),], aes(date_time, CO2))+
      geom_point(size=0.5)

# not removed
ggplot(flux.20[!is.na(FC),], aes(date_time, CO2))+
  geom_point(size=0.5)

# Check G_1_1_1 and G_1_2_1
# Decision: APPROVE. largest out-of-range in July was during a large rain event
ggplot(flux.20, aes(x=date_time))+
  geom_line(aes(y=G_1_1_1, colour="G_1_1_1"))+
  geom_line(aes(y=G_1_2_1, colour="G_1_2_1"))+
  geom_col(aes(y=P_RAIN_1_1_1*10), colour="blue")

# Check LE
# Decision: APPROVE. dynamics match rain events. They are pulsey and sporadic but that's how the system works
ggplot(flux.20, aes(x=date_time))+
  geom_line(aes(y=LE, colour="LE"))+
  geom_col(aes(y=P_RAIN_1_1_1*100, colour="P_Rain_1_1_1"), colour="blue")

# look at dirunal
ggplot(flux.20, aes(x=hour(date_time)))+
  geom_point(aes(y=LE, colour="LE"))+
  geom_col(aes(y=P_RAIN_1_1_1*100, colour="P_Rain_1_1_1", width=0.3), colour="blue")+
  facet_wrap(month(date_time)~.)

# Check SWC_2_3_1
# Decision: Request to remove all soil moisture data and submit as seperate file?
# as complete update?
ggplot(flux.20, aes(x=date_time))+
  geom_line(aes(y=SWC_2_3_1, colour="SWC_2_3_1"))+
  geom_col(aes(y=P_RAIN_1_1_1/10, colour="P_Rain_1_1_1"), colour="blue")
