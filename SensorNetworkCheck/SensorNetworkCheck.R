##############################
# Check Sensor Network Data  #
# to identify broken sensors #
#                            #
#   Created: 8 Jun 2022      #
# M. Mauritz & V. Martinez   #
##############################

####
# Sensor Network Data Units & Distances:
####
# Units of data:
# rain, mm
# pressure, mbar
# leaf wetness (lws), no unit
# par, uE
# solar radiation (solar), Wm-2
# soil moisture (moisture), m-3/m-3
# battery, V
# voltage, V
# current, mA

# Sensor network distances on tramline
# SN1: 104m
# SN2: 87.5m
# SN3: 64m
# SN4: 24m
# SN5: 36m
# SN6: 22m
# SN7: 79.5m
# SN8: 3m

# load libraries
library(tidyverse)
# if tidyverse doesn't work:
# library(ggplot2)
# library(dplyr)
# library(tidyr)

# Import data
setwd("~/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/SensorNetwork_Data")

sn_2020 <- read.table("WSN_L2_2020.csv", sep=",", header=TRUE, na.string=("NA"))
sn_2021 <- read.table("WSN_L2_2021.csv", sep=",", header=TRUE, na.string=("NA"))
sn_2022 <- read.table("WSN_L2_2022.csv", sep=",", header=TRUE, na.string=("NA"))

# combine individual dataframes into 1
sn <- dplyr::bind_rows(sn_2020, sn_2021, sn_2022)
summary(sn)

# convert date_time in sn to posixct
sn <- sn %>%
  mutate(date_time = ymd_hms(date_time))


# make data long with column names for each description of measurement
sn_long <-  sn %>%
  pivot_longer(!date_time,
               names_to=c("SN","variable","unit","veg","depth"),
                                     names_sep="_",
                                     values_to="value" )


# convert date_time column to datetime object and create a year column
sn_long <- sn_long %>%
  mutate(date_time = ymd_hms(date_time),
         year=year(date_time))

# graphs in wide format

# PRGL, SN8 PAR
ggplot(sn,aes(date_time, SN8_par_uE_PRGL_NA))+
  geom_line()

# PRGL and MUPO
ggplot(sn,aes(x=date_time))+
  geom_line(aes(y=SN8_par_uE_PRGL_NA), color="green")+
  geom_line(aes(y=SN8_par_uE_MUPO_NA), color="blue")

# graphs in long format
# PAR for SN8
sn_long %>%
  filter(SN=="SN8", variable=="par")%>%
  ggplot(., aes(date_time, value, color=veg))+
  geom_line()+
  facet_grid(veg~year, scale="free_x")

# solar radiation for SN8
sn_long %>%
  filter(SN=="SN8", variable=="solar")%>%
  ggplot(., aes(date_time, value, color=veg))+
  geom_line()+
  facet_grid(veg~year, scale="free_x")

# SN5
# par for SN5
sn_long %>%
  filter(SN=="SN5", variable=="battery")%>%
  ggplot(., aes(date_time, value, color=veg))+
  geom_line()+
  facet_grid(veg~year, scale="free_x")


# par for SN5
sn_long %>%
  filter(SN=="SN5", variable=="par")%>%
  ggplot(., aes(date_time, value, color=veg))+
  geom_line()+
  facet_grid(veg~year, scale="free_x")

# solar radiation for SN5
sn_long %>%
  filter(SN=="SN5", variable=="solar")%>%
  ggplot(., aes(date_time, value, color=veg))+
  geom_line()+
  facet_grid(veg~year, scale="free_x")

# see solar radiation across all SN
sn_long %>%
  filter(variable=="solar")%>%
  ggplot(., aes(date_time, value, color=SN))+
  geom_line()+
  facet_grid(veg~year, scale="free_x")

sn_long %>%
  filter(variable=="solar")%>%
  ggplot(., aes(date_time, value, color=SN))+
  geom_line()+
  facet_grid(SN+veg~year, scale="free_x")

