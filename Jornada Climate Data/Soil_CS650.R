# Graph CS650 Data from US-Jo1 Bajada Site
# Initial glance as CS650 data 

# Notes on CS650 12cm Installation at US-Jo1 for Dryland CZ Project
# 27 Mar 2021

# Probes installed (Lin Ma, Mark Engle, Nohemi Garay Valenzuela, Christian L Leach)

# Installation start~11:05am 
# Installation and testing end: ~5pm

# All read 1.9-2.2% VWC at same location

# SDI, SN, Depth (cm) of sensors, Location relative to Caliche
# SDI 1 SN 46381 Depth 100.5cm Caliche inside/below
# SDI 2 SN 46371 Depth 42.5cm Caliche above
# SDI 3 SN 46374 Depth 25.5 cm Caliche above
# SDI 4 SN 46416 Depth 17.5 cm Caliche above
# SDI 5 SN 46414 Depth 11.5 cm Caliche above

# load libraries
library(tidyverse)


# set working directory
setwd("~/Desktop/OneDrive - University of Texas at El Paso/CZ_Drylands/USJo1")

# load headers of file and data
cs650names <- colnames(read.table("dataL1_SoilCS650_2021.csv", sep=",", skip=1,heade=TRUE))
cs650wide <- read.table("dataL1_SoilCS650_2021.csv", sep=",", skip=4, col.names = cs650names, na.strings="-9999")

# convert to long format
cs650 <- cs650wide %>%
  pivot_longer(-c(TIMESTAMP,RECORD),names_to="IDcol") %>%
  separate(IDcol, c(NA,"metric","probe",NA), sep="_")

# format date/time and create depth labels for probes
cs650 <- cs650 %>%
  mutate(datetime = ymd_hms(TIMESTAMP)) %>%
  mutate(probe_depth = case_when(probe %in% "1" ~ 100.5,
                                 probe %in% "2" ~ 42.5,
                                 probe %in% "3" ~ 25.5,
                                 probe %in% "4" ~ 17.5,
                                 probe %in% "5" ~ 11.5))


# graph different metrics: "EC"  "P"   "PA"  "T"   "VR"  "VWC"
# graph VWC
cs650 %>%
  filter(metric %in% c("VWC") & !is.na(value)) %>%
  ggplot(., aes(datetime, value, colour=factor(probe_depth)))+
  geom_line()+
  labs(title = "VWC")


# graph T
cs650 %>%
  filter(metric %in% c("T") & !is.na(value)) %>%
  ggplot(., aes(datetime, value, colour=probe))+
  geom_line()+
  labs(title = "Soil Temperature")

