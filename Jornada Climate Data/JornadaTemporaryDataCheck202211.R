# Data check for temporary Bajada files

library(lubridate)
library(ggplot2)
library(dplyr)

setwd("~/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/20221104 - CZO_data")

# FLUX data
flux.name <- colnames(read.table("Bahada_CR3000_flux.dat", sep=",", dec=".",skip=1, header=TRUE))
flux <- read.table("Bahada_CR3000_flux.dat", sep=",", dec=".",skip=4,col.names = flux.name)

flux <- flux %>%
  mutate(datetime = ymd_hms(TIMESTAMP))

summary(flux$datetime)

ggplot(flux, aes(datetime, Fc_wpl))+geom_point()
ggplot(flux, aes(datetime, LE_wpl))+geom_point()
ggplot(flux, aes(datetime, Hc))+geom_point()+geom_line()
ggplot(flux, aes(datetime, agc_Avg))+geom_point()+geom_line()

ggplot(flux, aes(datetime, Rs_downwell_Avg))+geom_point()
ggplot(flux, aes(datetime, Rs_upwell_Avg))+geom_point()
ggplot(flux, aes(datetime, Rl_downwell_Avg))+geom_point()
ggplot(flux, aes(datetime, Rs_upwell_Avg))+geom_point()

ggplot(flux, aes(datetime, hfp01_1_Avg))+geom_point()
ggplot(flux, aes(datetime, hfp01_2_Avg))+geom_point()
ggplot(flux, aes(datetime, hfp01_3_Avg))+geom_point()
ggplot(flux, aes(datetime, hfp01_4_Avg))+geom_point()

ggplot(flux, aes(datetime, TS_surface_1_Avg))+geom_point()
ggplot(flux, aes(datetime, TS_surface_2_Avg))+geom_point()

ggplot(flux, aes(datetime, precip_Tot))+geom_point()

ggplot(flux, aes(datetime, t_hmp_mean))+geom_point()

# MET data
met.name <- colnames(read.table("Bahada_CR3000_met_data.dat", sep=",", dec=".",skip=1, header=TRUE))
met <- read.table("Bahada_CR3000_met_data.dat", sep=",", dec=".",skip=4,col.names = met.name)

met <- met %>%
  mutate(datetime = ymd_hms(TIMESTAMP))

summary(met$datetime)

ggplot(met, aes(datetime, precip_Tot))+geom_point()

ggplot(met, aes(datetime, t_hmp))+geom_point()

ggplot(met, aes(datetime, NetRs))+geom_point()
ggplot(met, aes(datetime, NetRl))+geom_point()
ggplot(met, aes(datetime, UpTot))+geom_point()
ggplot(met, aes(datetime, DnTot))+geom_point()

soil.name <- colnames(read.table("Bahada_CR3000_Soil_CS650.dat", sep=",", dec=".",skip=1, header=TRUE))
soil <- read.table("Bahada_CR3000_Soil_CS650.dat", sep=",", dec=".",skip=4,col.names = soil.name)

soil <- soil %>%
  mutate(datetime = ymd_hms(TIMESTAMP))

summary(soil$datetime)

ggplot(soil, aes(datetime, CS650_VWC_1_AVG))+geom_point()
ggplot(soil, aes(datetime, CS650_VWC_2_AVG))+geom_point()
ggplot(soil, aes(datetime, CS650_VWC_3_AVG))+geom_point()
ggplot(soil, aes(datetime, CS650_VWC_4_AVG))+geom_point()
ggplot(soil, aes(datetime, CS650_VWC_5_AVG))+geom_point()

ggplot(soil, aes(datetime, CS650_T_1_AVG))+geom_point()
ggplot(soil, aes(datetime, CS650_T_2_AVG))+geom_point()
ggplot(soil, aes(datetime, CS650_T_3_AVG))+geom_point()
ggplot(soil, aes(datetime, CS650_T_4_AVG))+geom_point()
ggplot(soil, aes(datetime, CS650_T_5_AVG))+geom_point()

