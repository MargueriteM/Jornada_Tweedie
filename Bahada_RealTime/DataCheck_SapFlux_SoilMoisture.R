# Check soil moisture and water potential data from Sap Flux Network 
# probes installed 11 August 2023
# script author: Marguerite Mauritz
# 17 August 2023

# load libraries
library(data.table)
library(ggplot2)
library(lubridate)
library(tidyr)
library(dplyr)

# import data 
basedir <- "/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Bahada/SapFlowNet/Data/Raw"
setwd(basedir)
# read column names and units
sfn_soildat_colnames <- fread("BajadaCR1000XSapFlowSoilMoisture_SoilData_SapFlow.dat",
                       header = TRUE, skip=1,sep=",", fill=TRUE,
                       na.strings=c(-9999,"#NAME?"))[1,]
# read data and use column names
sfn_soildat <- fread("BajadaCR1000XSapFlowSoilMoisture_SoilData_SapFlow.dat",
              header = FALSE, skip=4, sep=",", fill=TRUE,
              na.strings=c(-9999,"#NAME?"),
              col.names=colnames(sfn_soildat_colnames))

# format TIMESTAMP and make long format
sfn_soildat <- sfn_soildat %>%
  mutate(datetime = ymd_hms(TIMESTAMP, tz="UTC")) %>%
  pivot_longer(!c(TIMESTAMP, datetime, RECORD),names_to="probe", values_to="value")

# separate columns from probe into metric and probe.number
# specify columns to skip
rows_to_skip <- c("BattV_Avg","PTemp_C_Avg")

sfn_soildat1 <- sfn_soildat %>%
  filter(!probe %in% rows_to_skip) %>% 
  tidyr::separate(probe, into=c("metric","probe.num"), sep = "_", remove=FALSE, fill="right", extra="drop") %>%
bind_rows(filter(sfn_soildat, probe %in% rows_to_skip))%>%
  mutate(metric=case_when (probe %in% rows_to_skip ~ as.character(probe),
                           TRUE ~ as.character(metric)))

# graph data by metric and color by number (option to select specific probes)
sfn_soildat1 %>% filter(datetime>as.Date("2023-08-10")&
                          #probe.num %in% c(NA,1,2,3)& # option to select specific probes
                          metric %in% c("BattV_Avg","PTemp_C_Avg","T","Temp","VWC","WaterPot"))%>%
ggplot(., aes(datetime, value,color=factor(probe.num)))+
  geom_line()+
  facet_grid(metric~.,scales="free_y")

