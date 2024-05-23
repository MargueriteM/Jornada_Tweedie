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
basedir <- "C:/Users/vmartinez62/OneDrive - University of Texas at El Paso/Bahada/SapFlowNet/Data/ASCII"
setwd(basedir)

# read column names and units
sfn_soildat_colnames1 <- fread("BajadaCR1000XSapFlowSoilMoisture_SoilData_SapFlow.dat",
                       header = TRUE, skip=1,sep=",", fill=TRUE,
                       na.strings=c(-9999,"#NAME?"))[1,]
# read data and use column names
sfn_soildat1 <- fread("BajadaCR1000XSapFlowSoilMoisture_SoilData_SapFlow.dat",
              header = FALSE, skip=4, sep=",", fill=TRUE,
              na.strings=c(-9999,"#NAME?"),
              col.names=colnames(sfn_soildat_colnames1))

# read data and use column names from single file
# sfn_soildat2 <- fread("TOA5_20231027_26185.Sapflow_Soil_2023_09_15_1330.dat",
#                       header = FALSE, skip=4, sep=",", fill=TRUE,
#                       na.strings=c(-9999,"#NAME?"),
#                       col.names=colnames(sfn_soildat_colnames1))

# list all 26185.Sapflow files
sfn_files <- list.files(path=basedir,full.names=TRUE, pattern="26185.Sapflow_Soil")

# import and combine data from sfn_files
sfn_soildat2 <- do.call("rbind", lapply(sfn_files, header = FALSE, fread, sep=",", dec=".",skip = 4,
                                       fill=TRUE, na.strings=c(-9999,"#NAME?"), col.names=colnames(sfn_soildat_colnames1)))

# read metadata for probe IDs (accurate after 10-27-2023 when probes were moved from mesquite 2 to 5cm at M1, C2, bare)
sfn_metadata <- fread("C:/Users/vmartinez62/OneDrive - University of Texas at El Paso/Bahada/SapFlowNet/sapfluxProbeID_Metadata.csv")

# merge both data files
sfn_soildat <- rbind(sfn_soildat1, sfn_soildat2)

# format TIMESTAMP and make long format
sfn_soildat <- sfn_soildat %>%
  mutate(datetime = ymd_hms(TIMESTAMP, tz="UTC")) %>%
  pivot_longer(!c(TIMESTAMP, datetime, RECORD),names_to="measurement", values_to="value")

# specify columns to skip
rows_to_skip <- c("BattV_Avg","PTemp_C_Avg")

# separate columns from probe into metric and probe.number
sfn_soildat <- sfn_soildat %>%
  filter(!measurement %in% rows_to_skip) %>% 
  tidyr::separate(measurement, into=c("metric","probe.num"), sep = "_", remove=FALSE, fill="right", extra="drop") %>%
bind_rows(filter(sfn_soildat, measurement %in% rows_to_skip))%>%
  mutate(metric=case_when (measurement %in% rows_to_skip ~ as.character(measurement),
                           TRUE ~ as.character(metric)))

# add metadata to data file
sfn_soildat <- right_join(sfn_soildat,sfn_metadata, by="measurement")


# graph to check datalogger voltage and paneltemp
sfn_soildat %>% filter(datetime>as.Date("2023-08-10")&
                         metric %in% c("BattV_Avg","PTemp_C_Avg"))%>%
  ggplot(., aes(datetime, value))+
  geom_line()+
  facet_grid(metric~location,scales="free_y")


#data after this is after mesquite_23 probes were removed and placed at 5cm depth at bare, creosote_1,creosote_2 and mesquite_1
#creosote_1 does not have 5cm probe sensor


# graph data by metric and color by number 
sfn_soildat %>% filter(datetime>as.Date("2023-10-27")&
                          #probe.num %in% c(NA,1,2,3)&
                          metric %in% c("T","Temp","VWC","WaterPot"))%>%
ggplot(., aes(datetime, value,color=factor(depth)))+
  geom_line()+
  facet_grid(metric~location,scales="free_y")

#graph with VWC values less than one
sfn_soildat %>% filter(datetime>as.Date("2023-10-27")&
                         #probe.num %in% c(NA,1,2,3)&
                         ((metric=="VWC"&value<=1)|metric %in% c("T","Temp","WaterPot")))%>%
  ggplot(., aes(datetime, value,color=factor(depth)))+
  geom_line()+
  facet_grid(metric~location,scales="free_y")

#graph VWC and T from CS650 without 5cm
sfn_soildat %>% filter(datetime>as.Date("2023-10-27")&
                         #probe.num %in% c(NA,1,2,3)&
                         metric %in% c("T","VWC")&
                         depth!=5)%>%
  ggplot(., aes(datetime, value,color=factor(depth)))+
  geom_line()+
  facet_grid(metric~location,scales="free_y")


# graph data only from 5cm 
sfn_soildat %>% filter(datetime>as.Date("2023-10-27")&
                         #probe.num %in% c(NA,1,2,3)&
                         metric %in% c("T","Temp","VWC","WaterPot")&
                         depth==5)%>%
  ggplot(., aes(datetime, value,color=factor(depth)))+
  geom_line()+
  facet_grid(metric~location,scales="free_y")

