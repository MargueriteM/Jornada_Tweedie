# Check raw 7200 data


library(data.table)
library(ggplot2)
library(lubridate)
library(dplyr)

ec_files <- list.files(path="/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Bahada/Tower/ts_data_2/2023/Raw_Data/ASCII", full.names=TRUE) 
# read files and bind them into one file. fill=TRUE because of the missing columns in 2011

ec <- do.call("rbind", lapply(ec_files[1:5], header = FALSE, fread, sep=",", skip = 4,fill=TRUE,
                              na.strings=c(-9999,"#NAME?"),
                              col.names=c("TIMESTAMP",
                                          "RECORD",
                                          "Ux",	"Uy",	"Uz",	"Ts",	"CO2",	"H2O",
                                          "fw",	"press",	"diag_csat","diag_irga_raw",	"agc",	"t_hmp",	"e_hmp",
                                          "atm_press",	"CO2_dry_7200_raw",	"H2O_dry_7200_raw",
                                          "AGC_7200_raw",	"tmpr_avg_7200_raw",	"press_tot_7200_raw")))

ec2 <- do.call("rbind", lapply(ec_files[6:7], header = FALSE, fread, sep=",", skip = 4,fill=TRUE,
                              na.strings=c(-9999,"#NAME?"),
                              col.names=c("TIMESTAMP",
                                          "RECORD",
                                          "Ux",	"Uy",	"Uz",	"Ts",	"CO2",	"H2O",
                                          "fw",	"press",	"diag_csat","diag_irga_raw",	"agc",	"t_hmp",	"e_hmp",
                                          "atm_press",	"CO2_dry_7200_raw",	"H2O_dry_7200_raw",
                                          "AGC_7200_raw",	"tmpr_avg_7200_raw",	"press_tot_7200_raw")))



ec3 <- do.call("rbind", lapply(ec_files[8:76], header = FALSE, fread, sep=",", skip = 4,fill=TRUE,
                               na.strings=c(-9999,"#NAME?"),
                               col.names=c("TIMESTAMP",
                                           "RECORD",
                                           "Ux",	"Uy",	"Uz",	"Ts",	"CO2",	"H2O",
                                           "fw",	"press",	"diag_csat","diag_irga_raw",	"agc",	"t_hmp",	"e_hmp",
                                           "atm_press",	"CO2_dry_7200_raw",	"H2O_dry_7200_raw",
                                           "AGC_7200_raw",	"tmpr_avg_7200_raw",	"press_tot_7200_raw")))


ec4 <- do.call("rbind", lapply(ec_files[77:96], header = FALSE, fread, sep=",", skip = 4,fill=TRUE,
                              na.strings=c(-9999,"#NAME?"),
                              col.names=c("TIMESTAMP",
                                          "RECORD",
                                          "Ux",	"Uy",	"Uz",	"Ts",	"CO2",	"H2O",
                                          "fw",	"press",	"diag_csat","diag_irga_raw",	"agc",	"t_hmp",	"e_hmp",
                                          "atm_press",	"CO2_dry_7200_raw",	"H2O_dry_7200_raw",
                                          "AGC_7200_raw",	"tmpr_avg_7200_raw",	"press_tot_7200_raw")))


ec5 <- do.call("rbind", lapply(ec_files[97:103], header = FALSE, fread, sep=",", skip = 4,fill=TRUE,
                               na.strings=c(-9999,"#NAME?"),
                               col.names=c("TIMESTAMP",
                                           "RECORD",
                                           "Ux",	"Uy",	"Uz",	"Ts",	"CO2",	"H2O",
                                           "fw",	"press",	"diag_csat","diag_irga_raw",	"agc",	"t_hmp",	"e_hmp",
                                           "atm_press",	"CO2_dry_7200_raw",	"H2O_dry_7200_raw",
                                           "AGC_7200_raw",	"tmpr_avg_7200_raw",	"press_tot_7200_raw")))

ec.all <- rbind(ec, ec2,ec3,ec4)
ec.all <- ec5
ec.all[,':=' (hour=hour(TIMESTAMP), minute = minute(TIMESTAMP), second = second(TIMESTAMP))]

ec.sub <- copy(ec.all[second==0,])


# graph
ggplot(ec4, aes(TIMESTAMP, agc))+
  geom_point()


ggplot(ec.sub[CO2_dry_7200_raw>300,], aes(TIMESTAMP, CO2_dry_7200_raw))+
  geom_point(size=1)+
  geom_line(linewidth=0.5)

ggplot(ec.sub[CO2_dry_7200_raw>300,], aes(TIMESTAMP, CO2))+
  geom_point(size=1)+
  geom_line(linewidth=0.5)



ggplot(ec.sub[date(TIMESTAMP)>as.Date("2023-10-01")&CO2_dry_7200_raw>300,], aes(TIMESTAMP, CO2_dry_7200_raw))+
  geom_point(size=1)+
  geom_line(linewidth=0.5)

ggplot(ec.sub[date(TIMESTAMP)>as.Date("2023-11-05")&CO2_dry_7200_raw>300,], aes(TIMESTAMP, CO2_dry_7200_raw))+
  geom_point(size=1)+
  geom_line(linewidth=0.5)

ggplot(ec.sub[date(TIMESTAMP)>as.Date("2023-11-12")&CO2_dry_7200_raw>300,], aes(TIMESTAMP, CO2_dry_7200_raw))+
  geom_point(size=1)+
  geom_line(linewidth=0.5)


ggplot(ec.sub[date(TIMESTAMP)>as.Date("2023-11-13")&CO2_dry_7200_raw>300,], aes(TIMESTAMP, CO2_dry_7200_raw))+
  geom_point(size=1)+
  geom_line(linewidth=0.5)+facet_grid(.~date(TIMESTAMP))
