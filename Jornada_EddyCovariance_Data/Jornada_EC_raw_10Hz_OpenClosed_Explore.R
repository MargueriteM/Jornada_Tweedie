# Check raw 7200 data


library(data.table)
library(ggplot2)
library(lubridate)
library(dplyr)

ec_files <- list.files(path="/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Bahada/Tower/ts_data_2/2023/Raw_Data/ASCII", full.names=TRUE) 
ec_files24 <- list.files(path="/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Bahada/Tower/ts_data_2/2024/Raw_Data/ASCII", full.names=TRUE) 

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

ec6 <- do.call("rbind", lapply(ec_files[100:147], header = FALSE, fread, sep=",", skip = 4,fill=TRUE,
                               na.strings=c(-9999,"#NAME?"),
                               col.names=c("TIMESTAMP",
                                           "RECORD",
                                           "Ux",	"Uy",	"Uz",	"Ts",	"CO2",	"H2O",
                                           "fw",	"press",	"diag_csat","diag_irga_raw",	"agc",	"t_hmp",	"e_hmp",
                                           "atm_press",	"CO2_dry_7200_raw",	"H2O_dry_7200_raw",
                                           "AGC_7200_raw",	"tmpr_avg_7200_raw",	"press_tot_7200_raw")))


ec7 <- do.call("rbind", lapply(ec_files[148:156], header = FALSE, fread, sep=",", skip = 4,fill=TRUE,
                               na.strings=c(-9999,"#NAME?"),
                               col.names=c("TIMESTAMP",
                                           "RECORD",
                                           "Ux",	"Uy",	"Uz",	"Ts",	"CO2",	"H2O",
                                           "fw",	"press",	"diag_csat","diag_irga_raw",	"agc",	"t_hmp",	"e_hmp",
                                           "atm_press",	"CO2_dry_7200_raw",	"H2O_dry_7200_raw",
                                           "AGC_7200_raw",	"tmpr_avg_7200_raw",	"press_tot_7200_raw")))


ec8 <- do.call("rbind", lapply(ec_files[157:180], header = FALSE, fread, sep=",", skip = 4,fill=TRUE,
                               na.strings=c(-9999,"#NAME?"),
                               col.names=c("TIMESTAMP",
                                           "RECORD",
                                           "Ux",	"Uy",	"Uz",	"Ts",	"CO2",	"H2O",
                                           "fw",	"press",	"diag_csat","diag_irga_raw",	"agc",	"t_hmp",	"e_hmp",
                                           "atm_press",	"CO2_dry_7200_raw",	"H2O_dry_7200_raw",
                                           "AGC_7200_raw",	"tmpr_avg_7200_raw",	"press_tot_7200_raw")))

ec9 <- do.call("rbind", lapply(ec_files24[1:15], header = FALSE, fread, sep=",", skip = 4,fill=TRUE,
                               na.strings=c(-9999,"#NAME?"),
                               col.names=c("TIMESTAMP",
                                           "RECORD",
                                           "Ux",	"Uy",	"Uz",	"Ts",	"CO2",	"H2O",
                                           "fw",	"press",	"diag_csat","diag_irga_raw",	"agc",	"t_hmp",	"e_hmp",
                                           "atm_press",	"CO2_dry_7200_raw",	"H2O_dry_7200_raw",
                                           "AGC_7200_raw",	"tmpr_avg_7200_raw",	"press_tot_7200_raw")))

ec10 <- do.call("rbind", lapply(ec_files24[16:30], header = FALSE, fread, sep=",", skip = 4,fill=TRUE,
                               na.strings=c(-9999,"#NAME?"),
                               col.names=c("TIMESTAMP",
                                           "RECORD",
                                           "Ux",	"Uy",	"Uz",	"Ts",	"CO2",	"H2O",
                                           "fw",	"press",	"diag_csat","diag_irga_raw",	"agc",	"t_hmp",	"e_hmp",
                                           "atm_press",	"CO2_dry_7200_raw",	"H2O_dry_7200_raw",
                                           "AGC_7200_raw",	"tmpr_avg_7200_raw",	"press_tot_7200_raw")))

ec11 <- do.call("rbind", lapply(ec_files24[31:47], header = FALSE, fread, sep=",", skip = 4,fill=TRUE,
                                na.strings=c(-9999,"#NAME?"),
                                col.names=c("TIMESTAMP",
                                            "RECORD",
                                            "Ux",	"Uy",	"Uz",	"Ts",	"CO2",	"H2O",
                                            "fw",	"press",	"diag_csat","diag_irga_raw",	"agc",	"t_hmp",	"e_hmp",
                                            "atm_press",	"CO2_dry_7200_raw",	"H2O_dry_7200_raw",
                                            "AGC_7200_raw",	"tmpr_avg_7200_raw",	"press_tot_7200_raw")))


ec12 <- do.call("rbind", lapply(ec_files24[48:74], header = FALSE, fread, sep=",", skip = 4,fill=TRUE,
                                na.strings=c(-9999,"#NAME?"),
                                col.names=c("TIMESTAMP",
                                            "RECORD",
                                            "Ux",	"Uy",	"Uz",	"Ts",	"CO2",	"H2O",
                                            "fw",	"press",	"diag_csat","diag_irga_raw",	"agc",	"t_hmp",	"e_hmp",
                                            "atm_press",	"CO2_dry_7200_raw",	"H2O_dry_7200_raw",
                                            "AGC_7200_raw",	"tmpr_avg_7200_raw",	"press_tot_7200_raw")))

ec13 <- do.call("rbind", lapply(ec_files24[75:86], header = FALSE, fread, sep=",", skip = 4,fill=TRUE,
                                na.strings=c(-9999,"#NAME?"),
                                col.names=c("TIMESTAMP",
                                            "RECORD",
                                            "Ux",	"Uy",	"Uz",	"Ts",	"CO2",	"H2O",
                                            "fw",	"press",	"diag_csat","diag_irga_raw",	"agc",	"t_hmp",	"e_hmp",
                                            "atm_press",	"CO2_dry_7200_raw",	"H2O_dry_7200_raw",
                                            "AGC_7200_raw",	"tmpr_avg_7200_raw",	"press_tot_7200_raw")))


# subset every 60th row before merging
ec10.sub <- ec10 %>%
  filter(row_number() %% 60 == 1)

ec11.sub <- ec11 %>%
  filter(row_number() %% 60 == 1)

ec12.sub <- ec12 %>%
  filter(row_number() %% 60 == 1)

ec13.sub <- ec13 %>%
  filter(row_number() %% 60 == 1)


ec.all <- rbind(ec10.sub, ec11.sub, ec12.sub, ec13.sub)
#ec.all <- ec6
ec.all[,':=' (hour=hour(TIMESTAMP), minute = minute(TIMESTAMP), second = second(TIMESTAMP))]

# ec.sub <- copy(ec.all[second==0,])
# # if subsetting before merge:
ec.sub <- ec.all


# graph


# closed path
ggplot(ec.sub[CO2_dry_7200_raw>300,], aes(TIMESTAMP, CO2_dry_7200_raw))+
  geom_point(size=1)+
  geom_line(linewidth=0.5)

ggplot(ec.sub[H2O_dry_7200_raw<50], aes(TIMESTAMP, H2O_dry_7200_raw))+
  geom_point(size=1)+
  geom_line(linewidth=0.5)

ggplot(ec.sub[AGC_7200_raw>20], aes(TIMESTAMP, AGC_7200_raw))+
  geom_point(size=1)+
  geom_line(linewidth=0.5)


ggplot(ec.sub, aes(TIMESTAMP, tmpr_avg_7200_raw))+
  geom_point(size=1)+
  geom_line(linewidth=0.5)

ggplot(ec.sub, aes(TIMESTAMP, press_tot_7200_raw))+
  geom_point(size=1)+
  geom_line(linewidth=0.5)

# open path
ggplot(ec.sub[CO2>500&CO2<800], aes(TIMESTAMP, CO2))+
  geom_point(size=1)+
  geom_line(linewidth=0.5)

ggplot(ec.sub, aes(TIMESTAMP, H2O))+
  geom_point(size=1)+
  geom_line(linewidth=0.5)

ggplot(ec.sub, aes(TIMESTAMP, agc))+
  geom_point()

# specific time periods of closed-path CO2
ggplot(ec.sub[date(TIMESTAMP)>as.Date("2023-10-01")&CO2_dry_7200_raw>300,], aes(TIMESTAMP, CO2_dry_7200_raw))+
  geom_point(size=1)+
  geom_line(linewidth=0.5)

ggplot(ec.sub[date(TIMESTAMP)>as.Date("2023-11-05")&CO2_dry_7200_raw>300,], aes(TIMESTAMP, CO2_dry_7200_raw))+
  geom_point(size=1)+
  geom_line(linewidth=0.5)

ggplot(ec.sub[date(TIMESTAMP)>as.Date("2023-11-12")&CO2_dry_7200_raw>300,], aes(TIMESTAMP, CO2_dry_7200_raw))+
  geom_point(size=1)+
  geom_line(linewidth=0.5)


ggplot(ec.sub[date(TIMESTAMP)>as.Date("2023-12-25")&CO2_dry_7200_raw>300,], aes(TIMESTAMP, CO2_dry_7200_raw))+
  geom_point(size=1)+
  geom_line(linewidth=0.5)
