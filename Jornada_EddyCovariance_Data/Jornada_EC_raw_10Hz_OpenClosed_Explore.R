# Check raw 7200 data


library(data.table)
library(ggplot2)


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




ec <- do.call("rbind", lapply(ec_files[86:96], header = FALSE, fread, sep=",", skip = 4,fill=TRUE,
                              na.strings=c(-9999,"#NAME?"),
                              col.names=c("TIMESTAMP",
                                          "RECORD",
                                          "Ux",	"Uy",	"Uz",	"Ts",	"CO2",	"H2O",
                                          "fw",	"press",	"diag_csat","diag_irga_raw",	"agc",	"t_hmp",	"e_hmp",
                                          "atm_press",	"CO2_dry_7200_raw",	"H2O_dry_7200_raw",
                                          "AGC_7200_raw",	"tmpr_avg_7200_raw",	"press_tot_7200_raw")))


# graph
ggplot(ec, aes(TIMESTAMP, agc))+
  geom_point()


ggplot(ec, aes(TIMESTAMP, CO2_dry_7200_raw))+
  geom_point()
