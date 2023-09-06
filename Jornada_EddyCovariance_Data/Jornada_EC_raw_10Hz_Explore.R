
library(data.table)
library(ggplot2)


ec_files <- list.files(path="/Volumes/SEL_Data_Archive/Research Data/Desert/Jornada/Bahada/Tower/EddyCovariance_ts/2013/Raw_Data/ASCII", full.names=TRUE) 
# read files and bind them into one file. fill=TRUE because of the missing columns in 2011

ec <- do.call("rbind", lapply(ec_files[152:200], header = FALSE, fread, sep=",", skip = 4,fill=TRUE,
                                   na.strings=c(-9999,"#NAME?"),
                                   col.names=c("TIMESTAMP",
                                               "RECORD",
                                               "Ux",	"Uy",	"Uz",	"Ts",	"CO2",	"H2O",
                                               "fw",	"press",	"diag_csat",	"agc",	"t_hmp",	"e_hmp")))

# coulmn names for 2013
ec2 <- do.call("rbind", lapply(ec_files[182], header = FALSE, fread, sep=",", skip = 4,fill=TRUE,
                              na.strings=c(-9999,"#NAME?"),
                              col.names=c("TIMESTAMP",
                                          "RECORD",
                                          "Ux",	"Uy",	"Uz",	"Ts",	"CO2",	"H2O",
                                          "fw",	"press",	"diag_csat","agc","t_hmp",	"e_hmp",
                                          "atm_press")))


ggplot(ec2, aes(TIMESTAMP, agc))+
  geom_point()

ggplot(ec2[agc<60], aes(TIMESTAMP, CO2))+
  geom_point()

ggplot(ec2[agc<60], aes(TIMESTAMP, H2O))+
  geom_point()

plot(ec$CO2)
plot(ec$H2O)
plot(ec$Ux)
plot(ec$Uy)
plot(ec$Uz)
