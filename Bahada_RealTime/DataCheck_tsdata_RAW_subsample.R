# Code to check raw LI-7200 data
# from 1 minute sub-sampled files of ts_data_2
# written by M. Mauritz 20 May 2024

# Load Libraries
library(data.table)
library(ggplot2)
library(lubridate)
library(dplyr)
library(cowplot)

# list all filed on E drive for ts_data2 subsampled to 1 minute
ts_files <- list.files(path="/Volumes/Data/Bahada/CR3000/L1/EddyCovariance_ts_2/2024_1min", full.names=TRUE) 

# read and merge all files
ts <- do.call("rbind", lapply(ts_files, header = FALSE, fread, sep=",", skip = 4,fill=TRUE,
                              na.strings=c(-9999,-9999.0,-9999.00, -9999.000,"#NAME?"),
                              col.names=c("TIMESTAMP",
                                          "RECORD",
                                          "Ux",	"Uy",	"Uz",	"Ts",	"CO2",	"H2O",
                                          "fw",	"press",	"diag_csat","diag_irga_raw",	"agc",	"t_hmp",	"e_hmp",
                                          "atm_press",	"CO2_dry_7200_raw",	"H2O_dry_7200_raw",
                                          "AGC_7200_raw",	"tmpr_avg_7200_raw",	"press_tot_7200_raw")))

# Check data for LI-7200 and compare to LI-7500

# CO2 concentrations
p.co2.c <-ggplot(ts[CO2_dry_7200_raw>300 & CO2_dry_7200_raw<450,], aes(TIMESTAMP, CO2_dry_7200_raw))+
  geom_point(size=1)+
  geom_line(linewidth=0.5)+
  labs(title="Closed Path 7200 CO2 concentration (limited range 300-450)")

p.co2.o <- ggplot(ts[CO2>500&CO2<800], aes(TIMESTAMP, CO2))+
  geom_point(size=1)+
  geom_line(linewidth=0.5)+
  labs(title="Open Path 7500 CO2 concentration (limited range 500-800)")

# graph open-path and closed-path CO2 concentrations next to each other
plot_grid(p.co2.c,p.co2.o)

# H2O concentrations
p.h2o.c <-ggplot(ts[H2O_dry_7200_raw>=0 & H2O_dry_7200_raw<50], aes(TIMESTAMP, H2O_dry_7200_raw))+
  geom_point(size=1)+
  geom_line(linewidth=0.5)+
  labs(title="Closed Path 7200 H2O concentration (limited range 0-50)")

p.h2o.o <-ggplot(ts[H2O>(-9999) & H2O<40], aes(TIMESTAMP, H2O))+
  geom_point(size=1)+
  geom_line(linewidth=0.5)+
  labs(title="Open Path 7500 H2O concentration (limited range 0-40)")

# graph open-path and closed-path H2O concentrations next to each other
plot_grid(p.h2o.c,p.h2o.o)


# agc and signal strength
p.sigs.c <-ggplot(ts[AGC_7200_raw>20], aes(TIMESTAMP, AGC_7200_raw))+
  geom_point(size=1)+
  geom_line(linewidth=0.5)+
  labs(title="Closed Path 7200 Signal Strength: 100 when clean")


p.agc.o <-ggplot(ts[agc>-9999,], aes(TIMESTAMP, agc))+
  geom_point()+
  labs(title="Open Path 7500 AGC: 50-56 when clean, high is bad")

# graph open-path and closed-path signal strength and AGC next to each other
plot_grid(p.sigs.c,p.agc.o)

# graph temperature of closed path and sonic

p.T.c <- ggplot(ts[tmpr_avg_7200_raw>(-9999),], aes(TIMESTAMP, tmpr_avg_7200_raw))+
  geom_point(size=1)+
  geom_line(linewidth=0.5)+
  labs(title="Closed Path 7200 Air Temperature")

p.T.sonic <- ggplot(ts[Ts>(-9999),], aes(TIMESTAMP, Ts))+
  geom_point(size=1)+
  geom_line(linewidth=0.5)+
  labs(title="Sonic Anemometer Air Temperature")

# graph closed path and sonice temperature side-by-side
plot_grid(p.T.c, p.T.sonic)


# graph atmospheric pressure from closed path 7200
ggplot(ts[press_tot_7200_raw>(-9999),], aes(TIMESTAMP, press_tot_7200_raw))+
  geom_point(size=1)+
  geom_line(linewidth=0.5)+
  labs(title="Closed Path 7200 Atmospheric Pressure")


# specific time periods of closed-path CO2
ggplot(ts[date(TIMESTAMP)>as.Date("2023-10-01")&CO2_dry_7200_raw>300,], aes(TIMESTAMP, CO2_dry_7200_raw))+
  geom_point(size=1)+
  geom_line(linewidth=0.5)

ggplot(ts[date(TIMESTAMP)>as.Date("2023-11-05")&CO2_dry_7200_raw>300,], aes(TIMESTAMP, CO2_dry_7200_raw))+
  geom_point(size=1)+
  geom_line(linewidth=0.5)

ggplot(ts[date(TIMESTAMP)>as.Date("2023-11-12")&CO2_dry_7200_raw>300,], aes(TIMESTAMP, CO2_dry_7200_raw))+
  geom_point(size=1)+
  geom_line(linewidth=0.5)


ggplot(ts[date(TIMESTAMP)>as.Date("2023-12-25")&CO2_dry_7200_raw>300,], aes(TIMESTAMP, CO2_dry_7200_raw))+
  geom_point(size=1)+
  geom_line(linewidth=0.5)



