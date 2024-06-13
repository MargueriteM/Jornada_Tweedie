# Code to check raw LI-7500 and LI-7200 data
# from 1 minute sub-sampled files of ts_data_2
# COMPARE open and closed patch, check notes for each variable on comparison

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
                              na.strings=c(-9999,"#NAME?"),
                              col.names=c("TIMESTAMP",
                                          "RECORD",
                                          "Ux",	"Uy",	"Uz",	"Ts",	"CO2",	"H2O",
                                          "fw",	"press",	"diag_csat","diag_irga_raw",	"agc",	"t_hmp",	"e_hmp",
                                          "atm_press",	"CO2_dry_7200_raw",	"H2O_dry_7200_raw",
                                          "AGC_7200_raw",	"tmpr_avg_7200_raw",	"press_tot_7200_raw")))

# Check data for LI-7200 and compare to LI-7500

# Print most recent record in dataset
max(ts$TIMESTAMP)

# CO2 concentrations
# Check: how well do patterns match between open and closed path
p.co2.c <-ggplot(ts[CO2_dry_7200_raw>300 & CO2_dry_7200_raw<450,], aes(TIMESTAMP, CO2_dry_7200_raw))+
  geom_point(size=1)+
  geom_line(linewidth=0.5)+
  ylim(c(350,500))+
  labs(title="Closed Path 7200 CO2 umol/mol (limited range 300-450)")


# define R, universal gas constant, to convert CO2 in mg/m3 to umol/mol
# CO2_mm_m3*R*(t_hmp+273.15)/press*1000
R.gc <- 8.3143e-3

p.co2.o <- ggplot(ts[CO2>500&CO2<800], aes(TIMESTAMP, (CO2/44)*R.gc*(t_hmp+273.14)/press*1000))+
  geom_point(size=1)+
  geom_line(linewidth=0.5)+
  ylim(c(350,500))+
  labs(title="Open Path 7500 CO2 umol/mol (limited range 500-800)")

# graph open-path and closed-path CO2 concentrations next to each other
# Check: do patterns match?
plot_grid(p.co2.c,p.co2.o)

# H2O concentrations
# Check: how well to patterns match between open and closed?
p.h2o.c <-ggplot(ts[H2O_dry_7200_raw>=0 & H2O_dry_7200_raw<50], aes(TIMESTAMP, H2O_dry_7200_raw))+
  geom_point(size=1)+
  geom_line(linewidth=0.5)+
  ylim(c(0,50))+
  labs(title="Closed Path 7200 H2O mmol/mol (limited range 0-50)")

# convert H2O from mg/m3 to mmol/mol
# H2O_mm_m = H2O_mm_m3*R*(t_hmp+273.15)/press
p.h2o.o <-ggplot(ts[H2O>=(0) & H2O<40], aes(TIMESTAMP, (H2O/0.018)*R.gc*(t_hmp+273.14)/press))+
  geom_point(size=1)+
  geom_line(linewidth=0.5)+
  ylim(c(0,50))+
  labs(title="Open Path 7500 H2O mmol/mol (limited range 0-40)")

# graph open-path and closed-path H2O concentrations next to each other
# Check: do patterns match?
plot_grid(p.h2o.c,p.h2o.o)


# agc and signal strength
# Check: if AGC_7200 is declining or low, need to clean, if open path AGC is high, need to clean
p.sigs.c <-ggplot(ts[AGC_7200_raw>20], aes(TIMESTAMP, AGC_7200_raw))+
  geom_point(size=1)+
  geom_line(linewidth=0.5)+
  geom_hline(yintercept=100, colour="green")+
  labs(title="Closed Path 7200 Signal Strength: 100 when clean")


p.agc.o <-ggplot(ts, aes(TIMESTAMP, agc))+
  geom_point()+
  geom_hline(yintercept=50, colour="green")+
  geom_hline(yintercept=56, colour="green")+
  labs(title="Open Path 7500 AGC: 50-56 when clean, high is bad")

# graph open-path and closed-path signal strength and AGC next to each other
# Check: if AGC_7200 is declining or low, need to clean, if open path AGC is high, need to clean
plot_grid(p.sigs.c,p.agc.o)

# plot signal strength with CO2 concentration to see alignment
plot_grid(p.co2.c,p.co2.o,p.sigs.c,p.agc.o, align="hv")

# graph temperature of closed path and sonic
# Check: do patterns and magnitudes match?
p.T.c <- ggplot(ts, aes(TIMESTAMP, tmpr_avg_7200_raw))+
  geom_point(size=1)+
  geom_line(linewidth=0.5)+
  labs(title="Closed Path 7200 Air Temperature")

p.T.sonic <- ggplot(ts, aes(TIMESTAMP, Ts))+
  geom_point(size=1)+
  geom_line(linewidth=0.5)+
  labs(title="Sonic Anemometer Air Temperature")

# graph closed path and sonice temperature side-by-side
# Check: do patterns and magnitudes match?
plot_grid(p.T.c, p.T.sonic)

# graph 1:1 of sonic and open path temp
ggplot(ts, aes(Ts, tmpr_avg_7200_raw))+
  geom_point(size=1)+
  geom_abline(intercept=0,slope=1, colour="red", linetype="dashed")+
  labs(title="Air Temp: Sonic vs Closed Path")

# graph atmospheric pressure from closed path 7200
# Check: should be between 83 and 85
ggplot(ts, aes(TIMESTAMP, press_tot_7200_raw))+
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



