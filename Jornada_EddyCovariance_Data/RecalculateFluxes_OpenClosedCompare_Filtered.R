####################################################
# Read in full output data from EddyPro.          #
# recompete co2 fluxes with                       #
# scf and wpl correction to                       #
# to correct w'co2 by 0.9 (Russ Scott suggestion) #    
# correction with help from James & Israel at Licor #
#     12 Jan 2023                                 #
####################################################

# load libraries
library(ggplot2) # library for making figures in ggplot package
library(lubridate) # library for easier date manipulation 
library(data.table) # library for data table which is more efficient with large data sets
library(reader)
library(tidyr)
library(lsr) # contains quantileCut function
library(gridExtra)
library(viridis)
library(cowplot)
#############
# IMPORT DATA
#############
# get header and unit info from the first rows of data files
# file info
# fileinfo_open <- scan("/Users/memauritz/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_EddyPro_filtered/JER_flux_202210_202405_EddyPro_FullOutput_filterSD_20240623.Rdata",
#                         what='',sep=",",nlines=1)
# fileinfo_open <- data.table(t(fileinfo_open))
# # read only the first row to get the units
# flux.units.open <- (fread("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Bahada/Tower/EddyCovariance_ClosedPath/EddyPro_Out/eddypro_JER_2023_0811_1010_ClosedPathCompare2_full_output_2023-10-18T121739_adv.csv",header=TRUE,skip=1))[1,]


# closed path
fileinfo_closed <- scan("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Bahada/Tower/ts_data_2/2023/EddyPro_Out/ClosedPath/JER_flux_2023_EddyPro_FullOutput_filterSD_JuneDec_Closed.csv",
                      what='',sep=",",nlines=1)
fileinfo_closed <- data.table(t(fileinfo_closed))
# read only the first row to get the units
# flux.units.closed <- (fread("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Bahada/Tower/ts_data_2/2023/EddyPro_Out/ClosedPath/JER_flux_2023_EddyPro_FullOutput_filterSD_JuneDec_Closed.csv",header=TRUE))[1,]


# read the data, skippping the units row
# open path
load("/Users/memauritz/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_EddyPro_filtered/JER_flux_202210_202405_EddyPro_FullOutput_filterSD_20240623.Rdata")
flux_open <- flux_filter_sd
rm(flux_filter_sd)

# closed path
flux_closed2023 <- fread("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Bahada/Tower/ts_data_2/2023/EddyPro_Out/ClosedPath/JER_flux_2023_EddyPro_FullOutput_filterSD_JuneDec_Closed.csv", sep=",",
                   header=TRUE, na.strings=c("-9999","-9999.0","NAN","#NAME?","NA"))

flux_closed2024 <- fread("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Bahada/Tower/ts_data_2/2024/EddyPro_Out/ClosedPath/JER_flux_2024_EddyPro_FullOutput_filterSD_JanMay_Closed.csv", sep=",",
                         header=TRUE, na.strings=c("-9999","-9999.0","NAN","#NAME?","NA"))

flux_closed <- rbind(flux_closed2023,flux_closed2024)

# column numbers differ, find difference
c(setdiff(colnames(flux_open), colnames(flux_closed)), setdiff(colnames(flux_closed), colnames(flux_open)))

# change column names and merge
setnames(flux_open, 1, paste0(names(flux_open)[1], '_open'))
setnames(flux_open, 5:259, paste0(names(flux_open)[5:259], '_open'))

setnames(flux_closed, 1:2, paste0(names(flux_closed)[1:2], '_closed'))
setnames(flux_closed, 6:237, paste0(names(flux_closed)[6:237], '_closed'))

# merge open and closed
keycols = c("date","time","DOY")

setkeyv(flux_open, keycols)
setkeyv(flux_closed, keycols)

flux <- flux_open[flux_closed,]  

# format date
flux[,date_time := paste(date,time,sep=" ")]
flux[,':=' (date=as.Date(date),
            date_time = as.POSIXct(date_time, format="%Y-%m-%d %H:%M"),
            year=year(date_time))]

# Graph initial CO2 flux, just to see
# co2 flux
ggplot(flux, aes(date_time,co2_flux_open,colour=factor(qc_co2_flux_open)))+
  geom_point(size=0.2)+
  ylim(c(-20,20))

ggplot(flux, aes(date_time,co2_flux_closed,colour=factor(qc_co2_flux_closed)))+
  geom_point(size=0.2)+
  ylim(c(-20,20))

# graph the time-series of CO2 flux together
f1 <- ggplot(flux, aes(x=date_time))+
  geom_line(aes(y=co2_flux_open), linewidth=0.2, color="black")+
  geom_line(aes(y=co2_flux_closed), linewidth=0.2, color="green")+
    ylim(c(-10,10))+
  theme_bw()

## graph the closed path and provisionally corrected flux
f2 <- ggplot(flux, aes(x=date_time))+
  geom_line(aes(y=fc_wpl_adjust_open), linewidth=0.2, color="darkgrey")+
  geom_line(aes(y=co2_flux_closed), linewidth=0.2, color="green")+
  ylim(c(-10,10))+
  theme_bw()

plot_grid(f1+theme(axis.text.x=element_blank(), axis.title.x = element_blank()),
          f2,nrow=2, align="v")

# graph the difference between open vs closed and open adjusted vs closed
f1.diff <- ggplot(flux, aes(x=date_time))+
  geom_line(aes(y=abs(co2_flux_open)-abs(co2_flux_closed)), linewidth=0.2, color="black")+
  ylim(c(-5,8))+
  theme_bw()+
  labs(title="Open minus Closed")

f2.diff <- ggplot(flux, aes(x=date_time))+
  geom_line(aes(y=abs(fc_wpl_adjust_open)-abs(co2_flux_closed)), linewidth=0.2, color="black")+
  ylim(c(-5,8))+
  theme_bw()+
  labs(title="Open adjusted minus Closed")

plot_grid(f1.diff,f2.diff,nrow=2)


## graph the fluxes against each other
ggplot(flux,
       aes(co2_flux_closed,co2_flux_open))+
  geom_point(size=0.2)+
  ylim(c(-10,10))+
  geom_abline(intercept=0,slope=1)+
  geom_smooth(method="lm")+
  theme_bw()

## graph the closed path and provisionally corrected flux
ggplot(flux,
       aes(co2_flux_closed,fc_wpl_adjust_open))+
  geom_point(size=0.2)+
  ylim(c(-10,10))+
  geom_abline(intercept=0,slope=1)+
  geom_smooth(method="lm")+
  theme_bw()

# compare co2 molar density
ggplot(flux,
       aes(co2_molar_density_closed,co2_molar_density_open))+
  geom_point(size=0.2)+
  geom_abline(intercept=0,slope=1)

# compare molar density by month and graph with AGC
plot_grid(ggplot(flux,
       aes(co2_molar_density_closed,co2_molar_density_open))+
  geom_point(size=0.2)+
  geom_abline(intercept=0,slope=1)+
  facet_grid(.~month(date_time)),


ggplot(flux, aes(x=date_time))+
  geom_point(aes(y=agc_mean_open, colour="Open path"), size=0.4)+
  geom_point(aes(y=agc_mean_closed, colour="Closed path"), size=0.4)+
  facet_grid(.~month(date_time), scales="free_x")+
  theme(legend.position="bottom"),
nrow=2,
align="v")



# compare co2 mole fraction
ggplot(flux,
       aes(co2_mole_fraction_closed,co2_mole_fraction_open))+
  geom_point(size=0.2)+
  geom_abline(intercept=0,slope=1)

# compare air density
ggplot(flux,
       aes(air_density_closed,air_density_open))+
  geom_point(size=0.2)+
  geom_abline(intercept=0,slope=1)


# compare h2o molar density
ggplot(flux,
       aes(h2o_molar_density_closed,h2o_molar_density_open))+
  geom_point(size=0.2)+
  geom_abline(intercept=0,slope=1)

# graph the time-series of CO2 molar density
ggplot(flux, aes(x=date_time))+
  geom_point(aes(y=co2_molar_density_open), size=0.2, color="black")+
  geom_point(aes(y=co2_molar_density_closed), size=0.2, color="green")

# graph the time-series of CO2 mole fraction
ggplot(flux, aes(x=date_time))+
  geom_point(aes(y=co2_mole_fraction_open), size=0.2, color="black")+
  geom_point(aes(y=co2_mole_fraction_closed), size=0.2, color="green")

# graph the time-series of air density
ggplot(flux, aes(x=date_time))+
 geom_point(aes(y=air_density_open), size=0.2, color="black")+
  geom_point(aes(y=air_density_closed), size=0.2, color="green")

# graph the time-series of H2O molar density
ggplot(flux, aes(x=date_time))+
  geom_point(aes(y=h2o_molar_density_open), size=0.2, color="black")+
  geom_point(aes(y=h2o_molar_density_closed), size=0.2, color="green")


## graph the co2 fluxes against each other
fig.co2.compare <- ggplot(flux,
       aes(co2_flux_closed,co2_flux_open))+
  geom_point(size=0.5)+
  ylim(c(-10,10))+
  xlim(c(-10,10))+
  geom_abline(intercept=0,slope=1, color="dark grey")+
  theme_bw()

## graph the LE fluxes against each other
fig.le.compare <- ggplot(flux,
       aes(LE_closed,LE_open))+
  geom_point(size=0.5)+
  geom_abline(intercept=0,slope=1, color="dark grey")+
  theme_bw()

## graph the H fluxes against each other
fig.h.compare <- ggplot(flux,
       aes(H_closed,H_open))+
  geom_point(size=0.5)+
  geom_abline(intercept=0,slope=1, color="dark grey")+
  theme_bw()


# compare co2 molar density
fig.co2.moldens.compare <- ggplot(flux,
       aes(co2_molar_density_closed,co2_molar_density_open))+
  geom_point(size=0.2)+
  geom_abline(intercept=0,slope=1, color="dark grey")+
  theme_bw()

# compare h2o molar density
fig.h2o.moldens.compare <- ggplot(flux,
       aes(h2o_molar_density_closed,h2o_molar_density_open))+
  geom_point(size=0.2)+
  geom_abline(intercept=0,slope=1)+
  theme_bw()

# graph the covariances against each other
fig.cov.compare <- ggplot(flux,
       aes(`w/co2_cov_closed`,`w/co2_cov_open`))+
  geom_point(size=0.5)+
  ylim(c(-0.1,0.1))+
 xlim(c(-0.1,0.1))+
  geom_abline(intercept=0,slope=1, color="dark grey")+
  theme_bw()

plot_grid(fig.co2.compare, fig.cov.compare, fig.le.compare, fig.h.compare)
plot_grid(fig.co2.moldens.compare, fig.h2o.moldens.compare)

# co-plot time-series
# graph the time-series of CO2 flux together

# define manual color scale
cols3 <- c("Open path" = "black", "Closed path" = "#fdbb84", "Open path adj" = "#31a354")
cols2 <- c("Open path" = "black", "Closed path" = "#fdbb84")

fig.time.co2 <- ggplot()+
  geom_line(aes(x=date_time,y=co2_flux_open,colour="Open path"), data = flux , size=0.3)+
 # geom_point(aes(x=date_time,y=co2_flux_open,colour="Open path"), data = flux , size=0.1)+
  geom_line(aes(x=date_time, y=co2_flux_closed,colour="Closed path"), data=flux , size=0.3)+
  #geom_point(aes(x=date_time, y=co2_flux_closed,colour="Closed path"), data=flux , size=0.1)+
  geom_line(aes(x=date_time, y=fc_wpl_adjust_open,colour="Open path adj"), data=flux , size=0.3)+
 # geom_point(aes(x=date_time, y=fc_wpl_adjust_open,colour="Open path adj"), data=flux , size=0.)+
  ylim(c(-10,10))+
  theme_bw()+
  scale_color_manual(values=cols3, breaks=c("Open path","Closed path","Open path adj"))+
  labs(y="CO2 flux (umol/m2/s)", x="Date")

# graph covariance time-series together
fig.time.cov <- ggplot()+
  geom_line(aes(x=date_time,y=`w/co2_cov_open`,colour="Open path"), data = flux, size=0.2)+
  geom_line(aes(x=date_time, y=`w/co2_cov_closed`,colour="Closed path"), data=flux, size=0.2)+
  ylim(c(-0.2,0.2))+
  theme_bw()+
  scale_color_manual(values=cols2, breaks=c("Open path","Closed path")) +
  labs(title="w/CO2 Time-Series", y="w/co2 covariance")


# graph open corrected and closed w/co2 covariance
ggplot()+
  geom_line(aes(x=date_time,y=wco2_adjust_open,colour="Open path adj"), data = flux, size=0.2)+
  geom_line(aes(x=date_time, y=`w/co2_cov_closed`,colour="Closed path"), data=flux, size=0.2)+
  ylim(c(-0.2,0.2))+
  theme_bw()+
  scale_color_manual(values=c("Closed path" = "#7fcdbb", "Open path adj" = "#fdae6b"),
                     breaks=c("Closed path","Open path adj"))

# graph the spectral correction and wpl corrections to compare magnitude
fig.time.scf <- ggplot(flux, aes(x=date_time))+
  geom_point(aes(y=co2_scf_open, colour="Open path"), size=0.4)+
  geom_point(aes(y=co2_scf_closed, colour="Closed path"), size=0.4)+
  scale_color_manual(values=cols2, breaks=c("Open path","Closed path"))+
  theme_bw()+
  labs(title="Spectral Correction Factors", y="SCF")


fig.time.wpl <- ggplot(flux, aes(x=date_time))+
  geom_point(aes(y=(corrc1a_open+corrc2a_open)/(44.01/1e6), colour="Open WPL"), size=0.4)+
  geom_point(aes(y=wco2_open, colour="Open wco2"), size=0.4)+
  theme_bw()+
  labs(title="Open Path W/CO2 covariance and WPL correction", y="covariance and WPL")

# graph SCF, w/co2, and WPL together
plot_grid(fig.time.scf+theme(legend.position="bottom"),
          fig.time.cov+theme(legend.position="bottom")+ylim(c(-0.2,0.3)),
          fig.time.wpl+theme(legend.position="bottom")+ylim(c(-0.2,0.3)),
          ggplot()+
            geom_density(aes(`w/co2_cov_open`, fill="Open path"), data=flux,alpha=0.5)+
            geom_density(aes(`w/co2_cov_closed`, fill="Closed path"), data = flux,alpha=0.5)+
            theme_bw()+
            ylim(c(0,200))+
            scale_fill_manual(values=cols2, breaks=c("Open path","Closed path"))+
            labs(title="w/CO2 density")+
            theme(legend.position="bottom"),
          align="hv")

# graph the time-series of CO2 molar density
fig.time.co2.moldens <- ggplot(flux, aes(x=date_time))+
geom_line(aes(y=co2_molar_density_open, color="Open path"), data = flux, size=0.2)+
  geom_line(aes(y=co2_molar_density_closed, colour="Closed path"), data=flux, size=0.2)+
  geom_point(aes(y=co2_molar_density_open, color="Open path"), data = flux, size=0.2)+
  geom_point(aes(y=co2_molar_density_closed, color="Closed path"), data=flux, size=0.2)+
  theme_bw()+
  scale_color_manual(values=cols2, breaks=c("Open path","Closed path"))+
  labs(y="CO2 molar density", x="Date")


# graph the time-series of CO2 molar density
fig.time.h2o.moldens <- ggplot(flux, aes(x=date_time))+
 geom_line(aes(y=h2o_molar_density_open, color="Open path"), data = flux, size=0.2)+
  geom_line(aes(y=h2o_molar_density_closed, colour="Closed path"), data=flux, size=0.2)+
  geom_point(aes(y=h2o_molar_density_open, color="Open path"), data = flux, size=0.2)+
  geom_point(aes(y=h2o_molar_density_closed, color="Closed path"), data=flux, size=0.2)+
  theme_bw()+
  scale_color_manual(values=cols2, breaks=c("Open path","Closed path"))+
  labs(y="H2O molar density", x="Date")


# graph H time-series together
fig.time.h <- ggplot()+
geom_line(aes(x=date_time,y=H_open, color="Open path"), data = flux, size=0.2)+
  geom_line(aes(x=date_time, y=H_closed, colour="Closed path"), data=flux, size=0.2)+
  geom_point(aes(x=date_time,y=H_open, color="Open path"), data = flux, size=0.2)+
  geom_point(aes(x=date_time, y=H_closed, color="Closed path"), data=flux, size=0.2)+
  theme_bw()+
  scale_color_manual(values=cols2, breaks=c("Open path","Closed path"))+
  labs(y="Sensible Heat Flux (W/m2)", x="Date")


# graph LE time-series together
fig.time.le <- ggplot()+
  geom_line(aes(x=date_time,y=LE_open, color="Open path"), data = flux, size=0.2)+
  geom_line(aes(x=date_time, y=LE_closed, colour="Closed path"), data=flux, size=0.2)+
  geom_point(aes(x=date_time,y=LE_open, color="Open path"), data = flux, size=0.2)+
  geom_point(aes(x=date_time, y=LE_closed, color="Closed path"), data=flux, size=0.2)+
  theme_bw()+
  scale_color_manual(values=cols2, breaks=c("Open path","Closed path"))+
  labs(y="Latent Energy (W/m2)", x="Date")



plot_grid(fig.time.co2+theme(legend.position="none"),
          fig.time.co2.moldens+theme(legend.position="none"),
          fig.time.le+theme(legend.position="none"),
          fig.time.h+theme(legend.position="none"),
          align="hv")

plot_grid(fig.time.co2+ylim(-5,5), fig.time.le, nrow=2)

plot_grid(fig.time.co2.moldens, fig.time.h2o.moldens, nrow=2)

# graph diurnal Co2 fluxes
flux[,':='(month = month(date_time),
           hour=hour(date_time))]
flux.diurn <- flux[,.(fc_open = mean(co2_flux_open, na.rm=TRUE),
                      fc_adj_open = mean(fc_wpl_adjust_open, na.rm=TRUE),
                      fc_closed = mean(co2_flux_closed, na.rm=TRUE),
                      scf_open = mean(co2_scf_open, na.rm=TRUE),
                      scf_closed = mean(co2_scf_closed, na.rm=TRUE),
                      wpl_open = mean((corrc1a_open+corrc2a_open)/(44.01/1e6), na.rm=TRUE),
                      wco2_open = mean(wco2_open, na.rm=TRUE),
                      fc_open_sd = sd(co2_flux_open, na.rm=TRUE),
                      fc_adj_open_sd = sd(fc_wpl_adjust_open, na.rm=TRUE),
                      fc_closed_sd = sd(co2_flux_closed, na.rm=TRUE), 
                      scf_open_sd = sd(co2_scf_open, na.rm=TRUE),
                      scf_closed_sd = sd(co2_scf_closed, na.rm=TRUE),
                      wpl_open_sd = sd((corrc1a_open+corrc2a_open)/(44.01/1e6), na.rm=TRUE),
                      wco2_open_sd = sd(wco2_open, na.rm=TRUE)),
                   by="year,month,hour"]
# graph diurnal fluxes
ggplot(flux.diurn, aes(x=hour))+
  geom_line(aes(y=fc_open, colour="Open path"), linewidth=0.3)+
  geom_point(aes(y=fc_open, colour="Open path") , size=0.3)+
  geom_errorbar(aes(ymin=fc_open-fc_open_sd,ymax=fc_open+fc_open_sd, colour="Open path") , size=0.2, width=0.1)+
  geom_line(aes(y=fc_closed, colour="Closed path") , linewidth=0.3)+
  geom_point(aes(y=fc_closed, colour="Closed path") , size=0.3)+
  geom_errorbar(aes(ymin=fc_closed-fc_closed_sd,ymax=fc_closed+fc_closed_sd, colour="Closed path") , size=0.2, width=0.1)+
  geom_line(aes(y=fc_adj_open, colour="Open path adj"), linewidth=0.3)+
  geom_point(aes(y=fc_adj_open, colour="Open path adj"),  size=0.3)+
  geom_errorbar(aes(ymin=fc_adj_open-fc_adj_open_sd,ymax=fc_adj_open+fc_adj_open_sd, colour="Open path adj") , size=0.2, width=0.1)+
  #ylim(c(-10,10))+
  theme_bw()+
  facet_wrap(year~month)+
  scale_color_manual(values=cols3, breaks=c("Open path","Closed path","Open path adj"), )+
  labs(y="Mean Hourly Flux Rate (umol/m2/s)", x="Hour")

# graph diurnal scf
ggplot(flux.diurn, aes(x=hour))+
  geom_line(aes(y=scf_open, colour="Open path"), linewidth=0.3)+
  geom_point(aes(y=scf_open, colour="Open path") , size=0.3)+
  geom_errorbar(aes(ymin=scf_open-scf_open_sd,ymax=scf_open+scf_open_sd, colour="Open path") , size=0.2, width=0.1)+
  geom_line(aes(y=scf_closed, colour="Closed path") , linewidth=0.3)+
  geom_point(aes(y=scf_closed, colour="Closed path") , size=0.3)+
  geom_errorbar(aes(ymin=scf_closed-scf_closed_sd,ymax=scf_closed+scf_closed_sd, colour="Closed path") , size=0.2, width=0.1)+
   #ylim(c(-10,10))+
  theme_bw()+
  facet_wrap(year~month)+
  scale_color_manual(values=cols2, breaks=c("Open path","Closed path"), )+
  labs(y="Mean Hourly SCF", x="Hour")

# graph diurnal wpl with wco2
ggplot(flux.diurn, aes(x=hour))+
  geom_line(aes(y=wpl_open, colour="Open WPL"), linewidth=0.3)+
  geom_point(aes(y=wpl_open, colour="Open WPL") , size=0.3)+
  geom_errorbar(aes(ymin=wpl_open-wpl_open_sd,ymax=wpl_open+wpl_open_sd, colour="Open WPL") , size=0.2, width=0.1)+
  geom_line(aes(y=wco2_open, colour="Open w/co2"), linewidth=0.3)+
  geom_point(aes(y=wco2_open, colour="Open w/co2") , size=0.3)+
  geom_errorbar(aes(ymin=wco2_open-wco2_open_sd,ymax=wco2_open+wco2_open_sd, colour="Open w/co2") , size=0.2, width=0.1)+
  #ylim(c(-10,10))+
  theme_bw()+
  facet_wrap(year~month)+
 # scale_color_manual(values=cols2, breaks=c("Open path","Closed path") )+
  labs(y="Mean Hourly WPL and w/co2", x="Hour")

# calculate the daily fluxes from open, open adjusted, and closed
flux.daily <- flux[,.(fc_open = mean(co2_flux_open, na.rm=TRUE),
                      fc_adj_open = mean(fc_wpl_adjust_open, na.rm=TRUE),
                      fc_closed = mean(co2_flux_closed, na.rm=TRUE),
                      fc_open_sd = sd(co2_flux_open, na.rm=TRUE),
                      fc_adj_open_sd = sd(fc_wpl_adjust_open, na.rm=TRUE),
                      fc_closed_sd = sd(co2_flux_closed, na.rm=TRUE),
                      precip = sum(P_rain_1_1_1_open)),
                      by="date"]

# graph daily
fig.flux.daily <- ggplot(flux.daily, aes(x=date))+
  geom_line(aes(y=fc_open, colour="Open path"),linewidth=0.4)+
  #geom_errorbar(aes(ymin=fc_open-fc_open_sd,ymax=fc_open+fc_open_sd,colour="Open path") , size=0.2, width=0.1)+
  geom_line(aes(y=fc_adj_open, colour="Open path adj"),linewidth=0.4)+
 #geom_errorbar(aes(ymin=fc_adj_open-fc_adj_open_sd,ymax=fc_adj_open+fc_adj_open_sd, colour="Open path adj") , size=0.2, width=0.1)+
  geom_line(aes(y=fc_closed, colour="Closed path"),linewidth=0.4)+
  #geom_errorbar(aes(ymin=fc_closed-fc_closed_sd,ymax=fc_closed+fc_closed_sd,colour="Closed path") , size=0.2, width=0.1)+
  theme_bw()+
  facet_grid(.~year(date), scales="free_x")+
  scale_color_manual(values=cols3, breaks=c("Open path","Closed path","Open path adj"))+
  labs(y="Mean Daily Flux Rate (umol/m2/s)", x="Date")

# graph daily rainfall
fig.rain.daily <- ggplot(flux.daily, aes(x=date))+
  geom_col(aes(y=precip, fill="Daily Rainfall"))+
  #geom_errorbar(aes(ymin=fc_open-fc_open_sd,ymax=fc_open+fc_open_sd,colour="Open path") , size=0.2, width=0.1)+
   theme_bw()+
  facet_grid(.~year(date), scales="free_x")+
  scale_fill_manual(values=c("blue"))+
  labs(y="Daily total Rainfall (mm)", x="Date")


# graph daily flux with daily rain
plot_grid(fig.flux.daily+theme(axis.text.x = element_blank(),axis.title.x = element_blank()), 
          fig.rain.daily,
          nrow=2,
          align="v")


#
# # from James: scf and wpl corrections
# wco2 = mat2(:,173);  % covariance from EddyPro File
# wh2o = mat2(:,174);  % covariance from EddyPro File
# wts  = mat2(:,172);  % covariance from EddyPro File
#
# scfc = mat2(:,106);   % spectral correction factors from EddyPro File for co2
# scfh = mat2(:,104);   % spectral correction factors from EddyPro File for h2o
# scft = mat2(:,102);   % spectral correction factors from EddyPro File for wts

# % corrected covariance terms
# wco2 = wco2.*scfc;
# wq   = wh2o.*scfh;
# wT   = wts.* scft;

# co2d        = mat2(:,39);  % co2 molar density from EP file
# h2od        = mat2(:,44);  % h2o molar density from EP file
# 
# dena        =   mat2(:,62); % density of air in kg/m3o
# rho_a       =   dena;

# cp          =   mat2(:,63); % specific heat capacity of air from Eddypro file
# tair        =   mat2(:,60)-273.15;   % tair in celcius from EP file
# mu          =   29.002 / 18.02;
# rho_q       =   h2od.*(18.02/1e6); = rho_h
# sigma       =   rho_q ./ rho_a;
# rho_c       = co2d*(44.01/1e6); % in kg/m3
# rho_h       = h2od*(18.02/1e6); % in kg/m3
#
# corrh1a       =   mu.*(rho_h./rho_a).*(wq*(18.02/1e6))  ;
# corrh2a       =   rho_h.*(1+mu.*sigma).*(wT./(tair+273.15));
# 
# lambda        =   (2.501 - 0.00237*tair)*1000000; % latent heat of vaporization
# wpl_h2oa      =   corrh1a+corrh2a; % wpl term

# le_wpl        =   ((wq.*(0.01802/1000))+(corrh1a+corrh2a)).*lambda;  % le in Wm-2
# 
# corrc1a       =   mu.*(rho_c./rho_a).*((wq*(18.02/1e6)))  ;  % WPL term1
# 
# corrc2a       =   rho_c.*(1+mu.*sigma).*(wT./(tair+273.15)); % WPL term2 
# 
# fc_wpl        =  1000.*( wco2 + ((corrc1a+corrc2a)./(44.01/1e6)));  % converting back to umolm-2s-1

# implement scf and wpl correction for co2 and LE (LE ust to test process)
mu <- 29.002 / 18.02
flux[,':=' (wco2 = `w/co2_cov`*co2_scf,
              wco2_adjust = (`w/co2_cov`*0.9)*co2_scf,
              wT = `w/ts_cov`*H_scf,
              wq = `w/h2o_cov`*LE_scf,
              cp = air_heat_capacity,
              rho_a = air_density,
              tair = air_temperature-273.15,
              rho_c = co2_molar_density*(44.01/1e6),
              rho_q = h2o_molar_density*(18.02/1e6),
              rho_h = h2o_molar_density*(18.02/1e6))][
  ,':=' (sigma = rho_q/rho_a,
         lambda = (2.501 - 0.00237*tair)*1000000)]

flux[,':=' (corrc1a =mu*(rho_c/rho_a)*((wq*(18.02/1e6))), 
              corrc2a = rho_c*(1+mu*sigma)*(wT/(tair+273.15)),
              corrh1a = mu*(rho_h/rho_a)*(wq*(18.02/1e6)),
               corrh2a =  rho_h*(1+mu*sigma)*(wT/(tair+273.15)))]

flux[,':=' (fc_wpl = 1000* (wco2 + ((corrc1a+corrc2a)/(44.01/1e6))),
              fc_wpl_adjust = 1000* (wco2_adjust + ((corrc1a+corrc2a)/(44.01/1e6))),
              wpl_h2oa = corrh1a+corrh2a,
              LE_wpl = ((wq*(0.01802/1000))+(corrh1a+corrh2a))*lambda)]
  
# graph corrected 
# EddyPro corrected CO2 flux (black) and re-calculated CO2 flux (red)
ggplot(flux) +
  geom_point(aes(date, co2_flux), color="black",size=0.3)+
  geom_point(aes(date, fc_wpl),color="red", size=0.1)+
  ylim(-30,10)+
  facet_grid(.~year, scales="free_x")+
  labs(title="James: EddyPro corrected CO2 flux (black) and re-calculated CO2 flux (red)")

# and in 1;1
ggplot(flux) +
  geom_point(aes(co2_flux,fc_wpl),size=0.3)+
  geom_abline(intercept=0,slope=1)+
  labs(title="James: EddyPro corrected and re-calculated CO2 flux")+
  xlim(c(-200,400))+
  ylim(c(-200,400))

# look at offset corrected CO2 flux (sensu Scott et al 2015)
ggplot(flux) +
  geom_point(aes(date, co2_flux), color="black",size=0.3)+
  geom_point(aes(date, fc_wpl_adjust),color="green", size=0.1)+
  ylim(-30,10)+
  facet_grid(.~year, scales="free_x")+
  labs(title="James: EddyPro corrected CO2 flux (black) and 10% adjusted CO2 flux (green)")

# LE
ggplot(flux) +
  geom_point(aes(date, LE), color="black",size=0.3)+
  geom_point(aes(date, LE_wpl),color="blue", size=0.1)+
  facet_grid(.~year, scales="free_x")+
  labs(title="James: EddyPro corrected LE flux (black) and re-calculated LE flux (red)")

# and in 1;1
ggplot(flux) +
  geom_point(aes(LE,LE_wpl),size=0.3)+
  geom_abline(intercept=0,slope=1)+
  labs(title="James: EddyPro corrected and re-calculated CO2 flux")


# Save corrected full output for filtering and ReddyProc
save(flux,file="~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_CovarianceCorrect_Scott2015/JER_flux_EddyPro_FullOutput_Scott2015_Correct_20230112.RData")
