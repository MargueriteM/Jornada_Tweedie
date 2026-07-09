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
library(dplyr)
library(REddyProc)
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

# Cove: 12 July 2024 & Russ agrees!
# Can we get to cause of why instruments disagree?
# Is it self-heating?
# Could it be under-estimate of heat-flux that causes open path CO2 to be over-estimated? 

# Self-heating: 
# apply Burba correction to fluxes, what's the effect?
# what magnitude of self-heating is needed to produce this offset?

# Under-estimated heat-flux:
# calculate energy balance closure and correct EC-based H to close EB
# apply the inflated H to the WPL correction and see what it does to open-path fluxes

# also double check EddyPro processing for closed path. Based on molar ratio?
# 7200 is measuring mole fraction of co2 and h2o
# also cell temp and pressure which allows mixing ratios to be calculated and no wpl is needed. 
# https://www.licor.com/env/support/EddyPro/topics/converting-to-mixing-ratio.html 


# closed path
fileinfo_closed <- scan("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Bahada/Tower/ts_data_2/2023/EddyPro_Out/ClosedPath/JER_flux_2023_EddyPro_FullOutput_filterSD_JuneDec_Closed.csv",
                      what='',sep=",",nlines=1)
fileinfo_closed <- data.table(t(fileinfo_closed))
# read only the first row to get the units
# flux.units.closed <- (fread("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Bahada/Tower/ts_data_2/2023/EddyPro_Out/ClosedPath/JER_flux_2023_EddyPro_FullOutput_filterSD_JuneDec_Closed.csv",header=TRUE))[1,]


# read the data, skippping the units row
# open path
# Oct 2022 to May 2024
load("/Users/memauritz/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_EddyPro_filtered/JER_flux_202210_202405_EddyPro_FullOutput_filterSD_20240623.Rdata")
flux_open1 <- flux_filter_sd
rm(flux_filter_sd)

# Jun 2024 to Dec 2025
load("/Users/memauritz/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_EddyPro_filtered/JER_flux_202406_202512_EddyPro_FullOutput_filterSD_20260417.Rdata")
flux_open2 <- flux_filter_sd
rm(flux_filter_sd)

# combine and remove individual
flux_open <- rbind(flux_open1,flux_open2)
rm(flux_open1,flux_open2)

# closed path
flux_closed2023 <- fread("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Bahada/Tower/ts_data_2/2023/EddyPro_Out/ClosedPath/JER_flux_2023_EddyPro_FullOutput_filterSD_JuneDec_Closed.csv", sep=",",
                   header=TRUE, na.strings=c("-9999","-9999.0","NAN","#NAME?","NA"))

flux_closed2024 <- fread("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Bahada/Tower/ts_data_2/2024/EddyPro_Out/ClosedPath/JER_flux_2024_EddyPro_FullOutput_filterSD_JanMay_Closed.csv", sep=",",
                         header=TRUE, na.strings=c("-9999","-9999.0","NAN","#NAME?","NA"))

flux_closed2024a <- fread("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Bahada/Tower/ts_data_2/2024/EddyPro_Out/ClosedPath/JER_flux_2024_EddyPro_FullOutput_filterSD_JunNov_Closed.csv", sep=",",
                         header=TRUE, na.strings=c("-9999","-9999.0","NAN","#NAME?","NA"))

flux_closed2025 <- fread("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Bahada/CR3000/L3/Eddy_Covariance_ts_2/ClosedPath/JER_flux_2025_EddyPro_FullOutput_filterSD_JanDec_Closed.csv", sep=",",
                          header=TRUE, na.strings=c("-9999","-9999.0","NAN","#NAME?","NA"))

# combine closed path
flux_closed <- rbind(flux_closed2023,flux_closed2024, flux_closed2024a,flux_closed2025)
# remove individual
rm(flux_closed2023,flux_closed2024, flux_closed2024a,flux_closed2025)

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
            year=year(date_time),
            month=month(date_time))]

# drop the biomet columns for _closed using .SDcols
# check correct columns
colnames(flux[,442:460])
# create list to drop
cols_to_drop <- colnames(flux[,442:460])
flux[, (cols_to_drop) := NULL]

# add a descriptor for when the open path measurments are source or sink
flux[,co2_flux_open_sign := fifelse(co2_flux_open>=0,"source","sink")]
flux[,fc_wpl_adjust_open_sign := fifelse(fc_wpl_adjust_open>=0,"source","sink")]

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
  geom_point(aes(y=(co2_flux_open)-(co2_flux_closed),color=co2_flux_open_sign), size=0.2)+
  ylim(c(-5,8))+
  theme_bw()+
  labs(title="Open minus Closed")

f2.diff <- ggplot(flux, aes(x=date_time))+
  geom_point(aes(y=(fc_wpl_adjust_open)-(co2_flux_closed),color=fc_wpl_adjust_open_sign), size=0.2)+
  ylim(c(-5,8))+
  theme_bw()+
  labs(title="Open adjusted minus Closed")

plot_grid(f1.diff + theme(legend.position = "none"),
          f2.diff+ theme(legend.position = "bottom"),
          nrow=2)

# plot as histogram
plot_grid(
 ggplot(flux)+
    geom_density(aes((co2_flux_open)-(co2_flux_closed),fill=co2_flux_open_sign),alpha=0.2)+
    #ylim(c(-5,8))+
    theme_bw()+
   theme(legend.position = "bottom")+
    labs(title="Open minus Closed"),
  
  ggplot(flux)+
    geom_density(aes((fc_wpl_adjust_open)-(co2_flux_closed),fill=fc_wpl_adjust_open_sign), alpha=0.2)+
    #ylim(c(-5,8))+
    theme_bw()+
   theme(legend.position = "bottom")+
    labs(title="Open adjusted minus Closed"),
  ncol=2
)

# open  vs closed by month
ggplot(flux)+
  geom_density(aes((co2_flux_open)-(co2_flux_closed),fill=co2_flux_open_sign), alpha=0.2)+
  geom_vline(xintercept=0,linetype="dotted")+
  theme_bw()+
  theme(legend.position = "bottom")+
  facet_wrap(year+month~.)+
  labs(title="Open  minus Closed")

# open adjusted vs closed by month
ggplot(flux)+
  geom_density(aes((fc_wpl_adjust_open)-(co2_flux_closed),fill=fc_wpl_adjust_open_sign), alpha=0.2)+
  geom_vline(xintercept=0,linetype="dotted")+
  theme_bw()+
  theme(legend.position = "bottom")+
  facet_wrap(year+month~.)+
  labs(title="Open adjusted minus Closed")

# and as 1:1 with quadrants
plot_grid(
  ggplot(flux)+
     geom_point(aes(x=(co2_flux_open),y=(co2_flux_closed),color=co2_flux_open_sign), size=0.2)+
     theme_bw()+
     labs(title="Open vs Closed")+geom_hline(yintercept=0)+geom_vline(xintercept=0),
  ggplot(flux)+
    geom_point(aes(x=(fc_wpl_adjust_open),y=(co2_flux_closed),color=fc_wpl_adjust_open_sign), size=0.2)+
    theme_bw()+
    labs(title="Open adjust vs Closed")+geom_hline(yintercept=0)+geom_vline(xintercept=0),
  ncol=2)
  

# compare the difference by H and day/night
ggplot(flux[!is.na(daytime_open)], aes(x=H_open))+
  geom_point(aes(y=(co2_flux_open)-(co2_flux_closed),color=factor(daytime_open)), size=0.1)+
  ylim(c(-5,8))+
  theme_bw()+
  labs(title="Open minus Closed vs H")+
  facet_grid(daytime_open~.)

# compare the difference by u* and day/night
ggplot(flux[!is.na(daytime_open)], aes(x=`u*_open`))+
  geom_point(aes(y=(co2_flux_open)-(co2_flux_closed),color=co2_flux_open_sign), size=0.1)+
  ylim(c(-5,8))+
  theme_bw()+
  labs(title="Open minus Closed vs H")+
  facet_grid(daytime_open~.)

# compare the difference by air density and day/night
ggplot(flux[!is.na(daytime_open)], aes(x=rho_a_open))+
  geom_point(aes(y=(co2_flux_open)-(co2_flux_closed),color=co2_flux_open_sign), size=0.1)+
  ylim(c(-5,8))+
  theme_bw()+
  labs(title="Open minus Closed vs H")+
  facet_grid(daytime_open~.)

## graph the fluxes against each other by month and year
ggplot(flux,
       aes(co2_flux_closed,co2_flux_open))+
  geom_point(size=0.2)+
  ylim(c(-10,10))+
  geom_abline(intercept=0,slope=1)+
  geom_smooth(method="lm")+
  theme_bw()+
  facet_grid(year(date_time)~month(date_time))

## graph the closed path and provisionally corrected flux
ggplot(flux,
       aes(co2_flux_closed,fc_wpl_adjust_open))+
  geom_point(size=0.2)+
  ylim(c(-10,10))+
  geom_abline(intercept=0,slope=1)+
  geom_smooth(method="lm")+
  theme_bw()+
  facet_grid(year(date_time)~month(date_time))

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

# graph the time-series of H2O molar density
ggplot(flux, aes(x=date_time))+
  geom_point(aes(y=h2o_mole_fraction_open), size=0.2, color="black")+
  geom_point(aes(y=h2o_mole_fraction_closed), size=0.2, color="green")


## graph the co2 fluxes against each other
fig.co2.compare <- ggplot(flux,
       aes(co2_flux_closed,co2_flux_open))+
  geom_point(size=0.5)+
  ylim(c(-10,10))+
  xlim(c(-10,10))+
  geom_abline(intercept=0,slope=1, color="dark grey")+
  theme_bw()+
  facet_grid(year(date_time)~month(date_time))

## graph the co2 fluxes against each other by month and year
# with total monthly rainfall
rain_sum <- flux %>%
  group_by(year = year(date_time), month = month(date_time)) %>%
  summarise(total_rain = sum(P_rain_1_1_1_open, na.rm = TRUE), .groups = "drop")


ggplot(flux,
    aes(co2_flux_closed,co2_flux_open))+
  geom_point(size=0.5)+
  geom_smooth(method="lm")+
  ylim(c(-10,10))+
  xlim(c(-10,10))+
  geom_abline(intercept=0,slope=1, color="dark grey")+
  theme_bw()+
  facet_grid(year~month)+
  
  geom_text(
    data = rain_sum,
    aes(x = -1, y = 8, label = paste0("Rain (mm):", round(total_rain, 1))),
    inherit.aes = FALSE,
    size = 1.8
  )

# graph open and closed LE by month
ggplot(flux,
       aes(LE_closed,LE_open))+
  geom_point(size=0.5)+
  geom_smooth(method="lm")+
  ylim(c(-10,10))+
  xlim(c(-10,10))+
  geom_abline(intercept=0,slope=1, color="dark grey")+
  theme_bw()+
  facet_grid(year~month)+
  
  geom_text(
    data = rain_sum,
    aes(x = -1, y = 8, label = paste0("Rain (mm):", round(total_rain, 1))),
    inherit.aes = FALSE,
    size = 1.8
  )


# calculate the correlation between corrected and uncorrected
lm.closed.open.meas <- flux %>%
  group_by(year,month) %>%
  do(model = lm(co2_flux_open ~ co2_flux_closed, data = .))

lm.closed.openmeas.summary <- lm.closed.open.meas %>%
  summarise(
    year=year,
    month=month,
    intercept.meas = coef(model)[1],
    slope.meas = coef(model)[2],
    r2.meas=summary(model)$adj.r.squared
  )

## graph the closed path and provisionally corrected flux
ggplot(flux,
       aes(co2_flux_closed,fc_wpl_adjust_open))+
  geom_point(size=0.5)+
  geom_smooth(method="lm")+
  ylim(c(-10,10))+
  xlim(c(-10,10))+
  geom_abline(intercept=0,slope=1, color="dark grey")+
  theme_bw()+
  facet_grid(year~month)+
  
  geom_text(
    data = rain_sum,
    aes(x = -1, y = 8, label = paste0("Rain (mm):", round(total_rain, 1))),
    inherit.aes = FALSE,
    size = 1.8
  )

lm.closed.opencor <- flux %>%
  group_by(year,month) %>%
  do(model = lm(fc_wpl_adjust_open ~ co2_flux_closed, data = .))

lm.closed.opencor.summary <- lm.closed.opencor %>%
  summarise(
    year=year,
    month=month,
    intercept.cor = coef(model)[1],
    slope.cor = coef(model)[2],
    r2.cor=summary(model)$adj.r.squared
  )%>%
  left_join(lm.closed.openmeas.summary)%>%
  left_join(rain_sum)



# graph slope by rain
ggplot(lm.closed.opencor.summary, aes(month))+
  geom_point(aes(y=slope.cor))+
  geom_col(aes(y=total_rain/100))+
  geom_hline(yintercept=1)+
  facet_grid(year~.)

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
cols3_H <- c("Open path" = "black", "Closed path" = "#fdbb84", "Open path H correct" = "#31a354")
cols2 <- c("Open path" = "black", "Closed path" = "#fdbb84")

fig.time.co2 <- ggplot()+
  geom_line(aes(x=date_time,y=co2_flux_open,colour="Open path"), data = flux , linewidth=0.3)+
 # geom_point(aes(x=date_time,y=co2_flux_open,colour="Open path"), data = flux , size=0.1)+
  geom_line(aes(x=date_time, y=co2_flux_closed,colour="Closed path"), data=flux , linewidth=0.3)+
  #geom_point(aes(x=date_time, y=co2_flux_closed,colour="Closed path"), data=flux , size=0.1)+
 # geom_line(aes(x=date_time, y=fc_wpl_adjust_open,colour="Open path adj"), data=flux , size=0.3)+
 # geom_point(aes(x=date_time, y=fc_wpl_adjust_open,colour="Open path adj"), data=flux , size=0.)+
  ylim(c(-10,10))+
  theme_bw()+
  scale_color_manual(values=cols2, breaks=c("Open path","Closed path"), name="Sensor")+
  #scale_color_manual(values=cols3, breaks=c("Open path","Closed path","Open path adj"))+
  labs(y="CO2 flux (umol/m2/s)", x="Date")

# rainfall (with transparent background?)
fig.time.rain <-ggplot()+
     geom_line(aes(x=date_time,y=P_rain_1_1_1_open), data = flux , linewidth=0.3)+
     ylim(c(0,30))+
     theme_bw()+
     #scale_color_manual(values=cols2, breaks=c("Open path","Closed path"), name="Sensor")+
     #scale_color_manual(values=cols3, breaks=c("Open path","Closed path","Open path adj"))+
     labs(y="Rain mm", x="Date")+
  theme(panel.background = element_rect(fill = "transparent", color = NA), # Panel background
plot.background = element_rect(fill = "transparent", color = NA))  # Outer plot background


# graph flux and rainfall
plot_grid(fig.time.co2+theme(legend.position="none"),
          fig.time.rain+theme(legend.position="none"),
          nrow=2,
          align="v")



# graph covariance time-series together
fig.time.cov <- ggplot()+
  geom_line(aes(x=date_time,y=`w/co2_cov_open`,colour="Open path"), data = flux, linewidth=0.2)+
  geom_line(aes(x=date_time, y=`w/co2_cov_closed`,colour="Closed path"), data=flux, linewidth=0.2)+
  ylim(c(-0.2,0.2))+
  theme_bw()+
  scale_color_manual(values=cols2, breaks=c("Open path","Closed path")) +
  labs(title="w/CO2 Time-Series", y="w/co2 covariance")


# graph open corrected and closed w/co2 covariance
ggplot()+
  geom_line(aes(x=date_time,y=wco2_adjust_open,colour="Open path adj"), data = flux, linewidth=0.2)+
  geom_line(aes(x=date_time, y=`w/co2_cov_closed`,colour="Closed path"), data=flux, linewidth=0.2)+
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
  geom_line(aes(y=(corrc1a_open+corrc2a_open)/(44.01/1e6), colour="Open WPL"), linewidth =0.4)+
  geom_line(aes(y=wco2_open, colour="Open wco2"), linewidth=0.4)+
  geom_line(aes(y=co2_flux_open, colour="Open flux"), linewidth=0.4)+
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


# graph regression of flux and wpl
ggplot(flux)+
  geom_point(aes(x=co2_flux_open, y=(corrc1a_open+corrc2a_open)/(44.01/1e6)), size=0.4)+
  theme_bw()+
  labs(title="Open Path flux vs WPL correction", x="Open Path CO2 flux (umol/m2/s)", y="WPL")

# graph the time-series of CO2 molar density
fig.time.co2.moldens <- ggplot(flux, aes(x=date_time))+
geom_line(aes(y=co2_molar_density_open, color="Open path"), data = flux, linewidth=0.2)+
  geom_line(aes(y=co2_molar_density_closed, colour="Closed path"), data=flux, linewidth=0.2)+
  geom_point(aes(y=co2_molar_density_open, color="Open path"), data = flux, size=0.2)+
  geom_point(aes(y=co2_molar_density_closed, color="Closed path"), data=flux, size=0.2)+
  theme_bw()+
  scale_color_manual(values=cols2, breaks=c("Open path","Closed path"))+
  labs(y="CO2 molar density", x="Date")


# graph the time-series of H2O molar density
fig.time.h2o.moldens <- ggplot(flux, aes(x=date_time))+
 geom_line(aes(y=h2o_molar_density_open, color="Open path"), data = flux, linewidth=0.2)+
  geom_line(aes(y=h2o_molar_density_closed, colour="Closed path"), data=flux, linewidth=0.2)+
  geom_point(aes(y=h2o_molar_density_open, color="Open path"), data = flux, size=0.2)+
  geom_point(aes(y=h2o_molar_density_closed, color="Closed path"), data=flux, size=0.2)+
  theme_bw()+
  scale_color_manual(values=cols2, breaks=c("Open path","Closed path"))+
  labs(y="H2O molar density", x="Date")


# graph H time-series together
fig.time.h <- ggplot()+
geom_line(aes(x=date_time,y=H_open, color="Open path"), data = flux, linewidth=0.2)+
  geom_line(aes(x=date_time, y=H_closed, colour="Closed path"), data=flux, linewidth=0.2)+
  geom_point(aes(x=date_time,y=H_open, color="Open path"), data = flux, size=0.2)+
  geom_point(aes(x=date_time, y=H_closed, color="Closed path"), data=flux, size=0.2)+
  theme_bw()+
  scale_color_manual(values=cols2, breaks=c("Open path","Closed path"))+
  labs(y="Sensible Heat Flux (W/m2)", x="Date")


# graph LE time-series together
fig.time.le <- ggplot()+
  geom_line(aes(x=date_time,y=LE_open, color="Open path"), data = flux, linewidth=0.2)+
  geom_line(aes(x=date_time, y=LE_closed, colour="Closed path"), data=flux, linewidth=0.2)+
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
flux[,':='(hour=hour(date_time),
           min=minute(date_time))][,
             half.hour := hour+min/6]
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
                   by="year,month,half.hour"]
# graph diurnal fluxes
ggplot(flux.diurn, aes(x=half.hour))+
  geom_line(aes(y=fc_open, colour="Open path"), linewidth=0.3)+
  geom_point(aes(y=fc_open, colour="Open path") , size=0.3)+
  geom_errorbar(aes(ymin=fc_open-fc_open_sd,ymax=fc_open+fc_open_sd, colour="Open path") , linewidth=0.2, width=0.1)+
  geom_line(aes(y=fc_closed, colour="Closed path") , linewidth=0.3)+
  geom_point(aes(y=fc_closed, colour="Closed path") , size=0.3)+
  geom_errorbar(aes(ymin=fc_closed-fc_closed_sd,ymax=fc_closed+fc_closed_sd, colour="Closed path") , linewidth=0.2, width=0.1)+
  #geom_line(aes(y=fc_adj_open, colour="Open path adj"), linewidth=0.3)+
  #geom_point(aes(y=fc_adj_open, colour="Open path adj"),  size=0.3)+
  #geom_errorbar(aes(ymin=fc_adj_open-fc_adj_open_sd,ymax=fc_adj_open+fc_adj_open_sd, colour="Open path adj") , linewidth=0.2, width=0.1)+
  #ylim(c(-10,10))+
  theme_bw()+
  facet_wrap(year~month)+
  #scale_color_manual(values=cols3, breaks=c("Open path","Closed path","Open path adj"), )+
  scale_color_manual(values=cols2, breaks=c("Open path","Closed path"), )+
  labs(y="Mean Half-Hourly Flux Rate (umol/m2/s)", x="Hour")

# graph diurnal scf
ggplot(flux.diurn, aes(x=half.hour))+
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
ggplot(flux.diurn, aes(x=half.hour))+
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
                      LE_open =  mean(LE_open, na.rm=TRUE),
                      LE_open_sd =  mean(LE_open, na.rm=TRUE),
                      LE_closed =  mean(LE_closed, na.rm=TRUE),
                      LE_closed_sd =  mean(LE_closed, na.rm=TRUE),
                      H_open= mean(H_open, na.rm=TRUE),
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
  scale_fill_manual(values=c("blue"),name="")+
  labs(y="Daily total Rainfall (mm)", x="Date")


# graph daily flux with daily rain
plot_grid(fig.flux.daily+theme(axis.text.x = element_blank(),axis.title.x = element_blank()), 
          fig.rain.daily,
          nrow=2,
          align="v")

# calculate cumulative based on daily mean (place-holder for gap-filled data)
flux.daily[,year := year(date)][,":="(fc_open_cum = replace(fc_open, !is.na(fc_open),cumsum(na.omit(fc_open))) ,
                      fc_adj_open_cum = replace(fc_adj_open, !is.na(fc_adj_open),cumsum(na.omit(fc_adj_open))), 
                      fc_closed_cum = replace(fc_closed, !is.na(fc_closed),cumsum(na.omit(fc_closed))),
                      LE_open_cum = replace(LE_open, !is.na(LE_open),cumsum(na.omit(LE_open))),
                      LE_closed_cum = replace(LE_closed, !is.na(LE_closed),cumsum(na.omit(LE_closed))),
                      precip_cum = replace(precip, !is.na(precip),cumsum(na.omit(precip)))),
                   by=year]

# graph cumulatives
fig.flux.cum <- ggplot(flux.daily, aes(x=date))+
  geom_line(aes(y=fc_open_cum, colour="Open path"),linewidth=0.7)+
  geom_line(aes(y=fc_adj_open_cum, colour="Open path adj"),linewidth=0.7)+
  geom_line(aes(y=fc_closed_cum, colour="Closed path"),linewidth=0.7)+
  theme_bw()+
  facet_grid(.~year(date), scales="free_x")+
  scale_color_manual(values=cols3, breaks=c("Open path","Closed path","Open path adj"))+
  labs(y="~Cumulative Daily Flux Rate (umol/m2/s)", x="Date")

fig.le.cum <- ggplot(flux.daily, aes(x=date))+
  geom_line(aes(y=LE_open_cum, colour="Open path"),linewidth=0.7)+
  geom_line(aes(y=LE_closed_cum, colour="Closed path"),linewidth=0.7)+
  theme_bw()+
  facet_grid(.~year(date), scales="free_x")+
  scale_color_manual(values=cols2, breaks=c("Open path","Closed path","Open path adj"))+
  labs(y="~Cumulative Daily LE", x="Date")

fig.rain.cum <- ggplot(flux.daily, aes(x=date))+
  geom_line(aes(y=precip_cum, color="Daily Rainfall"))+
  theme_bw()+
  facet_grid(.~year(date), scales="free_x")+
  scale_color_manual(values=c("blue"),name="")+
  labs(y="Cumualtive Rainfall (mm)", x="Date")


# graph daily cumulative flux with daily rain
plot_grid(fig.flux.cum+theme(axis.text.x = element_blank(),axis.title.x = element_blank()), 
          fig.rain.cum,
          nrow=2,
          align="v")

# graph daily cumulative LE with daily rain
plot_grid(fig.le.cum+theme(axis.text.x = element_blank(),axis.title.x = element_blank()), 
          fig.rain.cum,
          nrow=2,
          align="v")


# look at energy balance components
# H+LE = Rn-(H+S)
# EasyFlux DL has equation for calculating S using soil surface temp and soil moisture

# calculate mean of SHF at height 1 and height 2
flux[,SHF_1_mean := as.list(colMeans(.SD[, c("SHF_1_1_1_open", "SHF_2_1_1_open")])),by=date_time]
flux[,SHF_2_mean := as.list(colMeans(.SD[, c("SHF_1_2_1_open", "SHF_2_2_1_open")])),by=date_time]

# compute H+LE and RN-SHF and Rn-SHF-LE (=H)
flux[,':=' (H_LE = H_open+LE_open,
            Rn_G = Rn_1_1_1_open-SHF_1_mean,
            Rn_G_LE = Rn_1_1_1_open-SHF_1_mean-LE_open)]

# graph Rn, H, LE, SHF
ggplot(flux, aes(x=date_time))+
  geom_point(aes(y=Rn_1_1_1_open), size=0.5)+
  geom_line(aes(y=Rn_1_1_1_open), linewidth=0.5)
  
ggplot(flux[date(date)>=as.Date("2023-08-01") & date(date)<=as.Date("2023-08-10"),], aes(x=date_time))+
  geom_point(aes(y=SHF_1_mean), size=0.5)+
  geom_line(aes(y=SHF_1_mean), linewidth=0.5)

ggplot(flux, aes(x=date_time))+
  geom_point(aes(y=SHF_2_mean), size=0.5)+
  geom_line(aes(y=SHF_2_mean), linewidth=0.5)


ggplot(flux, aes(x=date_time))+
  geom_point(aes(y=LE_open), size=0.5)+
  geom_line(aes(y=LE_open), linewidth=0.5)


ggplot(flux, aes(x=date_time))+
  geom_point(aes(y=H_open), size=0.5)+
  geom_line(aes(y=H_open), linewidth=0.5)



# relationship between measured H and H + missing H
# Filter by U*
#   
# Ratio creates potential for discontinuity at zero
# Add missing H from EB into the WPL correction
# graph residuals of EB
ggplot(flux)+
  geom_point(aes(x =date_time, y=Rn_1_1_1_open - SHF_1_mean - H_open - LE_open), size=0.5)+
  theme_bw()+
  geom_abline(intercept=0,slope=1, color="darkgrey")
  

# To calculate generic correction factor, try slope of:
#   X = H and Y = H + missing H

#   X = H and Y = H + missing H
ggplot(flux)+
  geom_point(aes(x =H_open, y=H_open+(Rn_1_1_1_open - SHF_1_mean - H_open - LE_open)), size=0.5)+
  theme_bw()+
  geom_abline(intercept=0,slope=1, color="darkgrey")

lm(H_open+(Rn_1_1_1_open - SHF_1_mean - H_open - LE_open) ~ H_open, data=flux)

# Call:
#   lm(formula = H_open + (Rn_1_1_1_open - SHF_1_mean - H_open - 
#                            LE_open) ~ H_open, data = flux)
# 
# Coefficients:
#   (Intercept)       H_open  
# -21.651        1.699  

# adding Jul 2024 - Dec 2025: 
# Coefficients:
#   (Intercept)       H_open  
# -22.581        1.705  


# graph H+LE vs Rn-G (relationship of turbulent EB with measured EB)
ggplot(flux)+
  geom_point(aes(x = H_open + LE_open, y=Rn_1_1_1_open - SHF_1_mean), size=0.5)+
  geom_abline(intercept=0,slope=1, color="darkgrey")+
  theme_bw()

# regression
lm((Rn_G) ~ (H_LE), data=flux)

# Call:
#   lm(formula = (Rn_G) ~ (H_LE), data = flux)
# 
# Coefficients:
#   (Intercept)         H_LE  
# -27.918        1.657  


# graph EB ratio
ggplot(flux)+
  geom_point(aes(x =date_time, y = (H_open + LE_open)/(Rn_1_1_1_open - SHF_1_mean)), size=0.5)+
  ylim(c(-12,12))+
  geom_hline(yintercept=0)+
  theme_bw()

# diurnal EB ratio
ggplot(flux)+
       geom_point(aes(x =hour(date_time), y = (H_open + LE_open)/(Rn_1_1_1_open - SHF_1_mean)), size=0.5)+
       #ylim(c(-2,2))+
       geom_hline(yintercept=1)+
       theme_bw()+facet_wrap(year+month~.)

# diurnal Rn - H - LE - SHF
ggplot(flux)+
  geom_point(aes(x =hour(date_time), y = Rn_1_1_1_open-SHF_1_mean-H_open-LE_open), size=0.5)+
  #ylim(c(-2,2))+
  geom_hline(yintercept=1)+
  theme_bw()+facet_wrap(year+month~.)

# graph regression of H vs Rn-G-LE
ggplot(flux)+
  geom_point(aes(x = H_open, y=Rn_G_LE), size=0.5)+
  geom_abline(intercept=0,slope=1, color="darkgrey")+
  theme_bw()+
  facet_wrap(year+month~.)

lm((Rn_G_LE) ~ (H_open), data=flux)

# Call:
#   lm(formula = (Rn_G_LE) ~ (H_open), data = flux)
# 
# Coefficients:
#   (Intercept)       H_open  
# -21.651        1.699  

# with addition of Jul 2024 - Dec 2025
# Coefficients:
#   (Intercept)       H_open  
# -22.581        1.705 

# EB of H being over-esimated during daytime ~ 7am to 5pm
ggplot(flux) + #[hour(date_time)>7 & hour(date_time)<17,])+
  geom_point(aes(x =date_time, y = (Rn_1_1_1_open - SHF_1_mean - LE_open)/(H_open)), size=0.5)+
  ylim(c(-12,12))+
  geom_hline(yintercept=0)+
  theme_bw()

# EB of H in 10 day increments
ggplot(flux[day(date)>=10 & day(date)<=20,]) +
  geom_point(aes(x =date_time, y = (Rn_1_1_1_open - SHF_1_mean - LE_open)/(H_open)), size=0.5)+
  ylim(-12,12)+
  geom_hline(yintercept=0)+
  facet_wrap(year+month(date_time)~., scales="free_x")+
  labs(title="EB days 10-20 of each month")


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
# 9 July 2026: add Scott's correction approach between 0.9-0.75 in 0.1 increments
mu <- 29.002 / 18.02
flux[,':=' (H_missing = Rn_1_1_1_open-SHF_1_mean-H_open-LE_open)]
flux[,':=' (wco2 = `w/co2_cov_open`*co2_scf_open,
              wco2_adjust = (`w/co2_cov_open`*0.9)*co2_scf_open,
              wT = `w/ts_cov_open`*H_scf_open,
             wq = `w/h2o_cov_open`*LE_scf_open,
              cp = air_heat_capacity_open,
              rho_a = air_density_open,
              tair = air_temperature_open-273.15,
              rho_c = co2_molar_density_open*(44.01/1e6),
              rho_q = h2o_molar_density_open*(18.02/1e6),
              rho_h = h2o_molar_density_open*(18.02/1e6))][
  ,':=' (sigma = rho_q/rho_a,
         lambda = (2.501 - 0.00237*tair)*1000000)][
          # ,':='   (wT_hcorr = `w/ts_cov_open`*(H_scf_open+ H_missing/cp/rho_a))][ # add correction for under-estimated H (H_missing needs to be in unit of wT), rho_a needs to be wet air density
             ,':='   (wT_hcorr = (`w/ts_cov_open`*(H_scf_open+ H_missing))/(cp*rho_a))][ # 7 July 2026 - parentheses error??
             ,':=' (ra=u_rot_open/(`u*_open`)^2, # add ra and Tsensor estimate from Kittler et al 2017
                 Tsens=0.0025*tair^2 + 0.9*tair+2.07+273.15)]



# DOUBLE CHECK UNITS IN EDDY PRO MANUAL
# Need H_corr in same unit as wT

#   data.LE = Lvap.*data.wq*mv*1e-3; % W/m2 - Latent heat flux
#   data.LE = Lvap.*data.wq*mv*1e-3; % W/m2 - Latent heat flux
#   mv=18/1000;                    % molar mass of water vapor [Kg/mol]
#   'wq','water vapor flux (after all corrections)','mmol H2O m-2 s-1';...
#   data.H = Cp.*data.rho.*data.wt; % W/m2 - Sensible heat flux (Eqn. 40 in WPL, 1980)
#   qs = data.rhov./data.rho; % Specific humidity
# Cp = Cpa*(1+0.84*qs); % J kg-1 K-1 - Specific heat of moist air
#   Cpa = 1004.67; % J Kg-1 K-1 - specific heat of dry air
#   data.rho  % Kg m-3 - moist air density

flux[,':=' (corrc1a =mu*(rho_c/rho_a)*((wq*(18.02/1e6))), 
              corrc2a = rho_c*(1+mu*sigma)*(wT/(tair+273.15)),
            corrc2a_hcorr = rho_c*(1+mu*sigma)*(wT_hcorr/(tair+273.15)),
              corrh1a = mu*(rho_h/rho_a)*(wq*(18.02/1e6)),
               corrh2a =  rho_h*(1+mu*sigma)*(wT/(tair+273.15)),
            corrh2a_hcorr =  rho_h*(1+mu*sigma)*(wT_hcorr/(tair+273.15)))]

flux[,':=' (fc_wpl = 1000* (wco2 + ((corrc1a+corrc2a)/(44.01/1e6))),
              fc_wpl_adjust = 1000* (wco2_adjust + ((corrc1a+corrc2a)/(44.01/1e6))),
            fc_wpl_hcorr = 1000* (wco2 + ((corrc1a+corrc2a_hcorr)/(44.01/1e6))),
              wpl_h2oa = corrh1a+corrh2a,
              LE_wpl = ((wq*(0.01802/1000))+(corrh1a+corrh2a))*lambda,
            LE_wpl_hcorr = ((wq*(0.01802/1000))+(corrh1a+corrh2a_hcorr))*lambda,
            wpl_open = ((corrc1a+corrc2a)/(44.01/1e6)))]

# 9 July 2026: add Scott's correction approach between 0.9-0.75 in 0.1 increments
# code with chatGPT

# create sequence of corfacts
make_fc <- function(dt,
                    corfacts = seq(0.75, 0.90, by = 0.01)) {
  
  # Names: wco2_adjust_090, wco2_adjust_089, etc
  # Names: fc_wpl_090, fc_wpl_089, etc
  suffix <- sprintf("%03d", round(corfacts * 100))
  
  w_names  <- paste0("wco2_adjust_", suffix)
  fc_names <- paste0("fc_wpl_", suffix)
  
  # Step 1 calculate wco2_adjust
  dt[, (w_names) :=
       lapply(corfacts, \(cf)
              (`w/co2_cov_open` * cf) * co2_scf_open)]
  
  # Step 2 calculate fc_wpl
  dt[, (fc_names) :=
       lapply(.SD, \(x)
              1000 * (x + ((corrc1a + corrc2a)/(44.01/1e6)))
       ),
     .SDcols = w_names]
  
  invisible(dt)
}

# Run it
make_fc(flux)


# apply QA/QC filtering from open path post-processing after EddyPro to recalculated fluxes:
# fc_wpl and fc_wpl_adjust
# use filter_fc_roll_daynight from processing
flux[filter_fc_roll_daynight_open!=0, ':=' (fc_wpl = NA, 
                                            fc_wpl_open = NA, 
                                            fc_wpl_adjust = NA)]

# Apply Burba correction using the fitting method of minimizing difference between CP and OP
# from Kittler et al 2017 https://agupubs.onlinelibrary.wiley.com/doi/10.1002/2017JG003830 
# and Deventer et al 2021 https://biometeorology.umn.edu/sites/biometeorology.umn.edu/files/2021-04/deventer2021.pdf
# equations 5 and 7 (estimate Tsensor from air temp only)
# fc_fit = fc_wpl_open + epsilon*(((Ts-Ta)*rho_c)/(ra*Ta))*(1 + mu*(rho_v/rho_d))
# epsilon parameter starting value = 0.05
 # fc_fit = co2_flux_closed

# graph Tsens and air temperature open (both K)
ggplot(flux, aes(x=date_time))+
  geom_line(aes(y=Tsens,color="IRGA temp, K"))+
  geom_line(aes(y=air_temperature_open,color="air temp, K"))

# fit model
model_fit_bc <- nls(co2_flux_closed ~
                      fc_wpl_open + epsilon*(((Tsens-air_temperature_open)*rho_c)/(ra*air_temperature_open))*(1 + mu*(rho_h/rho_a)),
                    data=flux,
                    start=list(epsilon=0.05))

# check model and calculate predicted values
summary(model_fit_bc)
fitY <- predict(model_fit_bc)
flux[,fit.model.kittler := predict(model_fit_bc,newdata=flux)]

# graph
ggplot(flux, aes(co2_flux_closed,fit.model.kittler))+
  geom_point()+
  geom_abline(yinterecpt=0,slope=1)+
  ylim(-15,15)+
  facet_wrap(year+month~.)

# add Wang correction Fwang = FOP + b1*H + b0
# b1 = 0.014257, b0 = - 0.066828 (in Wang et al 2017)
flux[,fc.wang := co2_flux_open + 0.014257*H_open - 0.066828]

# graph
ggplot(flux, aes(co2_flux_closed,fc.wang))+
  geom_point()+
  geom_abline(yinterecpt=0,slope=1)+
  ylim(-15,15)+
  facet_wrap(year+month~.)

# try calculating Scott-correct scalar to minimize open vs closed
model_fit_scottfact <- nls(co2_flux_closed ~
                             1000* (((`w/co2_cov_open`*scott.factor)*co2_scf_open) + ((corrc1a+corrc2a)/(44.01/1e6))),
                    data=flux,
                    start=list(scott.factor=1))

summary(model_fit_scottfact)

flux[,fit.model.scottfact := predict(model_fit_scottfact,newdata=flux)]

ggplot(flux, aes(co2_flux_closed,fit.model.scottfact))+
  geom_point()+
  geom_abline(yinterecpt=0,slope=1)+
  ylim(-15,15)+
  facet_wrap(year+month~.)

ggplot(flux) +
  geom_line(aes(date_time, co2_flux_closed), color="black",linewidth=0.2)+
  geom_line(aes(date_time, fit.model.scottfact),color="red", linewidth=0.2)+
  # ylim(-30,10)+
  facet_grid(.~year, scales="free_x")+
  labs(title="Regression based scott-correction-factor estimate (~0.753)")


# graph corrected 
# EddyPro corrected CO2 flux (black) and re-calculated CO2 flux (red)
ggplot(flux) +
  geom_point(aes(date_time, co2_flux_open), color="black",size=0.3)+
  geom_point(aes(date_time, fc_wpl_open),color="red", size=0.1)+
 # ylim(-30,10)+
  facet_grid(.~year, scales="free_x")+
  labs(title="James: EddyPro corrected CO2 flux (black) and re-calculated CO2 flux (red)")

# and in 1;1
ggplot(flux) +
  geom_point(aes(co2_flux_open,fc_wpl_open),size=0.3)+
  geom_abline(intercept=0,slope=1)+
  labs(title="James: EddyPro corrected and re-calculated CO2 flux (fc_wpl)")

# regression on my calculated fluxes
summary(lm((fc_wpl_open) ~ (co2_flux_open), data=flux))

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   0.0372262  0.0005732   64.95   <2e-16 ***
#   co2_flux_open 0.9825724  0.0005531 1776.48   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.09431 on 29705 degrees of freedom
# (5159 observations deleted due to missingness)
# Multiple R-squared:  0.9907,	Adjusted R-squared:  0.9907 

# look at offset corrected CO2 flux (sensu Scott et al 2015) and H corrected
ggplot(flux) +
  geom_point(aes(date, co2_flux_open), color="black",size=0.3)+
  geom_point(aes(date, fc_wpl_adjust),color="green", size=0.1)+
  geom_point(aes(date, fc_wpl_hcorr),color="blue", size=0.1)+
  #ylim(-30,10)+
  facet_grid(.~year, scales="free_x")+
  labs(title="EddyPro corrected CO2 flux (black) and 10% adjusted CO2 flux (green)")

# look at offset corrected CO2 flux (sensu Scott et al 2015) 
# and closed-path for reference
# graph full time series (or specific date range)
ggplot(flux[date>as.Date("2025-04-01") & date<as.Date("2025-04-30"),]) +
  geom_line(aes(date_time, co2_flux_open), color="black",size=0.3)+
  geom_line(aes(date_time, co2_flux_closed), color="#fdbb84",size=0.3)+
  geom_line(aes(date_time, fc_wpl_adjust),color="#31a354", size=0.2)+
  ylim(-5,5)+
  facet_grid(.~year, scales="free_x")+
  labs(title="Closed path, Open Path, Open path adjust")+
  theme_bw()


ggplot(flux[date>as.Date("2025-06-01") & date<as.Date("2025-06-30"),]) +
  geom_line(aes(date_time, co2_flux_open), color="black",size=0.3)+
  geom_line(aes(date_time, co2_flux_closed), color="#fdbb84",size=0.3)+
  geom_line(aes(date_time, fc_wpl_adjust),color="#31a354", size=0.2)+
  ylim(-5,5)+
  facet_grid(.~year, scales="free_x")+
  labs(title="Closed path, Open Path, Open path adjust")+
  theme_bw()

ggplot(flux[date>as.Date("2025-07-01") & date<as.Date("2025-07-30"),]) +
  geom_line(aes(date_time, co2_flux_open), color="black",size=0.3)+
  geom_line(aes(date_time, co2_flux_closed), color="#fdbb84",size=0.3)+
  geom_line(aes(date_time, fc_wpl_adjust),color="#31a354", size=0.2)+
  ylim(-5,5)+
  facet_grid(.~year, scales="free_x")+
  labs(title="Closed path, Open Path, Open path adjust")+
  theme_bw()

# look at H corrected vs open and closed in 10 day ranges
ggplot(flux[date>as.Date("2023-11-10") & date<as.Date("2023-11-20"),]) +
  geom_line(aes(date_time, co2_flux_open), color="black",size=0.3)+
  geom_line(aes(date_time, co2_flux_closed), color="red",size=0.3)+
  geom_line(aes(date_time, fc_wpl_hcorr),color="blue", size=0.1)+
  ylim(-5,5)+
  facet_grid(.~year, scales="free_x")+
  labs(title="James: EddyPro corrected CO2 flux (black) and 10% adjusted CO2 flux (green)")

# look at H corrected vs open and closed in 10 day range for each month and year
ggplot(flux[day(date)>5 & day(date)<16,]) +
  geom_line(aes(date_time, co2_flux_open, color="Open path"),size=0.3)+
  geom_line(aes(date_time, co2_flux_closed, color="Closed path"),size=0.3)+
  geom_line(aes(date_time, fc_wpl_hcorr,color="Open path H correct"), size=0.1)+
  ylim(-5,5)+
  facet_wrap(year+month~., scales="free_x")+
  scale_color_manual(values=cols3_H, breaks=c("Open path","Closed path", "Open path H correct"), name="Sensor")+
  labs(y="Hourly Flux Rate (umol/m2/s)", x="Date",title="Open path, Closed path, and H Correction")+
  theme_bw()


# look at H corrected vs open and closed by month and year
ggplot(flux[month==8]) +
  geom_line(aes(date_time, co2_flux_open), color="black",size=0.3)+
  geom_line(aes(date_time, co2_flux_closed), color="red",size=0.3)+
  geom_line(aes(date_time, fc_wpl_hcorr),color="blue", size=0.1)+
  ylim(-5,5)+
 # facet_grid(month~year, scales="free_x")+
  facet_wrap(year+month~., scales="free_x")+
  labs(title="James: EddyPro corrected CO2 flux (black) and 10% adjusted CO2 flux (green)")


# plot between days 10-20 of each month
  ggplot(flux[day(date)>=10 & day(date)<=20,]) +
    geom_line(aes(date_time, co2_flux_open), color="black",size=0.3)+
    geom_line(aes(date_time, co2_flux_closed), color="red",size=0.3)+
    geom_line(aes(date_time, fc_wpl_adjust),color="green", size=0.1)+
    geom_line(aes(date_time, fc_wpl_hcorr),color="blue", size=0.1)+
    ylim(-10,10)+
    facet_wrap(year+month(date_time)~., scales="free_x")+
    labs(title="James: EddyPro corrected CO2 flux (black) and 10% adjusted CO2 flux (green)")
  
  
# LE
ggplot(flux) +
  geom_point(aes(date, LE_open), color="black",size=0.3)+
  geom_point(aes(date, LE_wpl),color="blue", size=0.1)+
  facet_grid(.~year, scales="free_x")+
  labs(title="James: EddyPro corrected LE flux (black) and re-calculated LE flux (red)")

# and in 1;1
ggplot(flux) +
  geom_point(aes(LE_open,LE_wpl),size=0.3)+
  geom_abline(intercept=0,slope=1)+
  labs(title="James: EddyPro corrected and re-calculated CO2 flux")

# graph missing H vs wpl
ggplot(flux, aes(x=H_missing, y = ((corrc1a+corrc2a)/(44.01/1e6))))+
  geom_point()+
  facet_wrap(year~month(date_time))

# compare scott-corrected and H corrected

## graph the closed path and provisionally corrected flux with 0.9 offset
 ggplot(flux,
                 aes(co2_flux_closed,fc_wpl_adjust_open))+
     geom_point(size=0.5)+
     geom_smooth(method="lm")+
     ylim(c(-10,10))+
     xlim(c(-10,10))+
   geom_abline(intercept=0,slope=1, color="dark grey")+
     theme_bw()+
    facet_grid(year~month)+
    
    geom_text(
         data = rain_sum,
         aes(x = -1, y = 8, label = paste0("Rain (mm):", round(total_rain, 1))),
         inherit.aes = FALSE,
         size = 1.8
       )


## graph the closed path and provisionally corrected flux with H
ggplot(flux,
                 aes(co2_flux_closed,fc_wpl_hcorr))+
     geom_point(size=0.5)+
     geom_smooth(method="lm")+
     ylim(c(-10,10))+
     xlim(c(-10,10))+
     geom_abline(intercept=0,slope=1, color="dark grey")+
     theme_bw()+
     facet_grid(year~month)+
     
    geom_text(
         data = rain_sum,
         aes(x = -1, y = 8, label = paste0("Rain (mm):", round(total_rain, 1))),
         inherit.aes = FALSE,
         size = 1.8
      )

# graph for Kittler correction
## graph the closed path and provisionally corrected flux with H
ggplot(flux,
       aes(co2_flux_closed,fit.model.kittler))+
  geom_point(size=0.5)+
  geom_smooth(method="lm")+
  ylim(c(-10,10))+
  xlim(c(-10,10))+
  geom_abline(intercept=0,slope=1, color="dark grey")+
  theme_bw()+
  facet_grid(year~month)+
  
  geom_text(
    data = rain_sum,
    aes(x = -1, y = 8, label = paste0("Rain (mm):", round(total_rain, 1))),
    inherit.aes = FALSE,
    size = 1.8
  )

# graph for Wang correction
## graph the closed path and provisionally corrected flux with H
ggplot(flux,
       aes(co2_flux_closed,fc.wang))+
  geom_point(size=0.5)+
  geom_smooth(method="lm")+
  ylim(c(-10,10))+
  xlim(c(-10,10))+
  geom_abline(intercept=0,slope=1, color="dark grey")+
  theme_bw()+
  facet_grid(year~month)+
  
  geom_text(
    data = rain_sum,
    aes(x = -1, y = 8, label = paste0("Rain (mm):", round(total_rain, 1))),
    inherit.aes = FALSE,
    size = 1.8
  )

# regression by month and year for open vs closed
mod.lm.op.cp <- lm(co2_flux_open~co2_flux_closed*factor(year)*factor(month), data=flux)

summary(mod.lm.op.cp)

# regression by month and year for scott correct
mod.lm.scott <- lm(fc_wpl_adjust_open~co2_flux_closed*factor(year)*factor(month), data=flux)

summary(mod.lm.scott)

# regression by month and year with H correct
mod.lm.h <- lm(fc_wpl_hcorr~co2_flux_closed*factor(year)*factor(month), data=flux)

summary(mod.lm.h)

# regression by month and year model-estimated scott-factor
mod.lm.sf <- lm(fit.model.scottfact~co2_flux_closed*factor(year)*factor(month), data=flux)

summary(mod.lm.sf)

# regression by month and year model-estimated kittler correction
mod.lm.kit <- lm(fit.model.kittler~co2_flux_closed*factor(year)*factor(month), data=flux)

summary(mod.lm.kit)

# regression by month and year Wang H correction
mod.lm.wang <- lm(fc.wang~co2_flux_closed*factor(year)*factor(month), data=flux)

summary(mod.lm.wang)


# compare models
AIC(mod.lm.op.cp, mod.lm.scott, mod.lm.h,mod.lm.sf, mod.lm.kit, mod.lm.wang)


# calculate and graph daily with corrected
flux.daily.corr <- flux[,.(fc_open = mean(co2_flux_open, na.rm=TRUE),
                      fc_adj_open = mean(fc_wpl_adjust_open, na.rm=TRUE),
                      fc_closed = mean(co2_flux_closed, na.rm=TRUE),
                      fc_hcorr = mean(fc_wpl_hcorr, na.rm=TRUE),
                      fc_modsf = mean(fit.model.scottfact, na.rm=TRUE),
                      fc_modkit = mean(fit.model.kittler, na.rm=TRUE),
                      fc_wang = mean(fc.wang, na.rm=TRUE),
                      H_missing = mean(H_missing, na.rm=TRUE),
                      fc_open_sd = sd(co2_flux_open, na.rm=TRUE),
                      fc_adj_open_sd = sd(fc_wpl_adjust_open, na.rm=TRUE),
                      fc_closed_sd = sd(co2_flux_closed, na.rm=TRUE),
                      fc_hcorr_sd = sd(fc_wpl_hcorr, na.rm=TRUE),
                      H_missing_sd = sd(H_missing, na.rm=TRUE),
                      precip = sum(P_rain_1_1_1_open)),
                   by="date"]


# graph daily Open vs Closed path
fig.flux.daily.op.cp <- ggplot(flux.daily.corr, aes(x=date))+
  geom_line(aes(y=fc_open, colour="Open path"),linewidth=0.4)+
  #geom_errorbar(aes(ymin=fc_open-fc_open_sd,ymax=fc_open+fc_open_sd,colour="Open path") , size=0.2, width=0.1)+
    geom_line(aes(y=fc_closed, colour="Closed path"),linewidth=0.4)+
  #geom_errorbar(aes(ymin=fc_closed-fc_closed_sd,ymax=fc_closed+fc_closed_sd,colour="Closed path") , size=0.2, width=0.1)+
  theme_bw()+
  facet_grid(.~year(date), scales="free_x")+
  scale_color_manual(values=cols2, breaks=c("Open path","Closed path"))+
  labs(y="Mean Daily Flux Rate (umol/m2/s)", x="Date")

# graph daily rainfall
fig.rain.daily <- ggplot(flux.daily.corr, aes(x=date))+
  geom_col(aes(y=precip, fill="Daily Rainfall"))+
  #geom_errorbar(aes(ymin=fc_open-fc_open_sd,ymax=fc_open+fc_open_sd,colour="Open path") , size=0.2, width=0.1)+
  theme_bw()+
  facet_grid(.~year(date), scales="free_x")+
  scale_fill_manual(values=c("blue"),name="")+
  labs(y="Daily total Rainfall (mm)", x="Date")

# graph daily flux with daily rain
plot_grid(fig.flux.daily.op.cp+theme(axis.text.x = element_blank(),axis.title.x = element_blank()), 
          fig.rain.daily,
          nrow=2,
          align="v")

# graph daily with H correct
fig.flux.daily.c <- ggplot(flux.daily.corr, aes(x=date))+
  geom_line(aes(y=fc_open, colour="Open path"),linewidth=0.4)+
  #geom_errorbar(aes(ymin=fc_open-fc_open_sd,ymax=fc_open+fc_open_sd,colour="Open path") , size=0.2, width=0.1)+
  geom_line(aes(y=fc_hcorr, colour="Open path H correct"),linewidth=0.4)+
  #geom_errorbar(aes(ymin=fc_adj_open-fc_adj_open_sd,ymax=fc_adj_open+fc_adj_open_sd, colour="Open path adj") , size=0.2, width=0.1)+
  geom_line(aes(y=fc_closed, colour="Closed path"),linewidth=0.4)+
  #geom_errorbar(aes(ymin=fc_closed-fc_closed_sd,ymax=fc_closed+fc_closed_sd,colour="Closed path") , size=0.2, width=0.1)+
  theme_bw()+
  facet_grid(.~year(date), scales="free_x")+
  scale_color_manual(values=cols3_H, breaks=c("Open path","Closed path","Open path H correct"))+
  labs(y="Mean Daily Flux Rate (umol/m2/s)", x="Date")


# graph daily flux with daily rain
plot_grid(fig.flux.daily.c+theme(axis.text.x = element_blank(),axis.title.x = element_blank()), 
          fig.rain.daily,
          nrow=2,
          align="v")


# compare open, closed, corrected fluxes during day and night
# graph daily
fig.flux.daily.1 <- ggplot(flux.daily.corr, aes(x=date))+
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

# graph daily flux with daily rain
plot_grid(fig.flux.daily.1+theme(axis.text.x = element_blank(),axis.title.x = element_blank()), 
          fig.rain.daily,
          nrow=2,
          align="v")


# compare open, closed, scott corrected with nls model
# graph daily
fig.flux.daily.2 <- ggplot(flux.daily.corr, aes(x=date))+
  geom_line(aes(y=fc_open, colour="Open path"),linewidth=0.4)+
  #geom_errorbar(aes(ymin=fc_open-fc_open_sd,ymax=fc_open+fc_open_sd,colour="Open path") , size=0.2, width=0.1)+
  geom_line(aes(y=fc_modsf, colour="Open path adj 0.753"),linewidth=0.4)+
  #geom_errorbar(aes(ymin=fc_adj_open-fc_adj_open_sd,ymax=fc_adj_open+fc_adj_open_sd, colour="Open path adj") , size=0.2, width=0.1)+
  geom_line(aes(y=fc_closed, colour="Closed path"),linewidth=0.4)+
  #geom_errorbar(aes(ymin=fc_closed-fc_closed_sd,ymax=fc_closed+fc_closed_sd,colour="Closed path") , size=0.2, width=0.1)+
  theme_bw()+
  facet_grid(.~year(date), scales="free_x")+
  scale_color_manual(values=c("black","#fdbb84","#31a354"), breaks=c("Open path","Closed path","Open path adj 0.753"))+
  labs(y="Mean Daily Flux Rate (umol/m2/s)", x="Date")

# graph daily flux with daily rain
plot_grid(fig.flux.daily.2+theme(axis.text.x = element_blank(),axis.title.x = element_blank()), 
          fig.rain.daily,
          nrow=2,
          align="v")

# compare open, closed, kittler nls model
# graph daily
fig.flux.daily.3 <- ggplot(flux.daily.corr, aes(x=date))+
  geom_line(aes(y=fc_open, colour="Open path"),linewidth=0.4)+
  #geom_errorbar(aes(ymin=fc_open-fc_open_sd,ymax=fc_open+fc_open_sd,colour="Open path") , size=0.2, width=0.1)+
  geom_line(aes(y=fc_modkit, colour="Open path Kittler"),linewidth=0.4)+
  #geom_errorbar(aes(ymin=fc_adj_open-fc_adj_open_sd,ymax=fc_adj_open+fc_adj_open_sd, colour="Open path adj") , size=0.2, width=0.1)+
  geom_line(aes(y=fc_closed, colour="Closed path"),linewidth=0.4)+
  #geom_errorbar(aes(ymin=fc_closed-fc_closed_sd,ymax=fc_closed+fc_closed_sd,colour="Closed path") , size=0.2, width=0.1)+
  theme_bw()+
  facet_grid(.~year(date), scales="free_x")+
  scale_color_manual(values=c("black","#fdbb84","#31a354"), breaks=c("Open path","Closed path","Open path Kittler"))+
  labs(y="Mean Daily Flux Rate (umol/m2/s)", x="Date")

# graph daily flux with daily rain
plot_grid(fig.flux.daily.3+theme(axis.text.x = element_blank(),axis.title.x = element_blank()), 
          fig.rain.daily,
          nrow=2,
          align="v")

# compare open, closed, Wang correction
# graph daily
fig.flux.daily.4 <- ggplot(flux.daily.corr, aes(x=date))+
  geom_line(aes(y=fc_open, colour="Open path"),linewidth=0.4)+
  #geom_errorbar(aes(ymin=fc_open-fc_open_sd,ymax=fc_open+fc_open_sd,colour="Open path") , size=0.2, width=0.1)+
  geom_line(aes(y=fc_wang, colour="Open path Wang"),linewidth=0.4)+
  #geom_errorbar(aes(ymin=fc_adj_open-fc_adj_open_sd,ymax=fc_adj_open+fc_adj_open_sd, colour="Open path adj") , size=0.2, width=0.1)+
  geom_line(aes(y=fc_closed, colour="Closed path"),linewidth=0.4)+
  #geom_errorbar(aes(ymin=fc_closed-fc_closed_sd,ymax=fc_closed+fc_closed_sd,colour="Closed path") , size=0.2, width=0.1)+
  theme_bw()+
  facet_grid(.~year(date), scales="free_x")+
  scale_color_manual(values=c("black","#fdbb84","#31a354"), breaks=c("Open path","Closed path","Open path Wang"))+
  labs(y="Mean Daily Flux Rate (umol/m2/s)", x="Date")

# graph daily flux with daily rain
plot_grid(fig.flux.daily.4+theme(axis.text.x = element_blank(),axis.title.x = element_blank()), 
          fig.rain.daily,
          nrow=2,
          align="v")


# calculate and graph diurnal corrected fluxes
flux.diurn.corr <- flux[,.(fc_open = mean(co2_flux_open, na.rm=TRUE),
                      fc_adj_open = mean(fc_wpl_adjust_open, na.rm=TRUE),
                      fc_closed = mean(co2_flux_closed, na.rm=TRUE),
                      fc_hcorr = mean(fc_wpl_hcorr, na.rm=TRUE),
                      fc_modsf = mean(fit.model.scottfact, na.rm=TRUE),
                      H_missing = mean(H_missing, na.rm=TRUE),
                      scf_open = mean(co2_scf_open, na.rm=TRUE),
                      scf_closed = mean(co2_scf_closed, na.rm=TRUE),
                      wpl_open = mean((corrc1a_open+corrc2a_open)/(44.01/1e6), na.rm=TRUE),
                      wco2_open = mean(wco2_open, na.rm=TRUE),
                      fc_open_sd = sd(co2_flux_open, na.rm=TRUE),
                      fc_adj_open_sd = sd(fc_wpl_adjust_open, na.rm=TRUE),
                      fc_closed_sd = sd(co2_flux_closed, na.rm=TRUE), 
                      fc_hcorr_sd = sd(fc_wpl_hcorr, na.rm=TRUE),
                      H_missing_sd = sd(H_missing, na.rm=TRUE),
                      scf_open_sd = sd(co2_scf_open, na.rm=TRUE),
                      scf_closed_sd = sd(co2_scf_closed, na.rm=TRUE),
                      wpl_open_sd = sd((corrc1a_open+corrc2a_open)/(44.01/1e6), na.rm=TRUE),
                      wco2_open_sd = sd(wco2_open, na.rm=TRUE)),
                   by="year,month,half.hour"]
# graph diurnal fluxes with open path, closed path, H corrected open path
ggplot(flux.diurn.corr, aes(x=half.hour))+
  geom_line(aes(y=fc_open, colour="Open path"), linewidth=0.3)+
  geom_point(aes(y=fc_open, colour="Open path") , size=0.3)+
  geom_errorbar(aes(ymin=fc_open-fc_open_sd,ymax=fc_open+fc_open_sd, colour="Open path") , linewidth=0.2, width=0.1)+
  geom_line(aes(y=fc_closed, colour="Closed path") , linewidth=0.3)+
  geom_point(aes(y=fc_closed, colour="Closed path") , size=0.3)+
  geom_errorbar(aes(ymin=fc_closed-fc_closed_sd,ymax=fc_closed+fc_closed_sd, colour="Closed path") , linewidth=0.2, width=0.1)+
  #geom_line(aes(y=fc_adj_open, colour="Open path adj"), linewidth=0.3)+
  #geom_point(aes(y=fc_adj_open, colour="Open path adj"),  size=0.3)+
  #geom_errorbar(aes(ymin=fc_adj_open-fc_adj_open_sd,ymax=fc_adj_open+fc_adj_open_sd, colour="Open path adj") , linewidth=0.2, width=0.1)+
  ylim(c(-3,3))+
  geom_line(aes(y=fc_hcorr, colour="Open path H correct"), linewidth=0.3)+
  geom_point(aes(y=fc_hcorr, colour="Open path H correct") , size=0.3)+
  geom_errorbar(aes(ymin=fc_hcorr-fc_hcorr_sd,ymax=fc_hcorr+fc_hcorr_sd, colour="Open path H correct") , linewidth=0.2, width=0.1)+
  geom_hline(yintercept=0, colour="gray18")+
  theme_bw()+
  facet_wrap(year~month)+
  geom_hline(yintercept=0, colour="gray18")+
  scale_color_manual(values=cols3_H, breaks=c("Open path","Closed path", "Open path H correct"), )+
   labs(y="Mean Hourly Flux Rate (umol/m2/s)", x="Hour",title="Open path, Closed path, and H Correction")

# graph diurnal fluxes with open path, closed path, 0.9 adjust
ggplot(flux.diurn.corr, aes(x=half.hour))+
  geom_line(aes(y=fc_open, colour="Open path"), linewidth=0.3)+
  geom_point(aes(y=fc_open, colour="Open path") , size=0.3)+
  geom_errorbar(aes(ymin=fc_open-fc_open_sd,ymax=fc_open+fc_open_sd, colour="Open path") , linewidth=0.2, width=0.1)+
  geom_line(aes(y=fc_closed, colour="Closed path") , linewidth=0.3)+
  geom_point(aes(y=fc_closed, colour="Closed path") , size=0.3)+
  geom_errorbar(aes(ymin=fc_closed-fc_closed_sd,ymax=fc_closed+fc_closed_sd, colour="Closed path") , linewidth=0.2, width=0.1)+
  geom_line(aes(y=fc_adj_open, colour="Open path adj"), linewidth=0.3)+
  geom_point(aes(y=fc_adj_open, colour="Open path adj"),  size=0.3)+
  geom_errorbar(aes(ymin=fc_adj_open-fc_adj_open_sd,ymax=fc_adj_open+fc_adj_open_sd, colour="Open path adj") , linewidth=0.2, width=0.1)+
  #ylim(c(-10,10))+
  #geom_line(aes(y=fc_hcorr, colour="Open path H correct"), linewidth=0.3)+
  #geom_point(aes(y=fc_hcorr, colour="Open path H correct") , size=0.3)+
  #geom_errorbar(aes(ymin=fc_hcorr-fc_hcorr_sd,ymax=fc_hcorr+fc_hcorr_sd, colour="Open path H correct") , linewidth=0.2, width=0.1)+
  geom_hline(yintercept=0, colour="gray18")+
  theme_bw()+
  facet_wrap(year~month)+
  scale_color_manual(values=cols3, breaks=c("Open path","Closed path","Open path adj"), )+
  labs(y="Mean Hourly Flux Rate (umol/m2/s)", x="Hour", title="Open path, Closed path, and Fixed Correction")

# graph diurnal H missing
ggplot(flux.diurn.corr, aes(x=half.hour))+
  geom_line(aes(y=H_missing, colour="Missing EB"), linewidth=0.3)+
  geom_point(aes(y=H_missing, colour="Missing EB") , size=0.3)+
  geom_errorbar(aes(ymin=H_missing-H_missing_sd,ymax=H_missing+H_missing_sd, colour="Missing EB") , linewidth=0.2, width=0.1)+
  geom_hline(yintercept=0)+
  theme_bw()+
  facet_wrap(year~month)+
  labs(y="EB (W/m2)", x="Half-Hour", title="Missing Energy Balance: Rn - G - H - LE")+
  annotate("text", x = 10, y = -100, label = "H over-estimated", size=2)+
  annotate("text", x = 10, y = 250, label = "H under-estimated",size=2)


# aggregate dataset for ML to model fluxes guided by closed path
# variables to include: 
# 
# Biomet vars: Ta, RH, Pa, WD, MWS, PPFD, P_rain, SWC, Ts, SHF_1_mean, SHF_2_mean,
# LWin, LWout, SWout, Rg, Rn, VPD

# Flux vars (open only): NEE, LE, H, u*, co2_mole_fraction, h20_mole_fraction, air_density, w/co2_cov, 
# daytime, co2_scf, WPL

colnames(flux[,208:226])
ML_col_date <- c("year","date_time")

ML_col_biomet <- c(colnames(flux[,208:213]),
                   colnames(flux[,215:217]),
                   colnames(flux[,222:226]),
                   "SHF_1_mean","SHF_2_mean")

ML_col_flux <- c("co2_flux_closed","co2_flux_open", "H_open", "LE_open","u*_open","co2_mole_fraction_open",
                 "h2o_mole_fraction_open", "air_density_open","w/co2_cov_open","co2_scf_open",
                 "wpl_open","daytime_open")

ML_col_all <- c(ML_col_date,ML_col_flux,ML_col_biomet)

# subset flux into only columns for ML using column names in vectors
flux_ml <- copy(flux[,(ML_col_all),with=FALSE])

# calculate VPD (hPa)
flux_ml[,VPD_open := fCalcVPDfromRHandTair(RH_1_1_1_open,Ta_1_1_1_open)]

# calculate lag variables (24, 48, 144, 240, 336) => 0.5, 1, 3, 5, 7 days:
# P_rain (total), VPD (mean, max, sum), NEE/LE/H (mean, sum)
# 2026-06-03: currently not including flux variables due to many NA 
flux_ml[, ":=" (P_rain_24 = frollsum(P_rain_1_1_1_open,n=24,align="right",has.nf=TRUE),
                P_rain_48 = frollsum(P_rain_1_1_1_open,n=48,align="right",has.nf=TRUE),
                P_rain_144 = frollsum(P_rain_1_1_1_open,n=144,align="right",has.nf=TRUE),
                P_rain_240 = frollsum(P_rain_1_1_1_open,n=240,align="right",has.nf=TRUE),
                P_rain_336 = frollsum(P_rain_1_1_1_open,n=336,align="right",has.nf=TRUE),
                VPD_sum_24 = frollsum(VPD_open,n=24,align="right",has.nf=TRUE),
                VPD_sum_48 = frollsum(VPD_open,n=48,align="right",has.nf=TRUE),
                VPD_sum_144 = frollsum(VPD_open,n=144,align="right",has.nf=TRUE),
                VPD_sum_240 = frollsum(VPD_open,n=240,align="right",has.nf=TRUE),
                VPD_sum_336 = frollsum(VPD_open,n=336,align="right",has.nf=TRUE),
                VPD_mean_24 = frollmean(VPD_open,n=24,align="right",has.nf=TRUE),
                VPD_mean_48 = frollmean(VPD_open,n=48,align="right",has.nf=TRUE),
                VPD_mean_144 = frollmean(VPD_open,n=144,align="right",has.nf=TRUE),
                VPD_mean_240 = frollmean(VPD_open,n=240,align="right",has.nf=TRUE),
                VPD_mean_336 = frollmean(VPD_open,n=336,align="right",has.nf=TRUE),
                VPD_max_24 = frollmax(VPD_open,n=24,align="right",has.nf=TRUE),
                VPD_max_48 = frollmax(VPD_open,n=48,align="right",has.nf=TRUE),
                VPD_max_144 = frollmax(VPD_open,n=144,align="right",has.nf=TRUE),
                VPD_max_240 = frollmax(VPD_open,n=240,align="right",has.nf=TRUE),
                VPD_max_336 = frollmax(VPD_open,n=336,align="right",has.nf=TRUE))]

# graph lagged variable to see
ggplot(flux_ml, aes(x=date_time))+
  geom_line(aes(y=VPD_open),linewidth =0.1)+
  geom_line(aes(y=VPD_mean_24),linewidth=0.1,color="lightblue")+
  geom_line(aes(y=VPD_mean_48),linewidth=0.1,color="blue")+
  geom_line(aes(y=VPD_mean_336),linewidth=0.1,color="darkblue")+
  facet_wrap(year~month(date_time),scales="free_x")

# SAVE
setwd("/Users/memauritz/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_EddyPro_filtered/")

save(flux_ml, file="JER_flux_202307_202512_Open_Closed_ML_input.Rdata")

