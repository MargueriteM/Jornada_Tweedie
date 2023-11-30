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
fileinfo_open <- scan("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Bahada/Tower/EddyCovariance_ClosedPath/EddyPro_Out/20231013_OpenPathCompare/eddypro_JER_2023_0811_1010_OpenPathCompare_full_output_2023-10-13T154144_adv.csv",
                 what='',sep=",",nlines=1)
fileinfo_open <- data.table(t(fileinfo_open))
# read only the first row to get the units
flux.units.open <- (fread("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Bahada/Tower/EddyCovariance_ClosedPath/EddyPro_Out/20231013_OpenPathCompare/eddypro_JER_2023_0811_1010_OpenPathCompare_full_output_2023-10-13T154144_adv.csv",header=TRUE,skip=1))[1,]

# closed path
fileinfo_closed <- scan("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Bahada/Tower/EddyCovariance_ClosedPath/EddyPro_Out/20231018_ClosedPathCompare2_umolmol/eddypro_JER_2023_0811_1010_ClosedPathCompare2_full_output_2023-10-18T121739_adv.csv",
                      what='',sep=",",nlines=1)
fileinfo_closed <- data.table(t(fileinfo_closed))
# read only the first row to get the units
flux.units.closed <- (fread("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Bahada/Tower/EddyCovariance_ClosedPath/EddyPro_Out/20231018_ClosedPathCompare2_umolmol/eddypro_JER_2023_0811_1010_ClosedPathCompare2_full_output_2023-10-18T121739_adv.csv",header=TRUE,skip=1))[1,]


# read the data, skippping the units row
flux_open <- fread("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Bahada/Tower/EddyCovariance_ClosedPath/EddyPro_Out/20231013_OpenPathCompare/eddypro_JER_2023_0811_1010_OpenPathCompare_full_output_2023-10-13T154144_adv.csv", sep=",",skip=3,
                header=FALSE, na.strings=c("-9999","-9999.0","NAN","#NAME?"),col.names=colnames(flux.units.open))


flux_closed <- fread("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Bahada/Tower/EddyCovariance_ClosedPath/EddyPro_Out/20231018_ClosedPathCompare2_umolmol/eddypro_JER_2023_0811_1010_ClosedPathCompare2_full_output_2023-10-18T121739_adv.csv", sep=",",skip=3,
                   header=FALSE, na.strings=c("-9999","-9999.0","NAN","#NAME?"),col.names=colnames(flux.units.closed))



# column numbers differ, find difference
c(setdiff(colnames(flux_open), colnames(flux_closed)), setdiff(colnames(flux_closed), colnames(flux_open)))

# change column names and merge
setnames(flux_open, 1, paste0(names(flux_open)[1], '_open'))
setnames(flux_open, 5:179, paste0(names(flux_open)[5:179], '_open'))

setnames(flux_closed, 1, paste0(names(flux_closed)[1], '_closed'))
setnames(flux_closed, 5:176, paste0(names(flux_closed)[5:176], '_closed'))

# merge open and closed
keycols = c("date","time", "DOY")

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
ggplot(flux, aes(DOY,co2_flux_open,colour=factor(qc_co2_flux_open)))+
  geom_point(size=0.2)+
  ylim(c(-20,20))

ggplot(flux, aes(DOY,co2_flux_closed,colour=factor(qc_co2_flux_closed)))+
  geom_point(size=0.2)+
  ylim(c(-20,20))

# graph the time-series of CO2 flux together
ggplot(flux[qc_co2_flux_open<2 & qc_co2_flux_closed <2 & agc_mean_open<=56 & DOY>230], aes(x=DOY))+
  geom_line(aes(y=co2_flux_open), size=0.2, color="black")+
  geom_line(aes(y=co2_flux_closed), size=0.2, color="green")+
    ylim(c(-20,20))


## graph the fluxes against each other
ggplot(flux[qc_co2_flux_open<2 & qc_co2_flux_closed <2 & agc_mean_open<=56],
       aes(co2_flux_closed,co2_flux_open))+
  geom_point(size=0.2)+
  ylim(c(-20,20))+
  geom_abline(intercept=0,slope=1)


# compare co2 molar density
ggplot(flux[qc_co2_flux_open<2 & qc_co2_flux_closed <2 & agc_mean_open<=56],
       aes(co2_molar_density_closed,co2_molar_density_open))+
  geom_point(size=0.2)+
  geom_abline(intercept=0,slope=1)

# compare co2 mole fraction
ggplot(flux[qc_co2_flux_open<2 & qc_co2_flux_closed <2 & agc_mean_open<=56],
       aes(co2_mole_fraction_closed,co2_mole_fraction_open))+
  geom_point(size=0.2)+
  geom_abline(intercept=0,slope=1)

# compare air density
ggplot(flux[qc_co2_flux_open<2 & qc_co2_flux_closed <2 & agc_mean_open<=56],
       aes(air_density_closed,air_density_open))+
  geom_point(size=0.2)+
  geom_abline(intercept=0,slope=1)


# compare h2o molar density
ggplot(flux[qc_h2o_flux_open<2 & qc_h2o_flux_closed <2 & agc_mean_open<=56],
       aes(h2o_molar_density_closed,h2o_molar_density_open))+
  geom_point(size=0.2)+
  geom_abline(intercept=0,slope=1)

# graph the time-series of CO2 molar density
ggplot(flux[qc_co2_flux_open<2 & qc_co2_flux_closed <2 & agc_mean_open<=56 & DOY>230], aes(x=DOY))+
  geom_point(aes(y=co2_molar_density_open), size=0.2, color="black")+
  geom_point(aes(y=co2_molar_density_closed), size=0.2, color="green")

# graph the time-series of CO2 mole fraction
ggplot(flux[qc_co2_flux_open<2 & qc_co2_flux_closed <2 & agc_mean_open<=56 & DOY>230], aes(x=DOY))+
  geom_point(aes(y=co2_mole_fraction_open), size=0.2, color="black")+
  geom_point(aes(y=co2_mole_fraction_closed), size=0.2, color="green")

# graph the time-series of air density
ggplot(flux[qc_co2_flux_open<2 & qc_co2_flux_closed <2 & agc_mean_open<=56 & DOY>230], aes(x=DOY))+
  geom_point(aes(y=air_density_open), size=0.2, color="black")+
  geom_point(aes(y=air_density_closed), size=0.2, color="green")

# graph the time-series of H2O molar density
ggplot(flux[qc_co2_flux_open<2 & qc_co2_flux_closed <2 & agc_mean_open<=56 & DOY>230], aes(x=DOY))+
  geom_point(aes(y=h2o_molar_density_open), size=0.2, color="black")+
  geom_point(aes(y=h2o_molar_density_closed), size=0.2, color="green")


## graph the co2 fluxes against each other
fig.co2.compare <- ggplot(flux[qc_co2_flux_open<2 & qc_co2_flux_closed <2 & agc_mean_open<=56&DOY>230],
       aes(co2_flux_closed,co2_flux_open))+
  geom_point(size=0.5)+
  ylim(c(-20,20))+
  xlim(c(-20,20))+
  geom_abline(intercept=0,slope=1, color="dark grey")+
  theme_bw()

## graph the LE fluxes against each other
fig.le.compare <- ggplot(flux[qc_h2o_flux_open<2 & qc_h2o_flux_closed <2 & agc_mean_open<=56&DOY>230],
       aes(LE_closed,LE_open))+
  geom_point(size=0.5)+
  geom_abline(intercept=0,slope=1, color="dark grey")+
  theme_bw()

## graph the H fluxes against each other
fig.h.compare <- ggplot(flux[qc_H_open<2 & qc_H_closed <2& agc_mean_open<=56&DOY>230],
       aes(H_closed,H_open))+
  geom_point(size=0.5)+
  geom_abline(intercept=0,slope=1, color="dark grey")+
  theme_bw()


# compare co2 molar density
fig.co2.moldens.compare <- ggplot(flux[qc_co2_flux_open<2 & qc_co2_flux_closed <2 & agc_mean_open<=56],
       aes(co2_molar_density_closed,co2_molar_density_open))+
  geom_point(size=0.2)+
  geom_abline(intercept=0,slope=1, color="dark grey")+
  theme_bw()

# compare h2o molar density
fig.h2o.moldens.compare <- ggplot(flux[qc_h2o_flux_open<2 & qc_h2o_flux_closed <2 & agc_mean_open<=56],
       aes(h2o_molar_density_closed,h2o_molar_density_open))+
  geom_point(size=0.2)+
  geom_abline(intercept=0,slope=1)

# graph the covariances against each other
fig.cov.compare <- ggplot(flux[qc_co2_flux_open<2 & qc_co2_flux_closed <2 & agc_mean_open<=56&DOY>230],
       aes(`w/co2_cov_closed`,`w/co2_cov_open`))+
  geom_point(size=0.5)+
  ylim(c(-0.1,0.1))+
 xlim(c(-0.1,0.1))+
  geom_abline(intercept=0,slope=1, color="dark grey")+
  theme_bw()

plot_grid(fig.co2.compare, fig.moldens.compare, fig.le.compare, fig.h.compare)
plot_grid(fig.co2.moldens.compare, fig.h2o.moldens.compare)

# co-plot time-series
# graph the time-series of CO2 flux together

fig.time.co2 <- ggplot()+
  geom_line(aes(x=DOY,y=co2_flux_open), data = flux[qc_co2_flux_open<2 & agc_mean_open<=56&DOY>230], size=0.2, color="black")+
  geom_point(aes(x=DOY,y=co2_flux_open), data = flux[qc_co2_flux_open<2 & agc_mean_open<=56&DOY>230], size=0.1, color="black")+
  geom_line(aes(x=DOY, y=co2_flux_closed), data=flux[qc_co2_flux_open <2&DOY>230], size=0.2, color="green")+
  geom_point(aes(x=DOY, y=co2_flux_closed), data=flux[qc_co2_flux_open <2&DOY>230], size=0.1, color="green")+
  ylim(c(-20,20))

# graph covariance time-series together
fig.time.cov <- ggplot()+
  geom_line(aes(x=DOY,y=`w/co2_cov_open`), data = flux[qc_co2_flux_open<2 & agc_mean_open<=56&DOY>230], size=0.2, color="black")+
  geom_line(aes(x=DOY, y=`w/co2_cov_closed`), data=flux[qc_co2_flux_open <2&DOY>230], size=0.2, color="green")+
  ylim(c(-0.2,0.2))


# graph the time-series of CO2 molar density
fig.time.co2.moldens <- ggplot(flux[qc_co2_flux_open<2 & qc_co2_flux_closed <2 & agc_mean_open<=56 & DOY>230], aes(x=DOY))+
  geom_point(aes(y=co2_molar_density_open), size=0.2, color="black")+
  geom_point(aes(y=co2_molar_density_closed), size=0.2, color="green")+
  geom_line(aes(y=co2_molar_density_open), size=0.2, color="black")+
  geom_line(aes(y=co2_molar_density_closed), size=0.2, color="green")


# graph the time-series of CO2 molar density
fig.time.h2o.moldens <- ggplot(flux[qc_h2o_flux_open<2 & qc_h2o_flux_closed <2 & agc_mean_open<=56 & DOY>230], aes(x=DOY))+
  geom_point(aes(y=h2o_molar_density_open), size=0.2, color="black")+
  geom_point(aes(y=h2o_molar_density_closed), size=0.2, color="green")+
  geom_line(aes(y=h2o_molar_density_open), size=0.2, color="black")+
  geom_line(aes(y=h2o_molar_density_closed), size=0.2, color="green")


# graph H time-series together
fig.time.h <- ggplot()+
  geom_point(aes(x=DOY,y=H_open), data = flux[qc_H_open<2 & agc_mean_open<=56& DOY>230], size=0.2, color="black")+
  geom_point(aes(x=DOY, y=H_closed), data=flux[qc_H_closed <2&DOY>230], size=0.2, color="green")+
  geom_line(aes(x=DOY,y=H_open), data = flux[qc_H_open<2 & agc_mean_open<=56& DOY>230], size=0.2, color="black")+
  geom_line(aes(x=DOY, y=H_closed), data=flux[qc_H_closed <2&DOY>230], size=0.2, color="green")


# graph LE time-series together
fig.time.le <- ggplot()+
  geom_line(aes(x=DOY,y=LE_open), data = flux[qc_h2o_flux_open<2 & agc_mean_open<56&DOY>230], size=0.2, color="black")+
  geom_line(aes(x=DOY, y=LE_closed), data=flux[qc_h2o_flux_open <2&DOY>230], size=0.2, color="green")+
  geom_point(aes(x=DOY,y=LE_open), data = flux[qc_h2o_flux_open<2 & agc_mean_open<56&DOY>230], size=0.2, color="black")+
  geom_point(aes(x=DOY, y=LE_closed), data=flux[qc_h2o_flux_open <2&DOY>230], size=0.2, color="green")



plot_grid(fig.time.co2, fig.time.moldens, fig.time.le, fig.time.h)

plot_grid(fig.time.co2+ylim(-5,5), fig.time.le, nrow=2)

plot_grid(fig.time.co2.moldens, fig.time.h2o.moldens, nrow=2)

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
