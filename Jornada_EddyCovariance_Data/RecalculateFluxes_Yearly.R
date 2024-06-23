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
#############
# IMPORT DATA
#############
# get header and unit info from the first rows of data files
# file info
fileinfo <- scan("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Bahada/Tower/EddyCovariance_ts/2022/EddyPro_Out/eddypro_JER_2022_OctDec_full_output_2024-03-15T230111_exp.csv",
                 what='',sep=",",nlines=1)
fileinfo <- data.table(t(fileinfo))
# read only the first row to get the units
flux.units <- (fread("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Bahada/Tower/EddyCovariance_ts/2022/EddyPro_Out/eddypro_JER_2022_OctDec_full_output_2024-03-15T230111_exp.csv",header=TRUE,skip=1))[1,]

# read only the first row to get the units- files with 180 length have additional diag_75_mean column
# flux.units.180 <- (fread("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Bahada/Tower/EddyCovariance_ts/2022/EddyPro_Out/eddypro_JER_2022_OctDec_full_output_2024-03-15T230111_exp.csv",header=TRUE,skip=1))[1,]


# read the data, skippping the units row: Oct-Dec 2022
flux1 <- fread("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Bahada/Tower/EddyCovariance_ts/2022/EddyPro_Out/eddypro_JER_2022_OctDec_full_output_2024-03-15T230111_exp.csv", sep=",",skip=3,
                header=FALSE, na.strings=c("-9999","-9999.0","NAN","#NAME?"),
               col.names=colnames(flux.units))
# Jan - Jun 2023 (ts_data)
flux2 <- fread("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Bahada/Tower/EddyCovariance_ts/2023/EddyPro_Out/eddypro_JER_2023_JanJun_full_output_2024-03-15T230543_exp.csv", sep=",",skip=3,
               header=FALSE, na.strings=c("-9999","-9999.0","NAN","#NAME?"),
               col.names=colnames(flux.units))

# Jul - Dec ts_data_2
flux2a <- fread("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Bahada/Tower/ts_data_2/2023/EddyPro_Out/OpenPath/eddypro_JER_2023_JulDec_tsdata2_Open_full_output_2024-06-07T162306_exp.csv", sep=",",skip=3,
               header=FALSE, na.strings=c("-9999","-9999.0","NAN","#NAME?"),
               col.names=colnames(flux.units))

# Jan - May 2024 ts_data_2
flux3 <- fread("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Bahada/Tower/ts_data_2/2024/EddyPro_Out/OpenPath/eddypro_JER_2024_JanMay_tsdata2_Open_full_output_2024-06-11T182618_exp.csv", sep=",",skip=3,
               header=FALSE, na.strings=c("-9999","-9999.0","NAN","#NAME?"),
               col.names=colnames(flux.units))


# combine all individual years of flux runs
flux <- rbind(flux1, flux2, flux2a, flux3, fill=TRUE)

# format date
flux[,date_time := paste(date,time,sep=" ")]
flux[,':=' (date=as.Date(date),
            date_time = as.POSIXct(date_time, format="%Y-%m-%d %H:%M"),
            year=year(date_time))]

# Graph initial CO2 flux, just to see
# co2 flux
ggplot(flux, aes(DOY,co2_flux,colour=factor(qc_co2_flux)))+
  geom_point(size=0.2)+
  ylim(c(-20,20))+
  facet_grid(.~year)

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
