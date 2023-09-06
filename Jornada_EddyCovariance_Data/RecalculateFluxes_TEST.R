
# file info
fileinfo <- scan("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_2010/eddypro_JER_2010_full_output_2019-08-08T131210_adv.csv",
                 what='',sep=",",nlines=1)
fileinfo <- data.table(t(fileinfo))
# read only the first row to get the units
flux.units <- (fread("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_2010/eddypro_JER_2010_full_output_2019-08-08T131210_adv.csv",header=TRUE,skip=1))[1,]

# read the data, skippping the units row
flux1a <- fread("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_2010/length14/eddypro_JER_2010_length14_full_output_2020-02-04T105358_adv.csv", sep=",",skip=3,
                header=FALSE, na.strings=c("-9999","-9999.0","NAN","#NAME?"),col.names=colnames(flux.units))


# # from James
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

mu <- 29.002 / 18.02
flux1a[,':=' (wco2 = `w/co2_cov`*co2_scf,
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

flux1a[,':=' (corrc1a =mu*(rho_c/rho_a)*((wq*(18.02/1e6))), 
              corrc2a = rho_c*(1+mu*sigma)*(wT/(tair+273.15)),
              corrh1a = mu*(rho_h/rho_a)*(wq*(18.02/1e6)),
               corrh2a =  rho_h*(1+mu*sigma)*(wT/(tair+273.15)))]

flux1a[,':=' (fc_wpl = 1000* (wco2 + ((corrc1a+corrc2a)/(44.01/1e6))),
              fc_wpl_adjust = 1000* (wco2_adjust + ((corrc1a+corrc2a)/(44.01/1e6))),
              wpl_h2oa = corrh1a+corrh2a,
              LE_wpl = ((wq*(0.01802/1000))+(corrh1a+corrh2a))*lambda)]
  
# graph corrected 
# EddyPro corrected CO2 flux (black) and re-calculated CO2 flux (red)
ggplot(flux1a) +
  geom_point(aes(date, co2_flux), color="black",size=0.3)+
  geom_point(aes(date, fc_wpl),color="red", size=0.1)+
  ylim(-30,10)+
  labs(title="James: EddyPro corrected CO2 flux (black) and re-calculated CO2 flux (red)")

ggplot(flux1a) +
  geom_point(aes(date, co2_flux), color="black",size=0.3)+
  geom_point(aes(date, fc_wpl_adjust),color="green", size=0.1)+
  ylim(-30,10)+
  labs(title="James: EddyPro corrected CO2 flux (black) and adjusted CO2 flux (green)")


ggplot(flux1a) +
  geom_point(aes(date, LE), color="black",size=0.3)+
  geom_point(aes(date, LE_wpl),color="blue", size=0.1)+
  ylim(-30,10)+
  labs(title="James: EddyPro corrected LE flux (black) and re-calculated LE flux (red)")


# Try using Israel's instructions:
  
flux1a$un_co2_flux
flux1a$co2_scf
E = flux1a$LE/lambda
labmda <- 10^3* (3147.5-2.37*flux1a$air_temperature)
flux1a$H
flux1a$co2_molar_density
mu <- 1.6077
pd = flux1a$air_density_dry <- (flux1a$air_pressure-flux1a$e)/(287*flux1a$air_temperature)
flux1a$water_vapor_density
flux1a$air_density
flux1a$air_heat_capacity
flux1a$air_temperature

mu <- 1.6077

labmda <- 10^3* (3147.5-2.37*flux1a$air_temperature)

pd = flux1a$air_density_dry <- (flux1a$air_pressure-flux1a$e)/(287*flux1a$air_temperature)

flux1a <- flux1a[, E:= LE/ (10^3 *(3147.5-2.37*air_temperature))]

# air pressure for the first ~half of data must be estimated by site elevation as it's a constant value
flux1a <- flux1a[ ,fco2_test := (un_co2_flux*co2_scf) +
                   mu*(E/air_density_dry)*
                   (co2_molar_density/(1+mu*(water_vapor_density/air_density_dry)))+
                   (H/(air_density*air_heat_capacity))*
                   (co2_molar_density/air_temperature)]

ggplot(flux1a, aes(co2_flux, fco2_test, colour=factor(qc_co2_flux)))+geom_point()+geom_abline(intercept=0,slope=1)

ggplot(flux1a) +
  geom_point(aes(date, air_pressure, colour=factor(qc_co2_flux)), size=0.3)
# EddyPro corrected CO2 flux (black) and re-calculated CO2 flux (red)
ggplot(flux1a) +
  geom_point(aes(date, co2_flux), color="black",size=0.3)+
  geom_point(aes(date, fco2_test),color="red", size=0.1)+
  ylim(-30,10)+
  labs(title="EddyPro corrected CO2 flux (black) and re-calculated CO2 flux (red)")
# EddyPro corrected CO2 flux (black) and uncorrected CO2 flux (red)
ggplot(flux1a) +
  geom_point(aes(date, co2_flux), colour="black", size=0.3)+
  geom_point(aes(date, un_co2_flux),color="red", size=0.1)+
  ylim(-30,10)+
  labs(title="EddyPro corrected CO2 flux (black) and uncorrected CO2 flux (red)")
# EddyPro corrected CO2 flux (black) and uncorrected CO2 flux * co2_scf (red)
ggplot(flux1a) +
  geom_point(aes(date, co2_flux), colour="black", size=0.3)+
  geom_point(aes(date, un_co2_flux*co2_scf),color="red", size=0.1)+
  ylim(-30,10)+
  labs(title="EddyPro corrected CO2 flux (black) and uncorrected CO2 flux * co2_scf (red)")

flux1a$w
