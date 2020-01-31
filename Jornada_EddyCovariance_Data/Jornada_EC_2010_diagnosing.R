
# look at 2010 full output to see if I can figure out any issues
# Jason Hupp suggested: check flag criteria like skewness, kurtosis

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
fileinfo <- scan("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_2010/eddypro_JER_2010_full_output_2019-08-08T131210_adv.csv",
                 what='',sep=",",nlines=1)
fileinfo <- data.table(t(fileinfo))
# read only the first row to get the units
flux.units <- (fread("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_2010/eddypro_JER_2010_full_output_2019-08-08T131210_adv.csv",header=TRUE,skip=1))[1,]

# read the data, skippping the units row
flux1 <- fread("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_2010/eddypro_JER_2010_full_output_2019-08-08T131210_adv.csv", sep=",",skip=3,
               header=FALSE, na.strings=c("-9999","-9999.0","NAN","#NAME?"),col.names=colnames(flux.units))

ggplot(flux1, aes(DOY, skewness_kurtosis_hf))+geom_line()
ggplot(flux1[!is.na(co2_flux),], aes(DOY, skewness_kurtosis_hf, colour=factor(qc_co2_flux)))+
  geom_point()

flux1[,co2_flux2 := co2_flux]
flux1[is.na(co2_flux2), co2_flux2:=-9999]


ggplot(flux1, aes(skewness_kurtosis_hf, co2_flux2))+geom_point()

ggplot(flux1, aes(DOY, co2_flux2, colour=skewness_kurtosis_hf))+geom_point()

ggplot(flux1)+
  geom_line(aes(DOY, co2_mole_fraction), colour="green")+
  geom_line(aes(DOY, h2o_mole_fraction), colour="blue")

