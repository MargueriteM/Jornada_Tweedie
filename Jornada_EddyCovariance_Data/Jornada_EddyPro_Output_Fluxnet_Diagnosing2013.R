############################################
#  This code gets data output by Eddy Pro  #
#           written by: M. Mauritz         #
#             May 2019                     #
#    update: 9 August 2019                 #
############################################

# Diagnose issues with 2013 data: missing LE! 

# load libraries
library(ggplot2) # library for making figures in ggplot package
library(plotly)
library(lubridate) # library for easier date manipulation 
library(data.table) # library for data table which is more efficient with large data sets
library(reader)
library(tidyr)
library(stringr) # allows string manipulation
library(lsr) # contains quantileCut function
library(gridExtra)
library(viridis)
library(zoo)
#############
# IMPORT DATA
#############
# EddyPro 7.0.4 has column name slip problems, using Fluxnet output!

# read the data in fluxnet format

flux2013 <- fread("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_2013/eddypro_JER_2013_fluxnet_2019-08-08T105511_adv.csv",
                  sep=",", header=TRUE, na.strings=c("-9999"))

# remove duplicate data
flux2013 <- (flux2013[!(duplicated(flux2013, by=c("TIMESTAMP_START")))])

# make sure the data are ordered:
flux2013 <- flux2013[order(TIMESTAMP_START),]

# format date
flux2013[,':=' (date_time_orig = parse_date_time(TIMESTAMP_END,"YmdHM",tz="UTC"))][
  ,':='(date_orig=as.Date.POSIXct(date_time_orig),month_orig=month(date_time_orig),year_orig=year(date_time_orig))]

# compare re-processing with no limits on H2O
flux2013nl <- fread("/Volumes/SEL_Data_Archive/Research Data/Desert/Jornada/Bahada/Tower/EddyCovariance_ts/2013/EddyPro_Out_TestProcessingParameters/eddypro_JER_2013_NoLimits_fluxnet_2022-11-08T210631_adv.csv",
                    sep=",", header=TRUE, na.strings=c("-9999"), fill=TRUE)

# remove duplicate data
flux2013nl <- (flux2013nl[!(duplicated(flux2013nl, by=c("TIMESTAMP_START")))])

# make sure the data are ordered:
flux2013nl <- flux2013nl[order(TIMESTAMP_START),]

# format date
flux2013nl[,':=' (date_time_orig = parse_date_time(TIMESTAMP_END,"YmdHM",tz="UTC"))][
  ,':='(date_orig=as.Date.POSIXct(date_time_orig),month_orig=month(date_time_orig),year_orig=year(date_time_orig))]


# graph CO2 fluxes with a ylim
ggplot(flux2013[FC_SSITC_TEST<2 & CUSTOM_AGC_MEAN<60,], aes(date_time_orig,FC,colour=(factor(FC_SSITC_TEST))))+
  geom_point()+
  ylim(c(-20,20))

# graph H2O fluxes with a ylim with quality flag
ggplot(flux2013[LE_SSITC_TEST<2 & CUSTOM_AGC_MEAN<60,], aes(date_time_orig,LE,colour=(factor(LE_SSITC_TEST))))+
  geom_point()+
  ylim(c(-500,1000))

# graph H2O fluxes with a ylim with no quality check
ggplot(flux2013[CUSTOM_AGC_MEAN<60,], aes(date_time_orig,LE,colour=(factor(LE_SSITC_TEST))))+
  geom_point()+
  ylim(c(-500,1000))

# graph CO2 and H2O
ggplot(flux2013[CUSTOM_AGC_MEAN<60,], aes(date_time_orig,CO2,colour=(factor(FC_SSITC_TEST))))+
  geom_point()

ggplot(flux2013[CUSTOM_AGC_MEAN<60,], aes(date_time_orig,H2O,colour=(factor(LE_SSITC_TEST))))+
  geom_point()

# zoom in to July
ggplot(flux2013[month(date_time_orig)==9 & CUSTOM_AGC_MEAN<60,], aes(date_time_orig,H2O,colour=(factor(LE_SSITC_TEST))))+
  geom_point()

# ISSUE: 
# H2O data is missing. LE is not missing due to quality flag

# July has barely any data passing through Eddy Pro, check in ts data 
ec_files <- list.files(path="/Volumes/SEL_Data_Archive/Research Data/Desert/Jornada/Bahada/Tower/EddyCovariance_ts/2013/Raw_Data/ASCII", full.names=TRUE) 
# read files and bind them into one file. fill=TRUE because of the missing columns in 2011

# coulmn names for 2013
ec2 <- do.call("rbind", lapply(ec_files[273], header = FALSE, fread, sep=",", skip = 4,fill=TRUE,
                               na.strings=c(-9999,"#NAME?"),
                               col.names=c("TIMESTAMP",
                                           "RECORD",
                                           "Ux",	"Uy",	"Uz",	"Ts",	"CO2",	"H2O",
                                           "fw",	"press",	"diag_csat","agc","t_hmp",	"e_hmp",
                                           "atm_press")))

ggplot(ec2[agc<60], aes(TIMESTAMP, H2O))+
  geom_line(size=0.1)


# Check to see if the issue is during the statistical test steps in Eddy Pro
# From Israel Begashaw at Licor

# Hello Marguerite,
# The absolute limit test works on the variables that are involved in the flux computation
# to filter out the unreasonable concentrations.
# The complication on the absolute limit test is that if you have data values in density
# (open path analzyers) EddyPro uses Standard Temperature and Pressure to convert them to
# mole fraction just for the initial pass filtering. This is done for computational efficiency
# to be able to filter the variable on its own. However, some values that are within the
# range for the measured temperature and pressure may get computed to un-reasonable mole
# fraction when using standard temperature and pressure or the other way around.
# Typically this happens only on H2O and not CO2, because the CO2 range is wider.
# Because of the nature of the filtering I wouldn’t automatically reject CO2 fluxes
# for the periods where H2O is filtered out. I would look at other parameters,
# such as quality flags to make sure it is valid.
# The H2O filtering can be tweaked to accommodate potentially high humidity conditions.
# 
# Thanks,
# 
# 
# Israel Begashaw
# Sr. Science and Support Manager 


# Good Afternoon Israel,
# 
# I was hoping I could bother you with some follow up questions. I have been able to run the data for the year without the absolute limits test, and as you suspected, the H2O data was being removed in this step (see graphs below). The high values do align with the onset of the monsoon season (we are in a dry shrubland site near Las Cruces, New Mexico).
# How do I determine a 'reasonable' mole fraction value for H2O?
#   
#   I wasn't able to find much, but did find this graph of percent water in air by temperature and RH . If I can treat this as a reliable source, if I very roughly assume an average air temp of 30C and RH between 25-80, then a reasonable value of H2O mmol/mol would be approximately between 10-30mmol/mol.
# 
# Does this conjecturing seem reasonable to you or do you have another suggestion (or a better source I should read!)?
# 
# Many thanks,
# Marguerite



# Hi Mauritz
# 
# Typically H2O concentration in the air can range for 0 to 4% and this translates to a H2O mmol/mol of 0 to 40%.
# 
# Typically values close to 40 are observed in tropical environments only. The absolute limit test in EddyPro uses a standard temperature (25°C) and pressure(100 kPa)  to convert density to mole fraction  to filter values and so this conversion may not be accurate always. The reason is because at that stage of processing EddyPro does no have access to good temperature measurement such as from the biomet file. So as long as you are aware that the values you have is in the reasonable range (0 to 40 mmol/mol), there is no need to do the absolute filtering, if it is removing a lot of data.
# 
# The Foken QC flags from Eddypro (0,1,2) are not related to absolute limits , but more towards whether you have enough turbulence or whether the data does not have large trends in it. So it mostly uses the wind data and trends in the scalar data. you could also get large spikes in the data because of high AGC/low signal strength, so one think you could do is to filter the 30 minute values based on whether the AGC was below a threshold (below 70).
# 
# The H2O and Co2 mole fractions you have shown below are all within the normal observable ranges, so skipping the option to filter data based on absolute limit tests would be fine here.
# 
# 
# 
# Thanks
# 
# James


