######################################
#     Prepare daily met data for     #
#     D.Browning LTAR Phenocam-flux  #
#      synthesis                     #
#     (Daily 2017/2018 data)         #
#       written by: M. Mauritz       #
#                                    #
#     28 June 2019                   #
######################################

# data loaded from Eddy Pro Biomet data prep in: Jornada_EnvironmentalData_Combine.R
# metadata and data templates at: ftp://ltar_phenology:ltphenar401@jornada-ftp.nmsu.edu (v2.2 templates)

# LTAR site name: jerbajada

# DATE: yyyy-mm-dd
# TMAX_DAILY_DEGC
# TMIN_DAILY_DEGC
# TMEAN_DAILY_DEGC
# PPT_DAILY_MM
# RH_DAILY_MEAN
# RH_DAILY_SD (std of daily values)
# FLAG

setwd("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/MetDataFiles_EP")

load("Biomet_EddyPro_2010_2019_20190626.Rdata")


