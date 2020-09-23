

# This script takes the light response parameters (ETRmax) and (ETRopt) from PAM_data_Processing_20200818.R
# We will import the extracted parameters with LcNo and species information so that we can start with a clean script

# load libraries
library(tidyverse)
# alternative: 
# library(dplyr)
# library(ggplot2)
library(lubridate)

# load data from directory
# the data is in .R objects which are R-specific data objects
setwd("~/Desktop/OneDrive - University of Texas at El Paso/MiniPAM/R_code_data")

# dat.filter5
# this is all the LcNo data, filtered
load("PAM_dat_filter5.R")

# test.multi.all
# these are the model fit parameters
load("PAM_dat_filter5_modelfit.R")

# params.long
# these are the coefficients (ETRopt and ETRmax) extracted from the line fits
load("PAM_dat_filter5_coefficients.R")
 
# the params.long is the data we want to use
# let's look at the columns in there
params.long %>% glimpse()

# LcNo is the light curve number (now we have only one row per LcNo because we got only the ETRmax and ETRopt values)
# ETRmax.start is the ETRmax for each LcNo. It is the estimated photosynthetic optimum for the leaf
# ETRopt.start is the ETRopt for each LCNo. It is the PAR level at which the optimum is found
# Date is the date of sampling
# Spp is the species
# Posn is the location on the plant (have to check excel datasheets for code, I think T is top, L is lower)
# Age is an estimate of the leave age.... I think B might be bud, Y = young, O = old
# PlantID is the individual plant that was measured (there are ~5 plants per species, I think)


# Now we can start to make some graphs to look at the patterns of ETRmax.start
# There are some really high values of ETRmax.start so I filter those out (filter for ETRmax.start<1000)

# first let's break it down to see every individual plant by species. 
# graph by day of the year and show each year seperately
params.long %>% 
  filter(ETRmax.start<1000 & Spp=="LATR") %>%
  ggplot(., aes(yday(Date), ETRmax.start))+
         geom_point() +
         geom_line() + 
           facet_grid(PlantID~year(Date))

# I notice that some individuals had the wrong species code in a few years!!! 
 # I can fix it        
params.long <- params.long %>%
  mutate(Spp = case_when(PlantID %in% c("Prgl2","Prgl5")& Spp=="LATR" ~ "PRGL",
                         TRUE ~ as.character(Spp)))


# and then graph again: 
params.long %>% 
  filter(ETRmax.start<1000 & Spp=="LATR") %>%
  ggplot(., aes(yday(Date), ETRmax.start))+
  geom_point() +
  geom_line() + 
  facet_grid(PlantID~year(Date))

# intead of splitting by year we could also graph it continuously, like this: 
# I think that in the begining it's easier to have it the other way, with a box for each year. 
params.long %>% 
  filter(ETRmax.start<1000 & Spp=="LATR") %>%
  ggplot(., aes(Date, ETRmax.start))+
  geom_point() +
  geom_line() + 
  facet_grid(PlantID~.)

# Make a figure like this for each species by changing Spp==
# keep the ETRmax.start<1000 filter! 


 