####################################################
#      R code for processing MiniPAM data          #
#        written by: Marguerite Mauritx            #
#                  and Maria Franco Saenz          #
#        June 1, 2020                              #
####################################################

# This code will: 
# 1. import annual data from compiled tab in annual Excel sheets
# 2. plot and fit light response curves for each individual
# 3. extract useful parameters from light response curves
# 4. explore patterns between species and time (Ie: seasons, years)

# Load libraries
library(tidyverse) 

# Set the working directory to the file location where the data files are stored
setwd("~/Desktop/OneDrive - University of Texas at El Paso/MiniPAM")

# read the compiled tab from the individual excel files (uses readxl package from tidyverse)
dat.2012.2013 <- read_excel("2012-2013_Raw_Data.xlsx", sheet="compilation", na=c("NA","-"))

# look at the file to see how it imported: 
# check column names
colnames(dat.2012.2013)

# look at the first 3 rows
head(dat.2012.2013, n=3)

# the time column is importing in a weird format. I think it could be annoying to fix, and right now time isn't too important
# so I'm going to ignore it.

# check the structure of the data
str(dat.2012.2013)


dat.2014 <- read_excel("2014_Raw_Data.xlsx", sheet="Compilation", na=c("NA","-"))

str(dat.2014)



The following untracked working tree files would be overwritten by merge:
  Jornada_Tweedie.Rproj
Please move or remove them before you merge.
Aborting


