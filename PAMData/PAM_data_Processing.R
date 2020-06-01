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
library(lubridate) # this one is for working with dates

# Set the working directory to the file location where the data files are stored
setwd("~/Desktop/OneDrive - University of Texas at El Paso/MiniPAM")

# create a vector of easy names to use for the data columns
easy.colnames <- c("Date","Time","Type","Number","F","Fm_prime","PAR","Y_II","ETR","Temp","Fo_prime","ETR_F",
                   "qP","qN","qL","NPQ","Y_NO","Y_NPQ","Fo","Fm","Fv_Fm","LcNo")

# read the compiled tab from the individual excel files (uses readxl package from tidyverse)
dat.2012.2013 <- read_excel("2012-2013_Raw_Data.xlsx", sheet="compilation", na=c("NA","-"))

# import the LcNo
lcno.2012.2013 <- read_excel("2012-2013_Raw_Data.xlsx", sheet="LcNo", na=c("NA","-"))


# look at the file to see how it imported: 
# check column names
colnames(dat.2012.2013)

# change the column names to somehting easiery
dat.2012.2013 <- dat.2012.2013 %>% rename_at(vars(colnames(dat.2012.2013)), ~easy.colnames)


# look at the first 3 rows
head(dat.2012.2013, n=3)

# the time column is importing in a weird format. I think it could be annoying to fix, and right now time isn't too important
# so I'm going to ignore it.

# check the structure of the data
str(dat.2012.2013)

# merge the individual plant information with the data, using the common LcNo column to connect them
dat.2012.2013 <- merge(dat.2012.2013,lcno.2012.2013, by="LcNo")
str(dat.2014)

# import data from 2014
dat.2014 <- read_excel("2014_Raw_Data.xlsx", sheet="Compilation", na=c("NA","-"))

# import LcNo
# lcno.2014 <- ...

# merge dat.2014 and LcNo
# ....


# join data from multiple years

# to make light response curves we need to have only the data
# Therefore select only rows with FO and F (those are the data observations)
# Then graph every individual observation
# assign all this to an object

# the %>% is called a 'pipe' and it lets you perform multiple steps after each other

dat.filter1 <- filter(dat.2012.2013,Type=="FO"|Type=="F") %>%
  ggplot(.,aes(`1:PAR`,`1:ETR`, colour=LcNo))+geom_point()

# add more info: 
# filter the data to show only August, show the LcNo as a factor, separate individual LcNo, graph as lines
dat.filter <- filter(dat.2012.2013,(Type=="FO"|Type=="F")) %>%
  ggplot(.,aes(`1:PAR`,`1:ETR`, colour=factor(LcNo)))+geom_line()+
  facet_wrap(.~month(Date, label=TRUE,abbr=TRUE))+
  theme(legend.position = "none")

# show the figure by calling the object we created
dat.filter


# use the info from LcNo to plot the data by species
dat.species <- filter(dat.2012.2013,(Type=="FO"|Type=="F")) %>%
  ggplot(.,aes(`1:PAR`,`1:ETR`, colour=factor(LcNo)))+geom_line()+
  facet_grid(Spp~month(Date, label=TRUE,abbr=TRUE))+
  theme(legend.position = "none")

# show the figure by calling the object we created
dat.species

