####################################################
#      R code for processing MiniPAM data          #
#        written by: Marguerite Mauritz            #
#                  and Maria Franco Saenz          #
#        June 1, 2020                              #
####################################################

# This code will: 
# 1. import weekly data from compiled tab in annual Excel sheets
# 2. plot and fit light response curves for each individual
# 3. extract useful parameters from light response curves
# 4. explore patterns between species and time (Ie: seasons, years)

# Load libraries
library(tidyverse) 
library(lubridate) # this one is for working with dates
library(readxl)
library(dplyr)
library(plyr)
library(data.table)
#install.packages("dplyr")
# install.packages("tidyverse")

# Set the working directory to the file location where the data files are stored
setwd("~/Desktop/OneDrive - University of Texas at El Paso/MiniPAM")
setwd("C:/Users/maria/University of Texas at El Paso/Mauritz-Tozer, Marguerite E - MiniPAM")


# create a vector of easy names to use for the data columns
easy.colnames <- c("Date","Time","Type","Number","F","Fm_prime","PAR","Y_II","ETR","Temp","Fo_prime","ETR_F",
                   "qP","qN","qL","NPQ","Y_NO","Y_NPQ","Fo","Fm","Fv_Fm","LcNo")

# read the compiled tab from the individual excel files (uses readxl package from tidyverse)
dat.2012.2013 <- read_excel("2012-2013_Raw_Data.xlsx", sheet="compilation", na=c("NA","-"))
dat.2014 <- read_excel("2014_Raw_Data.xlsx", sheet="Compilation", na=c("NA","-"))
dat.2015 <- read_excel("2015_Raw_Data.xlsx", sheet="compilation", na=c("NA","-"))
dat.2016 <- read_excel("2016_Raw_Data.xlsx", sheet = "compilation", na=c("NA","-"))
dat.2017 <- read_excel("2017_Raw_Data_complete.xlsx", sheet = "Compilation", na=c("NA","-"))
dat.2018 <- read_excel("2018_Raw_Data.xlsx", sheet="Compilation", na=c("NA","-"))
dat.2019 <- read_excel("2019_Raw_Data_complete.xlsx", sheet="Compilation", na=c("NA","-"))
dat.2020 <- read_excel("2020_Raw_Data.xlsx", sheet="Compilation", na=c("NA","-"))

# import the LcNo_Compilation 2012-2013
lcno.2012.2013 <- read_excel("2012-2013_Raw_Data.xlsx", sheet="LcNo_Compilation", na=c("NA","-"))
lcno.2014 <-read_excel("2014_Raw_Data.xlsx", sheet="LcNo_Compilation", na=c("NA","-"))
lcno.2015 <-read_excel("2015_Raw_Data.xlsx", sheet="LcNo_Compilation", na=c("NA","-"))
lcno.2016 <-read_excel("2016_Raw_Data.xlsx", sheet="LcNo_Compilation", na=c("NA","-"))
lcno.2017 <-read_excel("2017_Raw_Data_complete.xlsx", sheet="LcNo_Compilation", na=c("NA","-"))
lcno.2018 <-read_excel("2018_Raw_Data.xlsx", sheet="LcNo_Compilation", na=c("NA","-"))
lcno.2019 <-read_excel("2019_Raw_Data_complete.xlsx", sheet="LcNo_Compilation", na=c("NA","-"))
lcno.2020 <-read_excel("2020_Raw_Data.xlsx", sheet="LcNo_Compilation", na=c("NA","-"))

# look at the file to see how it imported: 
# check column names
colnames(dat.2012.2013)
colnames(dat.2014)
colnames(dat.2015)
colnames(dat.2016)
colnames(dat.2017)
colnames(dat.2018)
colnames(dat.2019)
colnames(dat.2020)

# change the column names to something easier

dat.2012.2013 <- dat.2012.2013 %>% rename_at(vars(colnames(dat.2012.2013)), ~easy.colnames)
dat.2014 <- dat.2014 %>% rename_at(vars(colnames(dat.2014)), ~easy.colnames)
dat.2015 <- dat.2015 %>% rename_at(vars(colnames(dat.2015)), ~easy.colnames)
dat.2016 <- dat.2016 %>% rename_at(vars(colnames(dat.2016)), ~easy.colnames)
dat.2017 <- dat.2017 %>% rename_at(vars(colnames(dat.2017)), ~easy.colnames)
dat.2018 <- dat.2018 %>% rename_at(vars(colnames(dat.2018)), ~easy.colnames)
dat.2019 <- dat.2019 %>% rename_at(vars(colnames(dat.2019)), ~easy.colnames)
dat.2020 <- dat.2020 %>% rename_at(vars(colnames(dat.2020)), ~easy.colnames)


# the time column is importing in a weird format. I think it could be annoying to fix, and right now time isn't too important
# so I'm going to ignore it.

# check the structure of the data
str(dat.2012.2013)
str(dat.2014)
str(dat.2015)
str(dat.2016)
str(dat.2017)
str(dat.2018)
str(dat.2019)
str(dat.2020)


# merge the individual plant information with the data, using the common LcNo column to connect them
dat.2012.2013.merge <- merge(dat.2012.2013,lcno.2012.2013, by="LcNo")
dat.2014.merge <-merge(dat.2014, lcno.2014, by="LcNo")
dat.2015.merge <-merge(dat.2015, lcno.2015, by="LcNo")
dat.2016.merge <-merge(dat.2016, lcno.2016, by="LcNo")
dat.2017.merge <-merge(dat.2017, lcno.2017, by="LcNo")
dat.2018.merge <-merge(dat.2018, lcno.2018, by="LcNo")
dat.2019.merge <-merge(dat.2019, lcno.2019, by="LcNo")
dat.2020.merge <-merge(dat.2020, lcno.2020, by="LcNo")

# Removing Tag_id from 2019 and 2020
dat.2019.merge <- dat.2019.merge %>% mutate(`TAG ID` = NULL)
dat.2020.merge <- dat.2020.merge %>% mutate(`TAG ID` = NULL)

#Merge all of the data
dat.all <- rbind(dat.2012.2013.merge,dat.2014.merge,dat.2015.merge,
                 dat.2016.merge,dat.2017.merge,dat.2018.merge,
                 dat.2019.merge,dat.2020.merge)

summary(dat.all)

#Check PlantID 
levels(as.factor(dat.all$PlantID))

#Function to change letters to Uppercase
#https://stackoverflow.com/questions/18509527/first-letter-to-upper-case
firstup <- function(x) {
  x <- tolower(x)
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
#Replacing PlantID error 
dat.all <- dat.all %>% mutate(PlantID = firstup(PlantID))%>% 
  mutate(PlantID=ifelse(PlantID=="Prlg5","Prgl5",PlantID))
  

 
# to make light response curves we need to have only the data
# Therefore select only rows with FO and F (those are the data observations)
# Then graph every individual observation
# assign all this to an object

# the %>% is called a 'pipe' and it lets you perform multiple steps after each other

# | is 'or'
# & is 'and'
# == is "exact match"

#Histogram of PAR values
ggplot(dat.filter, aes(PAR))+geom_histogram()

# select only data to use for light response curves and remove really high out-lying values
dat.filter <- dat.all %>% filter(Type=="FO"|Type=="F")%>%
  filter(PAR<6000, ETR<800)

# remove duplicates in LcNo, PAR, ETR (these are duplicate entries)
dat.filter <- dat.filter %>% 
  distinct(LcNo,PAR,ETR, .keep_all=TRUE)


# graph all the data by year and month just to see an overview
dat.filter %>% 
  ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
  facet_grid(year(Date)~month(Date))+
  theme(legend.position = "none")+
  labs(title="dat.filter: high values and duplicates removed")

# a few issues we notice: 
# * May 2018 to July 2019 all curves look flat (think it's an insturment malfunction)
# * some curves look completely flat
# * some curves look very spiky or have really high values at the end


# There are a number of flat curves. Try to filter those out of the data. 
# First, let's look at some of the situations where we have flat curves

# all species 5-2018 to 7-2019
mupo.flat <- dat.filter %>% filter(Spp=="MUPO" & (year(Date)==2018 & month(Date)>=5 |
                                                    year(Date)==2019 & month(Date)<=7))%>%
  ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
  facet_grid(year(Date)~month(Date))+
  theme(legend.position = "none")+
  labs(title="MUPO")
# show the figure by calling the object we created
mupo.flat


latr.flat <- dat.filter %>% filter(Spp=="LATR" & (year(Date)==2018 & month(Date)>=5 |
                                     year(Date)==2019 & month(Date)<=7) &ETR<10 )%>%
  ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
  facet_grid(year(Date)~month(Date))+
  theme(legend.position = "none")+
  labs(title="LATR")
# show the figure by calling the object we created
latr.flat


flce.flat <- dat.filter %>% filter(Spp=="FLCE" & (year(Date)==2018 & month(Date)>=5 |
                                                    year(Date)==2019 & month(Date)<=7))%>%
  ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
  facet_grid(year(Date)~month(Date))+
  theme(legend.position = "none")+
  labs(title="FLCE")
# show the figure by calling the object we created
flce.flat

prgl.flat <- dat.filter %>% filter(Spp=="PRGL" & (year(Date)==2018 & month(Date)>=5 |
                                                    year(Date)==2019 & month(Date)<=7))%>%
  ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
  facet_grid(year(Date)~month(Date))+
  theme(legend.position = "none")+
  labs(title="PRGL")
# show the figure by calling the object we created
prgl.flat


# It looks like the instrument was malfuncitoning
# from week of April 27 2018 until the week of August 15th 2019
# remove data from this entire period

dat.filter2 <- dat.filter %>% 
  filter(Date<as.Date("2018-04-27") | Date>as.Date("2019-08-15"))


# PLOT to check whether data was removed correctly
dat.filter2 %>% filter(Spp=="PRGL")%>%
  ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
  facet_grid(year(Date)~month(Date))+
  theme(legend.position = "none")+
  labs(title="PRGL")


# there are still some dispersed periods of bad data
# Identify these using four additional approaches: 
# 1. remove light curves that have less than 6 data points (dat.filter3)
# 2. fit a linear straight-line and remove curves with 0 or negative slope parameters (dat.filter4)
# 3. fit a loess smoother and remove curves that cannot be fitted and remove points with very high outliers (loess.check)
# 4. fit a quadratic function (y = x + x^2) which will fit the data reasonably well & remove large outlier points (dat.filter5)
#    std.residuals doesn't work well for this stage because they vary systematically with PAR and all the ranges overlap
# * if a curve fitting fails remove entire LcNo record
# * if a curve has some outlying points, remove only the outliers as the general shape might still be good

# count the total number of records by light curve
count.records <- dat.filter2 %>% group_by(LcNo) %>% tally()

dat.filter2 <- dat.filter2 %>% left_join(count.records,by="LcNo")

# look at how many numbers there are
ggplot(count.records,aes(LcNo,n))+geom_point()

# get the LcNo with all the metadata to merge to lc/lm model parameters
lcno.filter2 <- dat.filter2 %>% 
  distinct(LcNo, .keep_all=TRUE)%>%
  select(LcNo,Date,Spp,Posn,Age,PlantID)

# look at the Light curves with more or less than 9 records (the typical number)
# records that are less than 6 don't give good curves
# records more than 10 might have an error with the LcNo copy-paste in the excel sheets.
dat.filter2 %>% filter(n==9) %>%
  ggplot(.,aes(PAR,ETR,colour=factor(LcNo)))+
  geom_line()+
  theme(legend.position="none")

# remove LcNo with less then 6 records
dat.filter3 <- dat.filter2 %>% 
  filter(n>6)

# fit a straight line to all light curves
lc.lm <- dat.filter3 %>%
  group_by(LcNo) %>%
  do(broom::tidy(lm(ETR~PAR,.)))

# extract only intercept and slope parameters and put in wide format
lc.lm.long <- lc.lm %>%
  select(LcNo,term,estimate)%>%
  pivot_wider(id_cols=LcNo,names_from=term,values_from = estimate,
              names_prefix="lm.")

# # get residuals
# lc.lm.resid <- dat.filter2 %>%
#   group_by(LcNo) %>%
#   do(broom::augment(lm(ETR~PAR,.)))


# combine the linear check (only the LcNo, lm.PAR, and lm.(Intercept)) with data
linear.check <- merge(lc.lm.long,lcno.filter2, by="LcNo")

# and add residuals (to see if that can remove spiky curves)
dat.filter4 <- linear.check %>% 
  select(LcNo,lm.PAR,`lm.(Intercept)`) %>%
  left_join(dat.filter3,by="LcNo") 


# look at how the light curves looks depending on the linear slope
dat.filter4 %>%
 # filter(Spp=="MUPO" & lm.PAR<=0)%>% # remove slopes less or equal to 0
 # filter(Spp=="MUPO" & lm.PAR>(0)&lm.PAR<0.2)%>% # keep all of these (some still aren't great but they might come out at later steps)
#  filter(Spp=="MUPO" & lm.PAR>=0.2)%>%
  
ggplot(.,aes(PAR,ETR,colour=factor(LcNo)))+
  geom_line()+
  facet_grid(year(Date)~month(Date))+
  theme(legend.position="none")


dat.filter4 %>%
  # filter(Spp=="LATR" & lm.PAR<=0 )%>% # remove all
   filter(Spp=="LATR" & lm.PAR>(0)&lm.PAR<0.2)%>% # keep all of these (some still aren't great but they might come out at later steps)
  # filter(Spp=="LATR" & lm.PAR>=0.11)%>%
  
  ggplot(.,aes(PAR,ETR,colour=factor(LcNo)))+
  geom_line()+
  facet_grid(year(Date)~month(Date))+
  theme(legend.position="none")

dat.filter2 %>%
  # filter(Spp=="FLCE" & lm.PAR<=0 )%>% # remove all
   filter(Spp=="FLCE" & lm.PAR>(0)&lm.PAR<0.2)%>% # keep all of these (some still aren't great but they might come out at later steps)
  # filter(Spp=="FLCE" & lm.PAR>=0.17)%>%
  
  ggplot(.,aes(PAR,ETR,colour=factor(LcNo)))+
  geom_line()+
  facet_grid(year(Date)~month(Date))+
  theme(legend.position="none")

dat.filter2 %>%
 # filter(Spp=="PRGL" & lm.PAR<=0 )%>% # remove all
 filter(Spp=="PRGL" & lm.PAR>(0)&lm.PAR<0.2)%>% # keep all of these (some still aren't great but they might come out at later steps)
  # filter(Spp=="PRGL" & lm.PAR>=0.17)%>%
  
  ggplot(.,aes(PAR,ETR,colour=factor(LcNo)))+
  geom_line()+
  facet_grid(year(Date)~month(Date))+
  theme(legend.position="none")

# remove lm.PAr (linear slope) <=0 for all species
dat.filter4 <- dat.filter4 %>%
  filter(lm.PAR>0)

# after removing flat curves, fit loess and then 2nd degree polynomial 
# fit loess
lc.lm.loess <- dat.filter4 %>%
  group_by(LcNo) %>%
  do(broom::augment(loess(ETR~PAR,.)))

# merge with the LcNo metadata
loess.check <- merge(lc.lm.loess,lcno.filter2, by="LcNo")

# find where the resids are NA and filter out LcNo when resids are NA, 
# many of these had faulty readings like all the ETR values stacked at a single PAR levels
loess.check <- loess.check %>% 
  dplyr::group_by(LcNo) %>%
  dplyr::mutate(nacheck = is.na(`.resid`))

# graph the LcNo that have NA in the loess residuals
loess.check %>% 
  dplyr::filter(any(nacheck==TRUE)) %>%
  ggplot(.)+
  geom_point(aes(PAR,ETR,colour=factor(LcNo)))+
  geom_line(aes(PAR,ETR,colour=factor(LcNo)))+
  theme(legend.position="none")#+
facet_wrap(.~LcNo)

# remove those with NA in the loess residuals
# and calculate max/min residuals by group
loess.check <- loess.check %>% 
  dplyr::group_by(LcNo)%>%
  dplyr::filter(any(nacheck==FALSE)) %>%
  dplyr::mutate(max.resid = max(`.resid`),
                min.resid = min(`.resid`)) 

# look at the distribution of loess residuals
loess.check %>% 
  ggplot(.)+
  geom_density(aes(`.resid`))

loess.check %>% 
  filter(`.resid`>(10)) %>%
  ggplot(.)+
  geom_density(aes(`.resid`))


# look at the curves for different levels or residuals
loess.check %>% 
  filter(abs(`.resid`)<10 ) %>%
  ggplot(.)+
  geom_line(aes(PAR,ETR,colour=factor(LcNo)))+
  #geom_smooth(aes(PAR,ETR,colour=factor(LcNo)),alpha=0)+
  facet_grid(year(Date)~month(Date))+
  theme(legend.position="none")

# count the number of points in curves with residual values less than 10 distance (ie: >+ 10 or < -10)
count.loess.10 <- loess.check %>%
  filter(abs(`.resid`)<10 ) %>%
group_by(LcNo) %>% tally()

loess.check <- merge(loess.check,count.loess.10,by="LcNo")

# graph
loess.check %>% 
  filter(abs(`.resid`)<10 & n>=6) %>%
  ggplot(.)+
  geom_line(aes(PAR,ETR,colour=factor(LcNo)))+
  #geom_smooth(aes(PAR,ETR,colour=factor(LcNo)),alpha=0)+
  facet_grid(year(Date)~month(Date))+
  theme(legend.position="none")

# there's something going on in 2017, look more closely
loess.check %>% 
  filter(abs(`.resid`)<10 & n>=6 & year(Date)==2017 & month(Date)==12) %>%
  ggplot(.) +
  # geom_line(aes(PAR,`.resid`,colour=factor(LcNo)))+
  geom_point(aes(PAR,ETR,colour=factor(LcNo)),size=1)+
  geom_line(aes(PAR,ETR,colour=factor(LcNo)),size=0.5)+
  facet_wrap(LcNo~.) +
  #facet_wrap(as.Date(Date)~.) +
  #facet_grid(Spp~month(Date))+
  #geom_hline(yintercept=c(-50,50))+
  theme(legend.position="none")

# 2017 LcNo with spikes in the middle, that also look like problematic measurements/curves
# 5105, 5319, 5405, 5480, 5491, 5447, 5448, 5451, 5542, 5571,
# 5536, 5591, 5612, 5631, 5633, 5658, 5698, 5671, 5737, 5746

# graph with dodgy curves excluded
loess.check %>% 
  filter(abs(`.resid`)<10 & n>=6 & 
           !(LcNo %in% c(5105, 5319, 5405, 5480, 5491, 5447, 5448, 5451, 5542, 5571,
                         5536, 5591, 5612, 5631, 5633, 5658, 5698, 5671, 5737, 5746))) %>%
  ggplot(.) +
  # geom_line(aes(PAR,`.resid`,colour=factor(LcNo)))+
  geom_point(aes(PAR,ETR,colour=factor(LcNo)),size=1)+
  geom_line(aes(PAR,ETR,colour=factor(LcNo)),size=0.5)+
  #facet_wrap(LcNo~.) +
  #facet_wrap(as.Date(Date)~.) +
  facet_grid(Spp~month(Date))+
  #geom_hline(yintercept=c(-50,50))+
  theme(legend.position="none")


# Remove points that have residual values greater than 10 (+/-)
# and remove entire LcNO if less than 6 records remain after removing residuals
# fit polynomial to the data filtered with LOESS
# because the Loess is too flexible
lc.lm.poly2 <- loess.check %>%
 dplyr::filter(abs(`.resid`)<10 & n>=6 & 
          !(LcNo %in% c(5105, 5319, 5405, 5480, 5491, 5447, 5448, 5451, 5542, 5571,
                        5536, 5591, 5612, 5631, 5633, 5658, 5698, 5671, 5737, 5746))) %>%
  dplyr::select(LcNo,PAR,ETR) %>%
  dplyr::group_by(LcNo) %>%
 # do({fit = broom::tidy(lm(ETR~poly(PAR,2),.))})
  do({fit = lm(ETR~poly(PAR,2),.)
   broom::augment(fit,.)})

# plot all residuals
lc.lm.poly2 %>% 
  ggplot(.,aes(PAR,`.resid`))+geom_point()

lc.lm.poly2 <- merge(lc.lm.poly2,lcno.filter2, by="LcNo")

# look at fits
lc.lm.poly2 %>%
  filter(Spp=="LATR" & year(Date)==2015 &month(Date)==8)%>%
  ggplot(.) +
  geom_point(aes(PAR,ETR))+
  geom_line(aes(PAR,`.fitted`))+
  facet_wrap(.~LcNo)


# look at residuals
lc.lm.poly2 %>%
  filter(abs(`.resid`)<100) %>% #& !(LcNo %in% c(1623,1637)))%>%
  #filter(Spp=="LATR")%>%
  ggplot(.) +
  # geom_point(aes(ETR,`.resid`,colour=factor(LcNo)))+
   geom_line(aes(PAR,ETR,colour=factor(LcNo)))+
  facet_grid(year(Date)~month(Date))+
  #geom_hline(yintercept=c(-50,50))+
  theme(legend.position="none")

# use this as the final data. Find the LcNo I want to keep.
# LcNo 1623 and 1837 were both from January 2014 and they have bad curves but aren't quite
# being picked up by the curve fitting... because they fit the flexible/non-specific 
# linear, smooth, and polynimial that I used. They don't show a 'light-curve' response though.
lcno.poly2.keep <- lc.lm.poly2 %>%
  filter(any(abs(`.resid`)<100) & !(LcNo %in% c(1623,1637)))%>%
  distinct(LcNo)

dat.filter5 <- dat.filter4 %>%
  filter(LcNo %in% lcno.poly2.keep$LcNo)

dat.filter5 %>%
  ggplot(.) +
  # geom_point(aes(ETR,`.resid`,colour=factor(LcNo)))+
  geom_line(aes(PAR,ETR,colour=factor(LcNo)))+
  facet_grid(year(Date)~month(Date))+
  #geom_hline(yintercept=c(-50,50))+
  theme(legend.position="none")


#


                            #AUGUST#
# #########################################
# #2012-2013
# # make a graph where we look only at August and colour by PlantID
# dat.August.2012.2013 <- filter(dat.2012.2013.merge,(Type=="FO"|Type=="F") & month(Date)==8) %>%
#   ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
#   facet_grid(Spp~as.Date(Date))+
#   theme(legend.position = "none")
# 
# # show the figure by calling the object we created
# dat.August.2012.2013
# 
# #2014
# # make a graph where we look only at August and colour by PlantID
# dat.August.2014 <- filter(dat.2014.merge,(Type=="FO"|Type=="F") & month(Date)==8) %>%
#   ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
#   facet_grid(Spp~as.Date(Date))+
#   theme(legend.position = "none")
# 
# # show the figure by calling the object we created
# dat.August.2014
# 
# #2015
# # make a graph where we look only at August and colour by PlantID
# dat.August.2015 <- filter(dat.2015.merge,(Type=="FO"|Type=="F") & month(Date)==8) %>%
#   ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
#   facet_grid(Spp~as.Date(Date))+
#   theme(legend.position = "none")
# 
# # show the figure by calling the object we created
# dat.August.2015
# 
# #2016
# # make a graph where we look only at August and colour by PlantID
# dat.August.2016 <- filter(dat.2016.merge,(Type=="FO"|Type=="F") & month(Date)==8) %>%
#   ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
#   facet_grid(Spp~as.Date(Date))+
#   theme(legend.position = "none")
# 
# # show the figure by calling the object we created
# dat.August.2016
# 
# #2017
# # make a graph where we look only at August and colour by PlantID
# dat.August.2017 <- filter(dat.2017.merge,(Type=="FO"|Type=="F") & month(Date)==8) %>%
#   ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
#   facet_grid(Spp~as.Date(Date))+
#   theme(legend.position = "none")
# 
# # show the figure by calling the object we created
# dat.August.2017
# 
# #2018
# # make a graph where we look only at August and colour by PlantID
# dat.August.2018 <- filter(dat.2018.merge,(Type=="FO"|Type=="F") & month(Date)==8) %>%
#   ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
#   facet_grid(Spp~as.Date(Date))+
#   theme(legend.position = "none")
# 
# # show the figure by calling the object we created
# dat.August.2018
# 
# #2019
# # make a graph where we look only at August and colour by PlantID
# dat.August.2019 <- filter(dat.2019.merge,(Type=="FO"|Type=="F") & month(Date)==8) %>%
#   ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
#   facet_grid(Spp~as.Date(Date))+
#   theme(legend.position = "none")
# 
# # show the figure by calling the object we created
# dat.August.2019
# 
# #2020
# # make a graph where we look only at August and colour by PlantID
# dat.August.2020 <- filter(dat.2020.merge,(Type=="FO"|Type=="F") & month(Date)==8) %>%
#   ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
#   facet_grid(Spp~as.Date(Date))+
#   theme(legend.position = "none")
# 
# # show the figure by calling the object we created
# dat.August.2020
# 
# 
# # make a graph where we look only at August and colour by PlantID
# dat.August.2012.2013 <- filter(dat.2012.2013.merge,(Type=="FO"|Type=="F") & month(Date)==8) %>%
#   ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
#   facet_grid(Spp~as.Date(Date))+
#   theme(legend.position = "none")
# 
# # show the figure by calling the object we created
# dat.August.2012.2013
# ##########################

# use the filtered data (dat.filter5) to make species-specific curves and fit light responses
#################
# # graph all the data by species
#                            #MUPO#
# use the info from LcNo to plot the data by species
figure1 <- dat.filter5 %>% filter(Spp=="MUPO")%>%
  ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
  facet_grid(year(Date)~month(Date))+
  theme(legend.position = "none")+
  labs(title="MUPO")
# show the figure by calling the object we created
figure1

                            #LATR#
figure2 <- dat.filter5 %>% filter(Spp=="LATR")%>%
  ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
  facet_grid(year(Date)~month(Date))+
  theme(legend.position = "none")+
  labs(title="LATR")
# show the figure by calling the object we created
figure2

                            #FLCE#
figure3 <- dat.filter5 %>% filter(Spp=="FLCE")%>%
  ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
  facet_grid(year(Date)~month(Date))+
  theme(legend.position = "none")+
  labs(title="FLCE")
# show the figure by calling the object we created
figure3

                         #PRGL#
figure4 <- dat.filter5 %>% filter(Spp=="PRGL")%>%
  ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
  facet_grid(year(Date)~month(Date))+
  theme(legend.position = "none")+
  labs(title="PRGL")
# show the figure by calling the object we created
figure4
###########




#Fitting light response curves 
#ETR = ((ETRmax * E)/Eopt)*exp(1-(E/Eopt)) /E=PAR

# nls_mult 
# flexible parameter search
# https://padpadpadpad.github.io/post/fitting-non-linear-regressions-with-broom-purrr-and-nls.multstart/ 
# https://www.granvillematheson.com/post/nonlinear-modelling-using-nls-nlme-and-brms/#fitting-using-nonlinear-least-squares-nls-with-the-nls.multstart-package

library(nls.multstart)

# write the model to be flexible with start parameters
# etrmax.start and eopt.start can be included in the dplyr command
modelLR <- function(df, etrmax.start, eopt.start) {
  nlsLM(ETR~((ETRmax*PAR)/Eopt) *exp((1-(PAR/Eopt))), df,
  start= list(ETRmax =etrmax.start,Eopt=eopt.start))}

dat.filter5 <- dat.filter5 %>% mutate(month=month(Date))

# try running the model for a larger number of groups at a time. 
LR.spp.month <-dlply(dat.filter5, .(Spp,month), modelLR, etrmax.start=200, eopt.start=1500)
LR.spp.month.coefs <-ldply(LR.spp.month, function(x) coef(x)[1:2])

lcno.spp.month <- dat.filter5 %>% 
  distinct(LcNo, Spp, month) 

LR.spp.month.coefs <- merge(lcno.spp.month,LR.spp.month.coefs, by=c("Spp","month"), all.x=TRUE)

# create a flexible LR function that will take the monthly start values and fit by LcNo

library(minpack.lm)
library(nls.multstart)

d.example <- data.frame(LcNo = c(128, 128, 128, 128, 128, 128, 128, 128, 128),
                        PAR = c(1115,  819,  544,  342,   10, 3327, 4820, 1618, 2201),
                        ETR = c(232.3, 187.1, 131.6,  84.9,   2.6, 341.0, 340.1, 278.6, 311.5))

ggplot(d.example,aes(PAR,ETR, colour=factor(LcNo)))+
  geom_point(colour="black")+
  geom_line(aes(PAR, ((351*PAR)/3455) *exp(1-(PAR/3455))), colour="red")


modelLR.multstart2 <- function(PAR,ETR,ETRmax,Eopt) {
  ((ETRmax*PAR)/Eopt)*exp((1-(PAR/Eopt)))
}


fit2 <- nls_multstart(ETR ~ modelLR.multstart2(PAR=PAR, ETR=ETR,ETRmax.start,Eopt.start),
                     data = d.example,
                     iter = 2,
                     start_lower = c(ETRmax.start= 250, Eopt.start= 1500),
                     start_upper = c(ETRmax.start= 350, Eopt.start= 3000),
                     #supp_errors = 'Y',
                     na.action = na.omit,
                     lower = c(ETRmax= 0, Eopt= 0))

fit2

test.multi <- dat.test %>%
  group_by(., LcNo) %>%
  nest() %>%
  mutate(fit = purrr::map(data, ~nls.multstart::nls_multstart(ETR ~ modelLR.multstart2(PAR=PAR, ETR=ETR,ETRmax.start,Eopt.start),
                                                              data = .x,
                                                              iter = 200,
                                                              start_lower = c(ETRmax.start= 250, Eopt.start= 1500),
                                                              start_upper = c(ETRmax.start= 350, Eopt.start= 3000),
                                                              #supp_errors = 'Y',
                                                              na.action = na.omit,
                                                              lower = c(ETRmax= 0, Eopt= 0))))


# try nls_multstart with a year of data
summary(LR.spp.month.coefs)

test.multi.all <- dat.filter5 %>%
  group_by(., LcNo) %>%
  nest() %>%
  mutate(fit = purrr::map(data, ~nls.multstart::nls_multstart(ETR ~ modelLR.multstart2(PAR=PAR, ETR=ETR,ETRmax.start,Eopt.start),
                                                              data = .x,
                                                              iter = 1000,
                                                              start_lower = c(ETRmax.start= 80, Eopt.start= 2000),
                                                              start_upper = c(ETRmax.start= 450, Eopt.start= 4000),
                                                              #supp_errors = 'Y',
                                                              na.action = na.omit,
                                                              lower = c(ETRmax= 0, Eopt= 0))))


# look at the fits
params <- test.multi.all %>%
  mutate(params = map(fit, tidy)) %>%
  unnest(params)

preds <- test.multi.all %>%
  mutate(pred =map(fit, augment)) %>%
  unnest(pred)



# run the model by month, for MUPO

 dat.filter %>% filter(Spp=="MUPO"&year(Date)==2015&month(Date)==3)%>%
  ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
  geom_line(aes(PAR, ((200*PAR)/2500) *exp(1-(PAR/2500))), colour="red")
  facet_grid(year(Date)~month(Date))+
  theme(legend.position = "none")+
  labs(title="MUPO")


 
#ETRmax and Eopt values 
dat.test <- dat.filter %>% filter(Spp=="MUPO"&year(Date)==2015&month(Date)%in% c(3,4))
test1 <-  dlply(dat.test, .(Spp,month(Date),LcNo), modelLR, etrmax.start=200, eopt.start=1500)

LRtest1.coefs <-ldply(test1, function(x) coef(x)[1:2])
test1.pred <- merge(dat.test,LRtest1.coefs,by="LcNo",all.x=TRUE)
test1.pred <- test1.pred %>%
mutate(ETR.pred = ((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt))
ggplot(test1.pred)+
geom_point(aes(PAR,ETR))+
geom_line(aes(PAR, ETR.pred),colour="blue")+
geom_line(aes(PAR, ((550*PAR)/1700) *exp((1-PAR)/1700)), colour="red")+
facet_wrap(.~LcNo)
ggplot(test1.pred, aes(Date, ETRmax))+geom_point()



####################MUPO#######################

##M1
dat.filter %>% filter(Spp=="MUPO"&year(Date)==2012&month(Date)==8)%>%
  ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
  geom_line(aes(PAR, ((550*PAR)/1700) *exp((1-PAR)/1700)), colour="red")
facet_grid(year(Date)~month(Date))+
  theme(legend.position = "none")+
  labs(title="MUPO")

modelLRM1 <- function(df) {nls(ETR~((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt), df,
                             start= list(ETRmax =550,Eopt= 1700), 
                             nls.control(maxiter=1000, minFactor=1/8192))}


#ETRmax and Eopt values 
dat.testM1 <- dat.filter %>% filter(Spp=="MUPO"&year(Date)==2012&month(Date)==8)
testM1 <-  dlply(dat.test, .(LcNo), modelLR)
LRtestM1.coefs <-ldply(test1, function(x) coef(x)[1:2])
testM1.pred <- merge(dat.test,LRtest1.coefs,by="LcNo",all.x=TRUE)
testM1.pred <- test1.pred %>%
  mutate(ETR.pred = ((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt))
ggplot(test1.pred)+
  geom_point(aes(PAR,ETR))+
  geom_line(aes(PAR, ETR.pred),colour="blue")+
  geom_line(aes(PAR, ((550*PAR)/1700) *exp((1-PAR)/1700)), colour="red")+
  facet_wrap(.~LcNo)
ggplot(test1.pred, aes(Date, ETRmax))+geom_point()

############################M2

dat.filter %>% filter(Spp=="MUPO"&year(Date)==2012&month(Date)==9)%>%
  ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
  geom_line(aes(PAR, ((350*PAR)/1500) *exp((1-PAR)/1500)), colour="red")
facet_grid(year(Date)~month(Date))+
  theme(legend.position = "none")+
  labs(title="MUPO")

modelLRM2 <- function(df) {nls(ETR~((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt), df,
                               start= list(ETRmax =350,Eopt= 1500), 
                               nls.control(maxiter=1000, minFactor=1/8192))}


#ETRmax and Eopt values 
dat.testM2 <- dat.filter %>% filter(Spp=="MUPO"&year(Date)==2012&month(Date)==9)
testM2 <-  dlply(dat.testM2, .(LcNo), modelLRM2)
LRtestM2.coefs <-ldply(testM2, function(x) coef(x)[1:2])
testM2.pred <- merge(dat.testM2,LRtestM2.coefs,by="LcNo",all.x=TRUE)
testM2.pred <- testM2.pred %>%
  mutate(ETR.pred = ((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt))
ggplot(testM2.pred)+
  geom_point(aes(PAR,ETR))+
  geom_line(aes(PAR, ETR.pred),colour="blue")+
  geom_line(aes(PAR, ((350*PAR)/1500) *exp((1-PAR)/1500)), colour="red")+
  facet_wrap(.~LcNo)
ggplot(testM2.pred, aes(Date, ETRmax))+geom_point()


############################M3

dat.filter %>% filter(Spp=="MUPO"&year(Date)==2012&month(Date)==10)%>%
  ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
  geom_line(aes(PAR, ((350*PAR)/1400) *exp((1-PAR)/1400)), colour="red")
facet_grid(year(Date)~month(Date))+
  theme(legend.position = "none")+
  labs(title="MUPO")

modelLRM3 <- function(df) {nls(ETR~((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt), df,
                               start= list(ETRmax =350,Eopt= 1400), 
                               nls.control(maxiter=1000, minFactor=1/8192))}


#ETRmax and Eopt values 
dat.testM3 <- dat.filter %>% filter(Spp=="MUPO"&year(Date)==2012&month(Date)==10)
testM3 <-  dlply(dat.testM3, .(LcNo), modelLRM3)
LRtestM3.coefs <-ldply(testM3, function(x) coef(x)[1:2])
testM3.pred <- merge(dat.testM3,LRtestM3.coefs,by="LcNo",all.x=TRUE)
testM3.pred <- testM3.pred %>%
  mutate(ETR.pred = ((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt))
ggplot(testM3.pred)+
  geom_point(aes(PAR,ETR))+
  geom_line(aes(PAR, ETR.pred),colour="blue")+
  geom_line(aes(PAR, ((350*PAR)/1400) *exp((1-PAR)/1400)), colour="red")+
  facet_wrap(.~LcNo)
ggplot(testM3.pred, aes(Date, ETRmax))+geom_point()

########################M4

dat.filter %>% filter(Spp=="MUPO"&year(Date)==2013&month(Date)==7)%>%
  ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
  geom_line(aes(PAR, ((490*PAR)/1900) *exp((1-PAR)/1900)), colour="red")
facet_grid(year(Date)~month(Date))+
  theme(legend.position = "none")+
  labs(title="MUPO")

modelLRM4 <- function(df) {nls(ETR~((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt), df,
                               start= list(ETRmax =490,Eopt= 1900), 
                               nls.control(maxiter=1000, minFactor=1/8192))}


#ETRmax and Eopt values 
dat.testM4 <- dat.filter %>% filter(Spp=="MUPO"&year(Date)==2013&month(Date)==7)
testM4 <-  dlply(dat.testM4, .(LcNo), modelLRM4)
LRtestM4.coefs <-ldply(testM4, function(x) coef(x)[1:2])
testM4.pred <- merge(dat.testM4,LRtestM4.coefs,by="LcNo",all.x=TRUE)
testM4.pred <- testM4.pred %>%
  mutate(ETR.pred = ((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt))
ggplot(testM4.pred)+
  geom_point(aes(PAR,ETR))+
  geom_line(aes(PAR, ETR.pred),colour="blue")+
  geom_line(aes(PAR, ((490*PAR)/1900) *exp((1-PAR)/1900)), colour="red")+
  facet_wrap(.~LcNo)
ggplot(testM4.pred, aes(Date, ETRmax))+geom_point()

########################M5

dat.filter %>% filter(Spp=="MUPO"&year(Date)==2013&month(Date)==8)%>%
  ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
  geom_line(aes(PAR, ((550*PAR)/1900) *exp((1-PAR)/1900)), colour="red")
facet_grid(year(Date)~month(Date))+
  theme(legend.position = "none")+
  labs(title="MUPO")

modelLRM5 <- function(df) {nls(ETR~((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt), df,
                               start= list(ETRmax =550,Eopt= 1900), 
                               nls.control(maxiter=1000, minFactor=1/8192))}


#ETRmax and Eopt values 
dat.testM5 <- dat.filter %>% filter(Spp=="MUPO"&year(Date)==2013&month(Date)==8)
testM5 <-  dlply(dat.testM5, .(LcNo), modelLRM5)
LRtestM5.coefs <-ldply(testM5, function(x) coef(x)[1:2])
testM5.pred <- merge(dat.testM5,LRtestM5.coefs,by="LcNo",all.x=TRUE)
testM5.pred <- testM5.pred %>%
  mutate(ETR.pred = ((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt))
ggplot(testM5.pred)+
  geom_point(aes(PAR,ETR))+
  geom_line(aes(PAR, ETR.pred),colour="blue")+
  geom_line(aes(PAR, ((550*PAR)/1900) *exp((1-PAR)/1900)), colour="red")+
  facet_wrap(.~LcNo)
ggplot(testM5.pred, aes(Date, ETRmax))+geom_point()

########################M6

dat.filter %>% filter(Spp=="MUPO"&year(Date)==2013&month(Date)==9)%>%
  ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
  geom_line(aes(PAR, ((570*PAR)/1900) *exp((1-PAR)/1900)), colour="red")+
  facet_grid(year(Date)~month(Date))+
  theme(legend.position = "none")+
  labs(title="MUPO")

modelLRM6 <- function(df) {nls(ETR~((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt), df,
                               start= list(ETRmax =570,Eopt= 1900), 
                               nls.control(maxiter=1000, minFactor=1/8192))}


#ETRmax and Eopt values 
dat.testM6 <- dat.filter %>% filter(Spp=="MUPO"&year(Date)==2013&month(Date)==9)
testM6 <-  dlply(dat.testM6, .(LcNo), modelLRM6)
LRtestM6.coefs <-ldply(testM6, function(x) coef(x)[1:2])
testM6.pred <- merge(dat.testM6,LRtestM6.coefs,by="LcNo",all.x=TRUE)
testM6.pred <- testM6.pred %>%
  mutate(ETR.pred = ((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt))
ggplot(testM6.pred)+
  geom_point(aes(PAR,ETR))+
  geom_line(aes(PAR, ETR.pred),colour="blue")+
  geom_line(aes(PAR, ((570*PAR)/1900) *exp((1-PAR)/1900)), colour="red")+
  facet_wrap(.~LcNo)
ggplot(testM6.pred, aes(Date, ETRmax))+geom_point()

########################M7

dat.filter %>% filter(Spp=="MUPO"&year(Date)==2013&month(Date)==10)%>%
  ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
  geom_line(aes(PAR, ((430*PAR)/1900) *exp((1-PAR)/1900)), colour="red")
facet_grid(year(Date)~month(Date))+
  theme(legend.position = "none")+
  labs(title="MUPO")

modelLRM7 <- function(df) {nls(ETR~((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt), df,
                               start= list(ETRmax =430,Eopt= 1900), 
                               nls.control(maxiter=1000, minFactor=1/8192))}


#ETRmax and Eopt values 
dat.testM7 <- dat.filter %>% filter(Spp=="MUPO"&year(Date)==2013&month(Date)==10)
testM7 <-  dlply(dat.testM7, .(LcNo), modelLRM7)
LRtestM7.coefs <-ldply(testM7, function(x) coef(x)[1:2])
testM7.pred <- merge(dat.testM7,LRtestM7.coefs,by="LcNo",all.x=TRUE)
testM7.pred <- testM7.pred %>%
  mutate(ETR.pred = ((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt))
ggplot(testM7.pred)+
  geom_point(aes(PAR,ETR))+
  geom_line(aes(PAR, ETR.pred),colour="blue")+
  geom_line(aes(PAR, ((430*PAR)/1900) *exp((1-PAR)/1900)), colour="red")+
  facet_wrap(.~LcNo)
ggplot(testM7.pred, aes(Date, ETRmax))+geom_point()

########################M8

dat.filter %>% filter(Spp=="MUPO"&year(Date)==2014&month(Date)==8)%>%
  ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
  geom_line(aes(PAR, ((490*PAR)/1900) *exp((1-PAR)/1900)), colour="red")
facet_grid(year(Date)~month(Date))+
  theme(legend.position = "none")+
  labs(title="MUPO")

modelLRM8 <- function(df) {nls(ETR~((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt), df,
                               start= list(ETRmax =490,Eopt= 1900), 
                               nls.control(maxiter=1000, minFactor=1/8192))}


#ETRmax and Eopt values 
dat.testM8 <- dat.filter %>% filter(Spp=="MUPO"&year(Date)==2014&month(Date)==8)
testM8 <-  dlply(dat.testM8, .(LcNo), modelLRM8)
LRtestM8.coefs <-ldply(testM8, function(x) coef(x)[1:2])
testM8.pred <- merge(dat.testM8,LRtestM8.coefs,by="LcNo",all.x=TRUE)
testM8.pred <- testM8.pred %>%
  mutate(ETR.pred = ((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt))
ggplot(testM8.pred)+
  geom_point(aes(PAR,ETR))+
  geom_line(aes(PAR, ETR.pred),colour="blue")+
  geom_line(aes(PAR, ((490*PAR)/1900) *exp((1-PAR)/1900)), colour="red")+
  facet_wrap(.~LcNo)
ggplot(testM8.pred, aes(Date, ETRmax))+geom_point()

########################M9

dat.filter %>% filter(Spp=="MUPO"&year(Date)==2014&month(Date)==9)%>%
  ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
  geom_line(aes(PAR, ((540*PAR)/1900) *exp((1-PAR)/1900)), colour="red")
facet_grid(year(Date)~month(Date))+
  theme(legend.position = "none")+
  labs(title="MUPO")

modelLRM9 <- function(df) {nls(ETR~((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt), df,
                               start= list(ETRmax =540,Eopt= 1900), 
                               nls.control(maxiter=1000, minFactor=1/8192))}


#ETRmax and Eopt values 
dat.testM9 <- dat.filter %>% filter(Spp=="MUPO"&year(Date)==2014&month(Date)==9)
testM9 <-  dlply(dat.testM9, .(LcNo), modelLRM9)
LRtestM9.coefs <-ldply(testM9, function(x) coef(x)[1:2])
testM9.pred <- merge(dat.testM9,LRtestM9.coefs,by="LcNo",all.x=TRUE)
testM9.pred <- testM9.pred %>%
  mutate(ETR.pred = ((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt))
ggplot(testM9.pred)+
  geom_point(aes(PAR,ETR))+
  geom_line(aes(PAR, ETR.pred),colour="blue")+
  geom_line(aes(PAR, ((540*PAR)/1900) *exp((1-PAR)/1900)), colour="red")+
  facet_wrap(.~LcNo)
ggplot(testM9.pred, aes(Date, ETRmax))+geom_point()

########################M10

dat.filter %>% filter(Spp=="MUPO"&year(Date)==2014&month(Date)==10)%>%
  ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
  geom_line(aes(PAR, ((490*PAR)/1900) *exp((1-PAR)/1900)), colour="red")
facet_grid(year(Date)~month(Date))+
  theme(legend.position = "none")+
  labs(title="MUPO")

modelLRM10 <- function(df) {nls(ETR~((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt), df,
                               start= list(ETRmax =490,Eopt= 1900), 
                               nls.control(maxiter=1000, minFactor=1/8192))}


#ETRmax and Eopt values 
dat.testM10 <- dat.filter %>% filter(Spp=="MUPO"&year(Date)==2014&month(Date)==10)
testM10 <-  dlply(dat.testM10, .(LcNo), modelLRM10)
LRtestM10.coefs <-ldply(testM10, function(x) coef(x)[1:2])
testM10.pred <- merge(dat.testM10,LRtestM10.coefs,by="LcNo",all.x=TRUE)
testM10.pred <- testM10.pred %>%
  mutate(ETR.pred = ((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt))
ggplot(testM10.pred)+
  geom_point(aes(PAR,ETR))+
  geom_line(aes(PAR, ETR.pred),colour="blue")+
  geom_line(aes(PAR, ((490*PAR)/1900) *exp((1-PAR)/1900)), colour="red")+
  facet_wrap(.~LcNo)
ggplot(testM10.pred, aes(Date, ETRmax))+geom_point()

########################M11

dat.filter %>% filter(Spp=="MUPO"&year(Date)==2014&month(Date)==12)%>%
  ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
  geom_line(aes(PAR, ((440*PAR)/1900) *exp((1-PAR)/1900)), colour="red")
facet_grid(year(Date)~month(Date))+
  theme(legend.position = "none")+
  labs(title="MUPO")

modelLRM11 <- function(df) {nls(ETR~((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt), df,
                               start= list(ETRmax =440,Eopt= 1900), 
                               nls.control(maxiter=1000, minFactor=1/8192))}


#ETRmax and Eopt values 
dat.testM11 <- dat.filter %>% filter(Spp=="MUPO"&year(Date)==2014&month(Date)==12)
testM11 <-  dlply(dat.testM11, .(LcNo), modelLRM11)
LRtestM11.coefs <-ldply(testM11, function(x) coef(x)[1:2])
testM11.pred <- merge(dat.testM11,LRtestM11.coefs,by="LcNo",all.x=TRUE)
testM11.pred <- testM11.pred %>%
  mutate(ETR.pred = ((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt))
ggplot(testM11.pred)+
  geom_point(aes(PAR,ETR))+
  geom_line(aes(PAR, ETR.pred),colour="blue")+
  geom_line(aes(PAR, ((440*PAR)/1900) *exp((1-PAR)/1900)), colour="red")+
  facet_wrap(.~LcNo)
ggplot(testM11.pred, aes(Date, ETRmax))+geom_point()

########################M12

dat.filter %>% filter(Spp=="MUPO"&year(Date)==2015&month(Date)==3)%>%
  ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
  geom_line(aes(PAR, ((560*PAR)/1900) *exp((1-PAR)/1900)), colour="red")
facet_grid(year(Date)~month(Date))+
  theme(legend.position = "none")+
  labs(title="MUPO")

modelLRM12 <- function(df) {nls(ETR~((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt), df,
                               start= list(ETRmax =560,Eopt= 1900), 
                               nls.control(maxiter=1000, minFactor=1/8192))}


#ETRmax and Eopt values 
dat.testM12 <- dat.filter %>% filter(Spp=="MUPO"&year(Date)==2015&month(Date)==3)
testM12 <-  dlply(dat.testM12, .(LcNo), modelLRM12)
LRtestM12.coefs <-ldply(testM12, function(x) coef(x)[1:2])
testM12.pred <- merge(dat.testM12,LRtestM12.coefs,by="LcNo",all.x=TRUE)
testM12.pred <- testM12.pred %>%
  mutate(ETR.pred = ((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt))
ggplot(testM12.pred)+
  geom_point(aes(PAR,ETR))+
  geom_line(aes(PAR, ETR.pred),colour="blue")+
  geom_line(aes(PAR, ((560*PAR)/1900) *exp((1-PAR)/1900)), colour="red")+
  facet_wrap(.~LcNo)
ggplot(testM12.pred, aes(Date, ETRmax))+geom_point()

########################M13

dat.filter %>% filter(Spp=="MUPO"&year(Date)==2015&month(Date)==4)%>%
  ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
  geom_line(aes(PAR, ((440*PAR)/1900) *exp((1-PAR)/1900)), colour="red")
facet_grid(year(Date)~month(Date))+
  theme(legend.position = "none")+
  labs(title="MUPO")

modelLRM13 <- function(df) {nls(ETR~((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt), df,
                                start= list(ETRmax =440,Eopt= 1900), 
                                nls.control(maxiter=1000, minFactor=1/8192))}


#ETRmax and Eopt values 
dat.testM13 <- dat.filter %>% filter(Spp=="MUPO"&year(Date)==2015&month(Date)==4)
testM13 <-  dlply(dat.testM13, .(LcNo), modelLRM13)
LRtestM13.coefs <-ldply(testM13, function(x) coef(x)[1:2])
testM13.pred <- merge(dat.testM13,LRtestM13.coefs,by="LcNo",all.x=TRUE)
testM13.pred <- testM13.pred %>%
  mutate(ETR.pred = ((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt))
ggplot(testM13.pred)+
  geom_point(aes(PAR,ETR))+
  geom_line(aes(PAR, ETR.pred),colour="blue")+
  geom_line(aes(PAR, ((440*PAR)/1900) *exp((1-PAR)/1900)), colour="red")+
  facet_wrap(.~LcNo)
ggplot(testM13.pred, aes(Date, ETRmax))+geom_point()

########################M14

dat.filter %>% filter(Spp=="MUPO"&year(Date)==2015&month(Date)==5)%>%
  ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
  geom_line(aes(PAR, ((500*PAR)/1900) *exp((1-PAR)/1900)), colour="red")
facet_grid(year(Date)~month(Date))+
  theme(legend.position = "none")+
  labs(title="MUPO")

modelLRM14 <- function(df) {nls(ETR~((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt), df,
                                start= list(ETRmax =500,Eopt= 1900), 
                                nls.control(maxiter=1000, minFactor=1/8192))}


#ETRmax and Eopt values 
dat.testM14 <- dat.filter %>% filter(Spp=="MUPO"&year(Date)==2015&month(Date)==5)
testM14 <-  dlply(dat.testM14, .(LcNo), modelLRM14)
LRtestM14.coefs <-ldply(testM14, function(x) coef(x)[1:2])
testM14.pred <- merge(dat.testM14,LRtestM14.coefs,by="LcNo",all.x=TRUE)
testM14.pred <- testM14.pred %>%
  mutate(ETR.pred = ((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt))
ggplot(testM14.pred)+
  geom_point(aes(PAR,ETR))+
  geom_line(aes(PAR, ETR.pred),colour="blue")+
  geom_line(aes(PAR, ((500*PAR)/1900) *exp((1-PAR)/1900)), colour="red")+
  facet_wrap(.~LcNo)
ggplot(testM14.pred, aes(Date, ETRmax))+geom_point()

########################M15

dat.filter %>% filter(Spp=="MUPO"&year(Date)==2015&month(Date)==6)%>%
  ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
  geom_line(aes(PAR, ((340*PAR)/1900) *exp((1-PAR)/1900)), colour="red")
facet_grid(year(Date)~month(Date))+
  theme(legend.position = "none")+
  labs(title="MUPO")

modelLRM15 <- function(df) {nls(ETR~((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt), df,
                                start= list(ETRmax =340,Eopt= 1900), 
                                nls.control(maxiter=1000, minFactor=1/8192))}


#ETRmax and Eopt values 
dat.testM15 <- dat.filter %>% filter(Spp=="MUPO"&year(Date)==2015&month(Date)==6)
testM15 <-  dlply(dat.testM15, .(LcNo), modelLRM15)
LRtestM15.coefs <-ldply(testM15, function(x) coef(x)[1:2])
testM15.pred <- merge(dat.testM15,LRtestM15.coefs,by="LcNo",all.x=TRUE)
testM15.pred <- testM15.pred %>%
  mutate(ETR.pred = ((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt))
ggplot(testM15.pred)+
  geom_point(aes(PAR,ETR))+
  geom_line(aes(PAR, ETR.pred),colour="blue")+
  geom_line(aes(PAR, ((340*PAR)/1900) *exp((1-PAR)/1900)), colour="red")+
  facet_wrap(.~LcNo)
ggplot(testM15.pred, aes(Date, ETRmax))+geom_point()

########################M16

dat.filter %>% filter(Spp=="MUPO"&year(Date)==2015&month(Date)==7)%>%
  ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
  geom_line(aes(PAR, ((390*PAR)/1900) *exp((1-PAR)/1900)), colour="red")
facet_grid(year(Date)~month(Date))+
  theme(legend.position = "none")+
  labs(title="MUPO")

modelLRM16 <- function(df) {nls(ETR~((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt), df,
                                start= list(ETRmax =390,Eopt= 1900), 
                                nls.control(maxiter=1000, minFactor=1/8192))}


#ETRmax and Eopt values 
dat.testM16 <- dat.filter %>% filter(Spp=="MUPO"&year(Date)==2015&month(Date)==7)
testM16 <-  dlply(dat.testM16, .(LcNo), modelLRM16)
LRtestM16.coefs <-ldply(testM16, function(x) coef(x)[1:2])
testM16.pred <- merge(dat.testM16,LRtestM16.coefs,by="LcNo",all.x=TRUE)
testM16.pred <- testM16.pred %>%
  mutate(ETR.pred = ((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt))
ggplot(testM16.pred)+
  geom_point(aes(PAR,ETR))+
  geom_line(aes(PAR, ETR.pred),colour="blue")+
  geom_line(aes(PAR, ((390*PAR)/1900) *exp((1-PAR)/1900)), colour="red")+
  facet_wrap(.~LcNo)
ggplot(testM16.pred, aes(Date, ETRmax))+geom_point()

########################M17

dat.filter %>% filter(Spp=="MUPO"&year(Date)==2015&month(Date)==8)%>%
  ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
  geom_line(aes(PAR, ((400*PAR)/1900) *exp((1-PAR)/1900)), colour="red")
facet_grid(year(Date)~month(Date))+
  theme(legend.position = "none")+
  labs(title="MUPO")

modelLRM17 <- function(df) {nls(ETR~((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt), df,
                                start= list(ETRmax =400,Eopt= 1900), 
                                nls.control(maxiter=1000, minFactor=1/8192))}


#ETRmax and Eopt values 
dat.testM17 <- dat.filter %>% filter(Spp=="MUPO"&year(Date)==2015&month(Date)==8)
testM17 <-  dlply(dat.testM17, .(LcNo), modelLRM17)
LRtestM17.coefs <-ldply(testM17, function(x) coef(x)[1:2])
testM17.pred <- merge(dat.testM17,LRtestM17.coefs,by="LcNo",all.x=TRUE)
testM17.pred <- testM17.pred %>%
  mutate(ETR.pred = ((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt))
ggplot(testM17.pred)+
  geom_point(aes(PAR,ETR))+
  geom_line(aes(PAR, ETR.pred),colour="blue")+
  geom_line(aes(PAR, ((400*PAR)/1900) *exp((1-PAR)/1900)), colour="red")+
  facet_wrap(.~LcNo)
ggplot(testM17.pred, aes(Date, ETRmax))+geom_point()

########################M18

dat.filter %>% filter(Spp=="MUPO"&year(Date)==2015&month(Date)==9)%>%
  ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
  geom_line(aes(PAR, ((470*PAR)/1900) *exp((1-PAR)/1900)), colour="red")
facet_grid(year(Date)~month(Date))+
  theme(legend.position = "none")+
  labs(title="MUPO")

modelLRM18 <- function(df) {nls(ETR~((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt), df,
                                start= list(ETRmax =470,Eopt= 1900), 
                                nls.control(maxiter=1000, minFactor=1/8192))}


#ETRmax and Eopt values 
dat.testM18 <- dat.filter %>% filter(Spp=="MUPO"&year(Date)==2015&month(Date)==9)
testM18 <-  dlply(dat.testM18, .(LcNo), modelLRM18)
LRtestM18.coefs <-ldply(testM18, function(x) coef(x)[1:2])
testM18.pred <- merge(dat.testM18,LRtestM18.coefs,by="LcNo",all.x=TRUE)
testM18.pred <- testM18.pred %>%
  mutate(ETR.pred = ((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt))
ggplot(testM18.pred)+
  geom_point(aes(PAR,ETR))+
  geom_line(aes(PAR, ETR.pred),colour="blue")+
  geom_line(aes(PAR, ((470*PAR)/1900) *exp((1-PAR)/1900)), colour="red")+
  facet_wrap(.~LcNo)
ggplot(testM18.pred, aes(Date, ETRmax))+geom_point()

########################M19

dat.filter %>% filter(Spp=="MUPO"&year(Date)==2015&month(Date)==11)%>%
  ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
  geom_line(aes(PAR, ((430*PAR)/1900) *exp((1-PAR)/1900)), colour="red")
facet_grid(year(Date)~month(Date))+
  theme(legend.position = "none")+
  labs(title="MUPO")

modelLRM19 <- function(df) {nls(ETR~((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt), df,
                                start= list(ETRmax =430,Eopt= 1900), 
                                nls.control(maxiter=1000, minFactor=1/8192))}


#ETRmax and Eopt values 
dat.testM19 <- dat.filter %>% filter(Spp=="MUPO"&year(Date)==2015&month(Date)==11)
testM19 <-  dlply(dat.testM19, .(LcNo), modelLRM19)
LRtestM19.coefs <-ldply(testM19, function(x) coef(x)[1:2])
testM19.pred <- merge(dat.testM19,LRtestM19.coefs,by="LcNo",all.x=TRUE)
testM19.pred <- testM19.pred %>%
  mutate(ETR.pred = ((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt))
ggplot(testM19.pred)+
  geom_point(aes(PAR,ETR))+
  geom_line(aes(PAR, ETR.pred),colour="blue")+
  geom_line(aes(PAR, ((430*PAR)/1900) *exp((1-PAR)/1900)), colour="red")+
  facet_wrap(.~LcNo)
ggplot(testM19.pred, aes(Date, ETRmax))+geom_point()

########################M20

dat.filter %>% filter(Spp=="MUPO"&year(Date)==2016&month(Date)==5)%>%
  ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
  geom_line(aes(PAR, ((560*PAR)/1900) *exp((1-PAR)/1900)), colour="red")
facet_grid(year(Date)~month(Date))+
  theme(legend.position = "none")+
  labs(title="MUPO")

modelLRM20 <- function(df) {nls(ETR~((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt), df,
                                start= list(ETRmax =560,Eopt= 1900), 
                                nls.control(maxiter=1000, minFactor=1/8192))}


#ETRmax and Eopt values 
dat.testM20 <- dat.filter %>% filter(Spp=="MUPO"&year(Date)==2016&month(Date)==5)
testM20 <-  dlply(dat.testM20, .(LcNo), modelLRM20)
LRtestM20.coefs <-ldply(testM20, function(x) coef(x)[1:2])
testM20.pred <- merge(dat.testM20,LRtestM20.coefs,by="LcNo",all.x=TRUE)
testM20.pred <- testM20.pred %>%
  mutate(ETR.pred = ((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt))
ggplot(testM20.pred)+
  geom_point(aes(PAR,ETR))+
  geom_line(aes(PAR, ETR.pred),colour="blue")+
  geom_line(aes(PAR, ((560*PAR)/1900) *exp((1-PAR)/1900)), colour="red")+
  facet_wrap(.~LcNo)
ggplot(testM20.pred, aes(Date, ETRmax))+geom_point()

########################M21

dat.filter %>% filter(Spp=="MUPO"&year(Date)==2016&month(Date)==6&ETR>10)%>%
  ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
  geom_line(aes(PAR, ((300*PAR)/1900) *exp((1-PAR)/1900)), colour="red")
facet_grid(year(Date)~month(Date))+
  theme(legend.position = "none")+
  labs(title="MUPO")

modelLRM21 <- function(df) {nls(ETR~((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt), df,
                                start= list(ETRmax =300,Eopt= 1900), 
                                nls.control(maxiter=1000, minFactor=1/8192))}


#ETRmax and Eopt values 
dat.testM21 <- dat.filter %>% filter(Spp=="MUPO"&year(Date)==2016&month(Date)==6)
testM21 <-  dlply(dat.testM21, .(LcNo), modelLRM21)
LRtestM21.coefs <-ldply(testM21, function(x) coef(x)[1:2])
testM21.pred <- merge(dat.testM21,LRtestM21.coefs,by="LcNo",all.x=TRUE)
testM21.pred <- testM21.pred %>%
  mutate(ETR.pred = ((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt))
ggplot(testM21.pred)+
  geom_point(aes(PAR,ETR))+
  geom_line(aes(PAR, ETR.pred),colour="blue")+
  geom_line(aes(PAR, ((300*PAR)/1900) *exp((1-PAR)/1900)), colour="red")+
  facet_wrap(.~LcNo)
ggplot(testM21.pred, aes(Date, ETRmax))+geom_point()


########################M22

dat.filter %>% filter(Spp=="MUPO"&year(Date)==2016&month(Date)==8)%>%
  ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
  geom_line(aes(PAR, ((570*PAR)/1900) *exp((1-PAR)/1900)), colour="red")
facet_grid(year(Date)~month(Date))+
  theme(legend.position = "none")+
  labs(title="MUPO")

modelLRM22 <- function(df) {nls(ETR~((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt), df,
                                start= list(ETRmax =570,Eopt= 1900), 
                                nls.control(maxiter=1000, minFactor=1/8192))}


#ETRmax and Eopt values 
dat.testM22 <- dat.filter %>% filter(Spp=="MUPO"&year(Date)==2016&month(Date)==8)
testM22 <-  dlply(dat.testM22, .(LcNo), modelLRM22)
LRtestM22.coefs <-ldply(testM22, function(x) coef(x)[1:2])
testM22.pred <- merge(dat.testM22,LRtestM22.coefs,by="LcNo",all.x=TRUE)
testM22.pred <- testM22.pred %>%
  mutate(ETR.pred = ((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt))
ggplot(testM22.pred)+
  geom_point(aes(PAR,ETR))+
  geom_line(aes(PAR, ETR.pred),colour="blue")+
  geom_line(aes(PAR, ((570*PAR)/1900) *exp((1-PAR)/1900)), colour="red")+
  facet_wrap(.~LcNo)
ggplot(testM22.pred, aes(Date, ETRmax))+geom_point()

########################M23

dat.filter %>% filter(Spp=="MUPO"&year(Date)==2016&month(Date)==9)%>%
  ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
  geom_line(aes(PAR, ((460*PAR)/1800) *exp((1-PAR)/1800)), colour="red")
facet_grid(year(Date)~month(Date))+
  theme(legend.position = "none")+
  labs(title="MUPO")

modelLRM23 <- function(df) {nls(ETR~((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt), df,
                                start= list(ETRmax =460,Eopt= 1800), 
                                nls.control(maxiter=1000, minFactor=1/8192))}


#ETRmax and Eopt values 
dat.testM23 <- dat.filter %>% filter(Spp=="MUPO"&year(Date)==2016&month(Date)==9)
testM23 <-  dlply(dat.testM23, .(LcNo), modelLRM23)
LRtestM23.coefs <-ldply(testM23, function(x) coef(x)[1:2])
testM23.pred <- merge(dat.testM23,LRtestM23.coefs,by="LcNo",all.x=TRUE)
testM23.pred <- testM23.pred %>%
  mutate(ETR.pred = ((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt))
ggplot(testM23.pred)+
  geom_point(aes(PAR,ETR))+
  geom_line(aes(PAR, ETR.pred),colour="blue")+
  geom_line(aes(PAR, ((460*PAR)/1800) *exp((1-PAR)/1800)), colour="red")+
  facet_wrap(.~LcNo)
ggplot(testM23.pred, aes(Date, ETRmax))+geom_point()

########################M24

dat.filter %>% filter(Spp=="MUPO"&year(Date)==2016&month(Date)==10)%>%
  ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
  geom_line(aes(PAR, ((570*PAR)/1800) *exp((1-PAR)/1800)), colour="red")
facet_grid(year(Date)~month(Date))+
  theme(legend.position = "none")+
  labs(title="MUPO")

modelLRM24 <- function(df) {nls(ETR~((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt), df,
                                start= list(ETRmax =570,Eopt= 1800), 
                                nls.control(maxiter=1000, minFactor=1/8192))}


#ETRmax and Eopt values 
dat.testM24 <- dat.filter %>% filter(Spp=="MUPO"&year(Date)==2016&month(Date)==10)
testM24 <-  dlply(dat.testM24, .(LcNo), modelLRM24)
LRtestM24.coefs <-ldply(testM24, function(x) coef(x)[1:2])
testM24.pred <- merge(dat.testM24,LRtestM24.coefs,by="LcNo",all.x=TRUE)
testM24.pred <- testM24.pred %>%
  mutate(ETR.pred = ((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt))
ggplot(testM24.pred)+
  geom_point(aes(PAR,ETR))+
  geom_line(aes(PAR, ETR.pred),colour="blue")+
  geom_line(aes(PAR, ((570*PAR)/1800) *exp((1-PAR)/1800)), colour="red")+
  facet_wrap(.~LcNo)
ggplot(testM24.pred, aes(Date, ETRmax))+geom_point()

########################M25

dat.filter %>% filter(Spp=="MUPO"&year(Date)==2016&month(Date)==11)%>%
  ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
  geom_line(aes(PAR, ((400*PAR)/1800) *exp((1-PAR)/1800)), colour="red")
facet_grid(year(Date)~month(Date))+
  theme(legend.position = "none")+
  labs(title="MUPO")

modelLRM25 <- function(df) {nls(ETR~((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt), df,
                                start= list(ETRmax =400,Eopt= 1800), 
                                nls.control(maxiter=1000, minFactor=1/8192))}


#ETRmax and Eopt values 
dat.testM25 <- dat.filter %>% filter(Spp=="MUPO"&year(Date)==2016&month(Date)==11)
testM25 <-  dlply(dat.testM25, .(LcNo), modelLRM25)
LRtestM25.coefs <-ldply(testM25, function(x) coef(x)[1:2])
testM25.pred <- merge(dat.testM25,LRtestM25.coefs,by="LcNo",all.x=TRUE)
testM25.pred <- testM25.pred %>%
  mutate(ETR.pred = ((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt))
ggplot(testM25.pred)+
  geom_point(aes(PAR,ETR))+
  geom_line(aes(PAR, ETR.pred),colour="blue")+
  geom_line(aes(PAR, ((400*PAR)/1800) *exp((1-PAR)/1800)), colour="red")+
  facet_wrap(.~LcNo)
ggplot(testM25.pred, aes(Date, ETRmax))+geom_point()

########################M26

dat.filter %>% filter(Spp=="MUPO"&year(Date)==2017&month(Date)==2)%>%
  ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
  geom_line(aes(PAR, ((540*PAR)/1800) *exp((1-PAR)/1800)), colour="red")
facet_grid(year(Date)~month(Date))+
  theme(legend.position = "none")+
  labs(title="MUPO")

modelLRM26 <- function(df) {nls(ETR~((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt), df,
                                start= list(ETRmax =540,Eopt= 1800), 
                                nls.control(maxiter=1000, minFactor=1/8192))}


#ETRmax and Eopt values 
dat.testM26 <- dat.filter %>% filter(Spp=="MUPO"&year(Date)==2017&month(Date)==2)
testM26 <-  dlply(dat.testM26, .(LcNo), modelLRM26)
LRtestM26.coefs <-ldply(testM26, function(x) coef(x)[1:2])
testM26.pred <- merge(dat.testM26,LRtestM26.coefs,by="LcNo",all.x=TRUE)
testM26.pred <- testM26.pred %>%
  mutate(ETR.pred = ((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt))
ggplot(testM26.pred)+
  geom_point(aes(PAR,ETR))+
  geom_line(aes(PAR, ETR.pred),colour="blue")+
  geom_line(aes(PAR, ((540*PAR)/1800) *exp((1-PAR)/1800)), colour="red")+
  facet_wrap(.~LcNo)
ggplot(testM26.pred, aes(Date, ETRmax))+geom_point()

########################M27

dat.filter %>% filter(Spp=="MUPO"&year(Date)==2017&month(Date)==3)%>%
  ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
  geom_line(aes(PAR, ((560*PAR)/1800) *exp((1-PAR)/1800)), colour="red")
facet_grid(year(Date)~month(Date))+
  theme(legend.position = "none")+
  labs(title="MUPO")

modelLRM27 <- function(df) {nls(ETR~((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt), df,
                                start= list(ETRmax =560,Eopt= 1800), 
                                nls.control(maxiter=1000, minFactor=1/8192))}


#ETRmax and Eopt values 
dat.testM27 <- dat.filter %>% filter(Spp=="MUPO"&year(Date)==2017&month(Date)==3)
testM27 <-  dlply(dat.testM27, .(LcNo), modelLRM26)
LRtestM27.coefs <-ldply(testM27, function(x) coef(x)[1:2])
testM27.pred <- merge(dat.testM27,LRtestM27.coefs,by="LcNo",all.x=TRUE)
testM27.pred <- testM27.pred %>%
  mutate(ETR.pred = ((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt))
ggplot(testM27.pred)+
  geom_point(aes(PAR,ETR))+
  geom_line(aes(PAR, ETR.pred),colour="blue")+
  geom_line(aes(PAR, ((560*PAR)/1800) *exp((1-PAR)/1800)), colour="red")+
  facet_wrap(.~LcNo)
ggplot(testM27.pred, aes(Date, ETRmax))+geom_point()

########################M28

dat.filter %>% filter(Spp=="MUPO"&year(Date)==2017&month(Date)==4)%>%
  ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
  geom_line(aes(PAR, ((530*PAR)/1800) *exp((1-PAR)/1800)), colour="red")
facet_grid(year(Date)~month(Date))+
  theme(legend.position = "none")+
  labs(title="MUPO")

modelLRM28 <- function(df) {nls(ETR~((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt), df,
                                start= list(ETRmax =530,Eopt= 1800), 
                                nls.control(maxiter=1000, minFactor=1/8192))}


#ETRmax and Eopt values 
dat.testM28 <- dat.filter %>% filter(Spp=="MUPO"&year(Date)==2017&month(Date)==4)
testM28 <-  dlply(dat.testM28, .(LcNo), modelLRM28)
LRtestM28.coefs <-ldply(testM28, function(x) coef(x)[1:2])
testM28.pred <- merge(dat.testM28,LRtestM28.coefs,by="LcNo",all.x=TRUE)
testM28.pred <- testM28.pred %>%
  mutate(ETR.pred = ((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt))
ggplot(testM28.pred)+
  geom_point(aes(PAR,ETR))+
  geom_line(aes(PAR, ETR.pred),colour="blue")+
  geom_line(aes(PAR, ((530*PAR)/1800) *exp((1-PAR)/1800)), colour="red")+
  facet_wrap(.~LcNo)
ggplot(testM28.pred, aes(Date, ETRmax))+geom_point()

########################M29

dat.filter %>% filter(Spp=="MUPO"&year(Date)==2017&month(Date)==6)%>%
  ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
  geom_line(aes(PAR, ((450*PAR)/1800) *exp((1-PAR)/1800)), colour="red")
facet_grid(year(Date)~month(Date))+
  theme(legend.position = "none")+
  labs(title="MUPO")

modelLRM29 <- function(df) {nls(ETR~((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt), df,
                                start= list(ETRmax =450,Eopt= 1800), 
                                nls.control(maxiter=1000, minFactor=1/8192))}


#ETRmax and Eopt values 
dat.testM29 <- dat.filter %>% filter(Spp=="MUPO"&year(Date)==2017&month(Date)==6)
testM29 <-  dlply(dat.testM29, .(LcNo), modelLRM29)
LRtestM29.coefs <-ldply(testM29, function(x) coef(x)[1:2])
testM29.pred <- merge(dat.testM29,LRtestM29.coefs,by="LcNo",all.x=TRUE)
testM29.pred <- testM29.pred %>%
  mutate(ETR.pred = ((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt))
ggplot(testM29.pred)+
  geom_point(aes(PAR,ETR))+
  geom_line(aes(PAR, ETR.pred),colour="blue")+
  geom_line(aes(PAR, ((450*PAR)/1800) *exp((1-PAR)/1800)), colour="red")+
  facet_wrap(.~LcNo)
ggplot(testM29.pred, aes(Date, ETRmax))+geom_point()

########################M30

dat.filter %>% filter(Spp=="MUPO"&year(Date)==2017&month(Date)==7)%>%
  ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
  geom_line(aes(PAR, ((525*PAR)/1800) *exp((1-PAR)/1800)), colour="red")
facet_grid(year(Date)~month(Date))+
  theme(legend.position = "none")+
  labs(title="MUPO")

modelLRM30 <- function(df) {nls(ETR~((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt), df,
                                start= list(ETRmax =525,Eopt= 1800), 
                                nls.control(maxiter=1000, minFactor=1/8192))}


#ETRmax and Eopt values 
dat.testM30 <- dat.filter %>% filter(Spp=="MUPO"&year(Date)==2017&month(Date)==7)
testM30 <-  dlply(dat.testM30, .(LcNo), modelLRM26)
LRtestM30.coefs <-ldply(testM30, function(x) coef(x)[1:2])
testM30.pred <- merge(dat.testM30,LRtestM30.coefs,by="LcNo",all.x=TRUE)
testM30.pred <- testM26.pred %>%
  mutate(ETR.pred = ((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt))
ggplot(testM30.pred)+
  geom_point(aes(PAR,ETR))+
  geom_line(aes(PAR, ETR.pred),colour="blue")+
  geom_line(aes(PAR, ((525*PAR)/1800) *exp((1-PAR)/1800)), colour="red")+
  facet_wrap(.~LcNo)
ggplot(testM30.pred, aes(Date, ETRmax))+geom_point()

########################M31

dat.filter %>% filter(Spp=="MUPO"&year(Date)==2017&month(Date)==8)%>%
  ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
  geom_line(aes(PAR, ((520*PAR)/1800) *exp((1-PAR)/1800)), colour="red")
facet_grid(year(Date)~month(Date))+
  theme(legend.position = "none")+
  labs(title="MUPO")

modelLRM31 <- function(df) {nls(ETR~((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt), df,
                                start= list(ETRmax =520,Eopt= 1800), 
                                nls.control(maxiter=1000, minFactor=1/8192))}


#ETRmax and Eopt values 
dat.testM31 <- dat.filter %>% filter(Spp=="MUPO"&year(Date)==2017&month(Date)==8)
testM31 <-  dlply(dat.testM31, .(LcNo), modelLRM31)
LRtestM31.coefs <-ldply(testM31, function(x) coef(x)[1:2])
testM31.pred <- merge(dat.testM31,LRtestM31.coefs,by="LcNo",all.x=TRUE)
testM31.pred <- testM31.pred %>%
  mutate(ETR.pred = ((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt))
ggplot(testM31.pred)+
  geom_point(aes(PAR,ETR))+
  geom_line(aes(PAR, ETR.pred),colour="blue")+
  geom_line(aes(PAR, ((520*PAR)/1800) *exp((1-PAR)/1800)), colour="red")+
  facet_wrap(.~LcNo)
ggplot(testM31.pred, aes(Date, ETRmax))+geom_point()

########################M32

dat.filter %>% filter(Spp=="MUPO"&year(Date)==2018&month(Date)==10)%>%
  ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
  geom_line(aes(PAR, ((540*PAR)/1800) *exp((1-PAR)/1800)), colour="red")
facet_grid(year(Date)~month(Date))+
  theme(legend.position = "none")+
  labs(title="MUPO")

modelLRM32 <- function(df) {nls(ETR~((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt), df,
                                start= list(ETRmax =540,Eopt= 1800), 
                                nls.control(maxiter=1000, minFactor=1/8192))}


#ETRmax and Eopt values 
dat.testM32 <- dat.filter %>% filter(Spp=="MUPO"&year(Date)==2018&month(Date)==2)
testM32 <-  dlply(dat.testM32, .(LcNo), modelLRM32)
LRtestM32.coefs <-ldply(testM32, function(x) coef(x)[1:2])
testM32.pred <- merge(dat.testM32,LRtestM32.coefs,by="LcNo",all.x=TRUE)
testM32.pred <- testM32.pred %>%
  mutate(ETR.pred = ((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt))
ggplot(testM32.pred)+
  geom_point(aes(PAR,ETR))+
  geom_line(aes(PAR, ETR.pred),colour="blue")+
  geom_line(aes(PAR, ((540*PAR)/1800) *exp((1-PAR)/1800)), colour="red")+
  facet_wrap(.~LcNo)
ggplot(testM32.pred, aes(Date, ETRmax))+geom_point()

########################M33
########################M34
########################M35
########################M36
########################M37
########################M38

########################M39
dat.filter %>% filter(Spp=="MUPO"&year(Date)==2019&month(Date)==8)%>%
  ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
  geom_line(aes(PAR, ((400*PAR)/1800) *exp((1-PAR)/1800)), colour="red")
facet_grid(year(Date)~month(Date))+
  theme(legend.position = "none")+
  labs(title="MUPO")

modelLRM39 <- function(df) {nls(ETR~((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt), df,
                                start= list(ETRmax =400,Eopt= 1800), 
                                nls.control(maxiter=1000, minFactor=1/8192))}


#ETRmax and Eopt values 
dat.testM39 <- dat.filter %>% filter(Spp=="MUPO"&year(Date)==2019&month(Date)==8)
testM39 <-  dlply(dat.testM39, .(LcNo), modelLRM39)
LRtestM39.coefs <-ldply(testM39, function(x) coef(x)[1:2])
testM39.pred <- merge(dat.testM39,LRtestM39.coefs,by="LcNo",all.x=TRUE)
testM39.pred <- testM39.pred %>%
  mutate(ETR.pred = ((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt))
ggplot(testM39.pred)+
  geom_point(aes(PAR,ETR))+
  geom_line(aes(PAR, ETR.pred),colour="blue")+
  geom_line(aes(PAR, ((400*PAR)/1800) *exp((1-PAR)/1800)), colour="red")+
  facet_wrap(.~LcNo)
ggplot(testM29.pred, aes(Date, ETRmax))+geom_point()

########################M40

dat.filter %>% filter(Spp=="MUPO"&year(Date)==2019&month(Date)==9)%>%
  ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
  geom_line(aes(PAR, ((450*PAR)/1800) *exp((1-PAR)/1800)), colour="red")
facet_grid(year(Date)~month(Date))+
  theme(legend.position = "none")+
  labs(title="MUPO")

modelLRM40 <- function(df) {nls(ETR~((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt), df,
                                start= list(ETRmax =450,Eopt= 1800), 
                                nls.control(maxiter=1000, minFactor=1/8192))}


#ETRmax and Eopt values 
dat.testM40 <- dat.filter %>% filter(Spp=="MUPO"&year(Date)==2019&month(Date)==9)
testM40 <-  dlply(dat.testM40, .(LcNo), modelLRM40)
LRtestM40.coefs <-ldply(testM40, function(x) coef(x)[1:2])
testM40.pred <- merge(dat.testM40,LRtestM40.coefs,by="LcNo",all.x=TRUE)
testM40.pred <- testM40.pred %>%
  mutate(ETR.pred = ((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt))
ggplot(testM31.pred)+
  geom_point(aes(PAR,ETR))+
  geom_line(aes(PAR, ETR.pred),colour="blue")+
  geom_line(aes(PAR, ((450*PAR)/1800) *exp((1-PAR)/1800)), colour="red")+
  facet_wrap(.~LcNo)
ggplot(testM31.pred, aes(Date, ETRmax))+geom_point()

########################M41

dat.filter %>% filter(Spp=="MUPO"&year(Date)==2019&month(Date)==10)%>%
  ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
  geom_line(aes(PAR, ((300*PAR)/1800) *exp((1-PAR)/1800)), colour="red")
facet_grid(year(Date)~month(Date))+
  theme(legend.position = "none")+
  labs(title="MUPO")

modelLRM41 <- function(df) {nls(ETR~((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt), df,
                                start= list(ETRmax =300,Eopt= 1800), 
                                nls.control(maxiter=1000, minFactor=1/8192))}


#ETRmax and Eopt values 
dat.testM41 <- dat.filter %>% filter(Spp=="MUPO"&year(Date)==2019&month(Date)==10)
testM41 <-  dlply(dat.testM41, .(LcNo), modelLRM41)
LRtestM41.coefs <-ldply(testM41, function(x) coef(x)[1:2])
testM41.pred <- merge(dat.testM41,LRtestM41.coefs,by="LcNo",all.x=TRUE)
testM41.pred <- testM41.pred %>%
  mutate(ETR.pred = ((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt))
ggplot(testM41.pred)+
  geom_point(aes(PAR,ETR))+
  geom_line(aes(PAR, ETR.pred),colour="blue")+
  geom_line(aes(PAR, ((450*PAR)/1800) *exp((1-PAR)/1800)), colour="red")+
  facet_wrap(.~LcNo)
ggplot(testM41.pred, aes(Date, ETRmax))+geom_point()

########################M42

dat.filter %>% filter(Spp=="MUPO"&year(Date)==2019&month(Date)==11&ETR>0.050)%>%
  ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
  #geom_line(aes(PAR, ((70*PAR)/1800) *exp((1-PAR)/1800)), colour="red")
facet_grid(year(Date)~month(Date))+
  theme(legend.position = "none")+
  labs(title="MUPO")

modelLRM42 <- function(df) {nls(ETR~((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt), df,
                                start= list(ETRmax =70,Eopt= 1800), 
                                nls.control(maxiter=1000, minFactor=1/8192))}


#ETRmax and Eopt values 
dat.testM42 <- dat.filter %>% filter(Spp=="MUPO"&year(Date)==2019&month(Date)==11&ETR>2)
testM42 <-  dlply(dat.testM42, .(LcNo), modelLRM42)
LRtestM42.coefs <-ldply(testM42, function(x) coef(x)[1:2])
testM42.pred <- merge(dat.testM42,LRtestM42.coefs,by="LcNo",all.x=TRUE)
testM42.pred <- testM42.pred %>%
  mutate(ETR.pred = ((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt))
ggplot(testM42.pred)+
  geom_point(aes(PAR,ETR))+
  geom_line(aes(PAR, ETR.pred),colour="blue")+
  geom_line(aes(PAR, ((70*PAR)/1800) *exp((1-PAR)/1800)), colour="red")+
  facet_wrap(.~LcNo)
ggplot(testM42.pred, aes(Date, ETRmax))+geom_point()

###Combining MUPO light curve results

# use grep to find all dataframes in the environment that match 'testM\d\d.pred'.
# Then combine all those into a list
Pattern.m<-grep("testM\\d.pred",names(.GlobalEnv),value=TRUE)
Pattern.m2<-grep("testM\\d\\d.pred",names(.GlobalEnv),value=TRUE)

Pattern.m_list<-do.call("list",mget(Pattern.m))
Pattern.m2_list<-do.call("list",mget(Pattern.m2))



###Combining MUPO light curve results
lrm.all1<-rbindlist(Pattern.m_list, use.names=TRUE)
lrm.all2<-rbindlist(Pattern.m2_list, use.names=TRUE)
lrm.all<-rbind(lrm.all1,lrm.all2)

ggplot(subset(lrm.all, ETRmax<10000),aes(Date,ETRmax,colour=year(Date)))+geom_point()

########################L43

dat.filter %>% filter(Spp=="LATR"&year(Date)==2012&month(Date)==8)%>%
  ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
  geom_line(aes(PAR, ((560*PAR)/1800) *exp((1-PAR)/1800)), colour="red")
facet_grid(year(Date)~month(Date))+
  theme(legend.position = "none")+
  labs(title="MUPO")

modelLRL43 <- function(df) {nls(ETR~((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt), df,
                                start= list(ETRmax =560,Eopt= 1800), 
                                nls.control(maxiter=1000, minFactor=1/8192))}


#ETRmax and Eopt values 
dat.testL43 <- dat.filter %>% filter(Spp=="LATR"&year(Date)==2012&month(Date)==8)
testL43 <-  dlply(dat.testL43, .(LcNo), modelLRL43)
LRtestL43.coefs <-ldply(testL43, function(x) coef(x)[1:2])
testL43.pred <- merge(dat.testL43,LRtestL43.coefs,by="LcNo",all.x=TRUE)
testL43.pred <- testL43.pred %>%
  mutate(ETR.pred = ((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt))
ggplot(testL43.pred)+
  geom_point(aes(PAR,ETR))+
  geom_line(aes(PAR, ETR.pred),colour="blue")+
  geom_line(aes(PAR, ((560*PAR)/1800) *exp((1-PAR)/1800)), colour="red")+
  facet_wrap(.~LcNo)
ggplot(testL43.pred, aes(Date, ETRmax))+geom_point()

########################L44

########################L45
dat.filter %>% filter(Spp=="LATR"&year(Date)==2012&month(Date)==10)%>%
  ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
  geom_line(aes(PAR, ((600*PAR)/1800) *exp((1-PAR)/1800)), colour="red")
facet_grid(year(Date)~month(Date))+
  theme(legend.position = "none")+
  labs(title="LATR")

modelLRL45 <- function(df) {nls(ETR~((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt), df,
                                start= list(ETRmax =600,Eopt= 1800), 
                                nls.control(maxiter=1000, minFactor=1/8192))}


#ETRmax and Eopt values 
dat.testL45 <- dat.filter %>% filter(Spp=="LATR"&year(Date)==2012&month(Date)==10)
testL45 <-  dlply(dat.testL45, .(LcNo), modelLRL45)
LRtestL45.coefs <-ldply(testL45, function(x) coef(x)[1:2])
testL45.pred <- merge(dat.testL45,LRtestL45.coefs,by="LcNo",all.x=TRUE)
testL45.pred <- testL45.pred %>%
  mutate(ETR.pred = ((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt))
ggplot(testL45.pred)+
  geom_point(aes(PAR,ETR))+
  geom_line(aes(PAR, ETR.pred),colour="blue")+
  geom_line(aes(PAR, ((600*PAR)/1800) *exp((1-PAR)/1800)), colour="red")+
  facet_wrap(.~LcNo)
ggplot(testL44.pred, aes(Date, ETRmax))+geom_point()

########################L46
########################L47
########################L48

########################L49
dat.filter %>% filter(Spp=="LATR"&year(Date)==2013&month(Date)==2)%>%
  ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
  geom_line(aes(PAR, ((240*PAR)/1800) *exp((1-PAR)/1800)), colour="red")
facet_grid(year(Date)~month(Date))+
  theme(legend.position = "none")+
  labs(title="LATR")

modelLRL49 <- function(df) {nls(ETR~((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt), df,
                                start= list(ETRmax =240,Eopt= 1800), 
                                nls.control(maxiter=1000, minFactor=1/8192))}


#ETRmax and Eopt values 
dat.testL49 <- dat.filter %>% filter(Spp=="LATR"&year(Date)==2013&month(Date)==2)
testL49 <-  dlply(dat.testL49, .(LcNo), modelLRL49)
LRtestL49.coefs <-ldply(testL49, function(x) coef(x)[1:2])
testL49.pred <- merge(dat.testL49,LRtestL49.coefs,by="LcNo",all.x=TRUE)
testL49.pred <- testL49.pred %>%
  mutate(ETR.pred = ((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt))
ggplot(testL49.pred)+
  geom_point(aes(PAR,ETR))+
  geom_line(aes(PAR, ETR.pred),colour="blue")+
  geom_line(aes(PAR, ((240*PAR)/1800) *exp((1-PAR)/1800)), colour="red")+
  facet_wrap(.~LcNo)
ggplot(testL43.pred, aes(Date, ETRmax))+geom_point()

########################L50
dat.filter %>% filter(Spp=="LATR"&year(Date)==2013&month(Date)==3)%>%
  ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
  geom_line(aes(PAR, ((390*PAR)/1800) *exp((1-PAR)/1800)), colour="red")
facet_grid(year(Date)~month(Date))+
  theme(legend.position = "none")+
  labs(title="MUPO")

modelLRL50 <- function(df) {nls(ETR~((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt), df,
                                start= list(ETRmax =390,Eopt= 1800), 
                                nls.control(maxiter=1000, minFactor=1/8192))}


#ETRmax and Eopt values 
dat.testL50 <- dat.filter %>% filter(Spp=="LATR"&year(Date)==2013&month(Date)==3)
testL50 <-  dlply(dat.testL50, .(LcNo), modelLRL50)
LRtestL50.coefs <-ldply(testL50, function(x) coef(x)[1:2])
testL50.pred <- merge(dat.testL50,LRtestL50.coefs,by="LcNo",all.x=TRUE)
testL50.pred <- testL50.pred %>%
  mutate(ETR.pred = ((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt))
ggplot(testL50.pred)+
  geom_point(aes(PAR,ETR))+
  geom_line(aes(PAR, ETR.pred),colour="blue")+
  geom_line(aes(PAR, ((390*PAR)/1800) *exp((1-PAR)/1800)), colour="red")+
  facet_wrap(.~LcNo)
ggplot(testL50.pred, aes(Date, ETRmax))+geom_point()

########################L51
dat.filter %>% filter(Spp=="LATR"&year(Date)==2013&month(Date)==4)%>%
  ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
  geom_line(aes(PAR, ((400*PAR)/1800) *exp((1-PAR)/1800)), colour="red")
facet_grid(year(Date)~month(Date))+
  theme(legend.position = "none")+
  labs(title="MUPO")

modelLRL51 <- function(df) {nls(ETR~((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt), df,
                                start= list(ETRmax =400,Eopt= 1800), 
                                nls.control(maxiter=1000, minFactor=1/8192))}


#ETRmax and Eopt values 
dat.testL51 <- dat.filter %>% filter(Spp=="LATR"&year(Date)==2013&month(Date)==4)
testL51 <-  dlply(dat.testL51, .(LcNo), modelLRL51)
LRtestL51.coefs <-ldply(testL51, function(x) coef(x)[1:2])
testL51.pred <- merge(dat.testL51,LRtestL51.coefs,by="LcNo",all.x=TRUE)
testL51.pred <- testL51.pred %>%
  mutate(ETR.pred = ((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt))
ggplot(testL51.pred)+
  geom_point(aes(PAR,ETR))+
  geom_line(aes(PAR, ETR.pred),colour="blue")+
  geom_line(aes(PAR, ((400*PAR)/1800) *exp((1-PAR)/1800)), colour="red")+
  facet_wrap(.~LcNo)
ggplot(testL51.pred, aes(Date, ETRmax))+geom_point()

########################L52

dat.filter %>% filter(Spp=="LATR"&year(Date)==2013&month(Date)==5)%>%
  ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
  geom_line(aes(PAR, ((400*PAR)/1800) *exp((1-PAR)/1800)), colour="red")
facet_grid(year(Date)~month(Date))+
  theme(legend.position = "none")+
  labs(title="MUPO")

modelLRL52 <- function(df) {nls(ETR~((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt), df,
                                start= list(ETRmax =400,Eopt= 1800), 
                                nls.control(maxiter=1000, minFactor=1/8192))}


#ETRmax and Eopt values 
dat.testL52 <- dat.filter %>% filter(Spp=="LATR"&year(Date)==2013&month(Date)==5)
testL52 <-  dlply(dat.testL52, .(LcNo), modelLRL52)
LRtestL52.coefs <-ldply(testL50, function(x) coef(x)[1:2])
testL52.pred <- merge(dat.testL52,LRtestL52.coefs,by="LcNo",all.x=TRUE)
testL52.pred <- testL52.pred %>%
  mutate(ETR.pred = ((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt))
ggplot(testL52.pred)+
  geom_point(aes(PAR,ETR))+
  geom_line(aes(PAR, ETR.pred),colour="blue")+
  geom_line(aes(PAR, ((400*PAR)/1800) *exp((1-PAR)/1800)), colour="red")+
  facet_wrap(.~LcNo)
ggplot(testL50.pred, aes(Date, ETRmax))+geom_point()

########################L53

dat.filter %>% filter(Spp=="LATR"&year(Date)==2013&month(Date)==6)%>%
  ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
  geom_line(aes(PAR, ((420*PAR)/1800) *exp((1-PAR)/1800)), colour="red")
facet_grid(year(Date)~month(Date))+
  theme(legend.position = "none")+
  labs(title="MUPO")

modelLRL53 <- function(df) {nls(ETR~((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt), df,
                                start= list(ETRmax =420,Eopt= 1800), 
                                nls.control(maxiter=1000, minFactor=1/8192))}


#ETRmax and Eopt values 
dat.testL53 <- dat.filter %>% filter(Spp=="LATR"&year(Date)==2013&month(Date)==6)
testL53 <-  dlply(dat.testL53, .(LcNo), modelLRL53)
LRtestL53.coefs <-ldply(testL53, function(x) coef(x)[1:2])
testL53.pred <- merge(dat.testL53,LRtestL53.coefs,by="LcNo",all.x=TRUE)
testL53.pred <- testL53.pred %>%
  mutate(ETR.pred = ((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt))
ggplot(testL53.pred)+
  geom_point(aes(PAR,ETR))+
  geom_line(aes(PAR, ETR.pred),colour="blue")+
  geom_line(aes(PAR, ((420*PAR)/1800) *exp((1-PAR)/1800)), colour="red")+
  facet_wrap(.~LcNo)
ggplot(testL50.pred, aes(Date, ETRmax))+geom_point()

########################L54
dat.filter %>% filter(Spp=="LATR"&year(Date)==2013&month(Date)==7)%>%
  ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
  geom_line(aes(PAR, ((500*PAR)/1800) *exp((1-PAR)/1800)), colour="red")
facet_grid(year(Date)~month(Date))+
  theme(legend.position = "none")+
  labs(title="MUPO")

modelLRL54 <- function(df) {nls(ETR~((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt), df,
                                start= list(ETRmax =500,Eopt= 1800), 
                                nls.control(maxiter=1000, minFactor=1/8192))}


#ETRmax and Eopt values 
dat.testL54 <- dat.filter %>% filter(Spp=="LATR"&year(Date)==2013&month(Date)==7)
testL54 <-  dlply(dat.testL54, .(LcNo), modelLRL54)
LRtestL54.coefs <-ldply(testL54, function(x) coef(x)[1:2])
testL54.pred <- merge(dat.testL54,LRtestL54.coefs,by="LcNo",all.x=TRUE)
testL54.pred <- testL54.pred %>%
  mutate(ETR.pred = ((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt))
ggplot(testL54.pred)+
  geom_point(aes(PAR,ETR))+
  geom_line(aes(PAR, ETR.pred),colour="blue")+
  geom_line(aes(PAR, ((500*PAR)/1800) *exp((1-PAR)/1800)), colour="red")+
  facet_wrap(.~LcNo)
ggplot(testL50.pred, aes(Date, ETRmax))+geom_point()

########################L55

dat.filter %>% filter(Spp=="LATR"&year(Date)==2013&month(Date)==8)%>%
  ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
  geom_line(aes(PAR, ((650*PAR)/1800) *exp((1-PAR)/1800)), colour="red")
facet_grid(year(Date)~month(Date))+
  theme(legend.position = "none")+
  labs(title="MUPO")

modelLRL55 <- function(df) {nls(ETR~((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt), df,
                                start= list(ETRmax =650,Eopt= 1800), 
                                nls.control(maxiter=1000, minFactor=1/8192))}


#ETRmax and Eopt values 
dat.testL55 <- dat.filter %>% filter(Spp=="LATR"&year(Date)==2013&month(Date)==8)
testL55 <-  dlply(dat.testL55, .(LcNo), modelLRL50)
LRtestL55.coefs <-ldply(testL55, function(x) coef(x)[1:2])
testL55.pred <- merge(dat.testL55,LRtestL55.coefs,by="LcNo",all.x=TRUE)
testL55.pred <- testL55.pred %>%
  mutate(ETR.pred = ((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt))
ggplot(testL55.pred)+
  geom_point(aes(PAR,ETR))+
  geom_line(aes(PAR, ETR.pred),colour="blue")+
  geom_line(aes(PAR, ((650*PAR)/1800) *exp((1-PAR)/1800)), colour="red")+
  facet_wrap(.~LcNo)
ggplot(testL50.pred, aes(Date, ETRmax))+geom_point()

########################L56
dat.filter %>% filter(Spp=="LATR"&year(Date)==2013&month(Date)==9)%>%
  ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
  geom_line(aes(PAR, ((650*PAR)/1800) *exp((1-PAR)/1800)), colour="red")
facet_grid(year(Date)~month(Date))+
  theme(legend.position = "none")+
  labs(title="MUPO")

modelLRL56 <- function(df) {nls(ETR~((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt), df,
                                start= list(ETRmax =650,Eopt= 1800), 
                                nls.control(maxiter=1000, minFactor=1/8192))}


#ETRmax and Eopt values 
dat.testL56 <- dat.filter %>% filter(Spp=="LATR"&year(Date)==2013&month(Date)==9)
testL56 <-  dlply(dat.testL56, .(LcNo), modelLRL56)
LRtestL56.coefs <-ldply(testL56, function(x) coef(x)[1:2])
testL56.pred <- merge(dat.testL56,LRtestL56.coefs,by="LcNo",all.x=TRUE)
testL56.pred <- testL56.pred %>%
  mutate(ETR.pred = ((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt))
ggplot(testL56.pred)+
  geom_point(aes(PAR,ETR))+
  geom_line(aes(PAR, ETR.pred),colour="blue")+
  geom_line(aes(PAR, ((650*PAR)/1800) *exp((1-PAR)/1800)), colour="red")+
  facet_wrap(.~LcNo)
ggplot(testL50.pred, aes(Date, ETRmax))+geom_point()

########################L57

dat.filter %>% filter(Spp=="LATR"&year(Date)==2013&month(Date)==10)%>%
  ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
  geom_line(aes(PAR, ((530*PAR)/1800) *exp((1-PAR)/1800)), colour="red")
facet_grid(year(Date)~month(Date))+
  theme(legend.position = "none")+
  labs(title="MUPO")

modelLRL57 <- function(df) {nls(ETR~((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt), df,
                                start= list(ETRmax =530,Eopt= 1800), 
                                nls.control(maxiter=1000, minFactor=1/8192))}


#ETRmax and Eopt values 
dat.testL57 <- dat.filter %>% filter(Spp=="LATR"&year(Date)==2013&month(Date)==10)
testL57 <-  dlply(dat.testL57, .(LcNo), modelLRL57)
LRtestL57.coefs <-ldply(testL57, function(x) coef(x)[1:2])
testL57.pred <- merge(dat.testL57,LRtestL57.coefs,by="LcNo",all.x=TRUE)
testL57.pred <- testL57.pred %>%
  mutate(ETR.pred = ((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt))
ggplot(testL57.pred)+
  geom_point(aes(PAR,ETR))+
  geom_line(aes(PAR, ETR.pred),colour="blue")+
  geom_line(aes(PAR, ((530*PAR)/1800) *exp((1-PAR)/1800)), colour="red")+
  facet_wrap(.~LcNo)
ggplot(testL50.pred, aes(Date, ETRmax))+geom_point()

########################L58

dat.filter %>% filter(Spp=="LATR"&year(Date)==2013&month(Date)==11)%>%
  ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
  geom_line(aes(PAR, ((400*PAR)/1800) *exp((1-PAR)/1800)), colour="red")
facet_grid(year(Date)~month(Date))+
  theme(legend.position = "none")+
  labs(title="MUPO")

modelLRL58 <- function(df) {nls(ETR~((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt), df,
                                start= list(ETRmax =400,Eopt= 1800), 
                                nls.control(maxiter=1000, minFactor=1/8192))}


#ETRmax and Eopt values 
dat.testL58 <- dat.filter %>% filter(Spp=="LATR"&year(Date)==2013&month(Date)==11)
testL58 <-  dlply(dat.testL58, .(LcNo), modelLRL58)
LRtestL58.coefs <-ldply(testL58, function(x) coef(x)[1:2])
testL58.pred <- merge(dat.testL58,LRtestL58.coefs,by="LcNo",all.x=TRUE)
testL58.pred <- testL58.pred %>%
  mutate(ETR.pred = ((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt))
ggplot(testL58.pred)+
  geom_point(aes(PAR,ETR))+
  geom_line(aes(PAR, ETR.pred),colour="blue")+
  geom_line(aes(PAR, ((400*PAR)/1800) *exp((1-PAR)/1800)), colour="red")+
  facet_wrap(.~LcNo)
ggplot(testL50.pred, aes(Date, ETRmax))+geom_point()

########################L59

dat.filter %>% filter(Spp=="LATR"&year(Date)==2013&month(Date)==12)%>%
  ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
  geom_line(aes(PAR, ((300*PAR)/1800) *exp((1-PAR)/1800)), colour="red")
facet_grid(year(Date)~month(Date))+
  theme(legend.position = "none")+
  labs(title="MUPO")

modelLRL59 <- function(df) {nls(ETR~((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt), df,
                                start= list(ETRmax =300,Eopt= 1800), 
                                nls.control(maxiter=1000, minFactor=1/8192))}


#ETRmax and Eopt values 
dat.testL59 <- dat.filter %>% filter(Spp=="LATR"&year(Date)==2013&month(Date)==12)
testL59 <-  dlply(dat.testL59, .(LcNo), modelLRL59)
LRtestL59.coefs <-ldply(testL59, function(x) coef(x)[1:2])
testL59.pred <- merge(dat.testL59,LRtestL59.coefs,by="LcNo",all.x=TRUE)
testL59.pred <- testL59.pred %>%
  mutate(ETR.pred = ((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt))
ggplot(testL59.pred)+
  geom_point(aes(PAR,ETR))+
  geom_line(aes(PAR, ETR.pred),colour="blue")+
  geom_line(aes(PAR, ((300*PAR)/1800) *exp((1-PAR)/1800)), colour="red")+
  facet_wrap(.~LcNo)
ggplot(testL50.pred, aes(Date, ETRmax))+geom_point()

########################L60

dat.filter %>% filter(Spp=="LATR"&year(Date)==2014&month(Date)==1)%>%
  ggplot(.,aes(PAR,ETR, colour=factor(LcNo)))+geom_line()+
  geom_line(aes(PAR, ((115*PAR)/1800) *exp((1-PAR)/1800)), colour="red")
facet_grid(year(Date)~month(Date))+
  theme(legend.position = "none")+
  labs(title="MUPO")

modelLRL60 <- function(df) {nls(ETR~((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt), df,
                                start= list(ETRmax =115,Eopt= 1800), 
                                nls.control(maxiter=1000, minFactor=1/8192))}


#ETRmax and Eopt values 
dat.testL60 <- dat.filter %>% filter(Spp=="LATR"&year(Date)==2014&month(Date)==1)
testL60 <-  dlply(dat.testL60, .(LcNo), modelLRL60)
LRtestL60.coefs <-ldply(testL60, function(x) coef(x)[1:2])
testL60.pred <- merge(dat.testL60,LRtestL60.coefs,by="LcNo",all.x=TRUE)
testL60.pred <- testL60.pred %>%
  mutate(ETR.pred = ((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt))
ggplot(testL60.pred)+
  geom_point(aes(PAR,ETR))+
  geom_line(aes(PAR, ETR.pred),colour="blue")+
  geom_line(aes(PAR, ((115*PAR)/1800) *exp((1-PAR)/1800)), colour="red")+
  facet_wrap(.~LcNo)
ggplot(testL50.pred, aes(Date, ETRmax))+geom_point()














































setwd("C:/Users/maria/University of Texas at El Paso/Mauritz-Tozer, Marguerite E - MiniPAM/Lc_Analysis")
write.table(test1.pred, file="MUPO_2015_LightCurveFit.csv",sep=",",
            dec=".",row.names=FALSE)
mupo.2015 <-read.table(file="MUPO_2015_LightCurveFit.csv", header=TRUE,sep=",",dec=".")
mupo.2015 <- mupo.2015 %>%mutate(Date=as.Date(Date))
str(mupo.2015)


dat.test2 <- dat.filter %>% filter(Spp=="MUPO"&year(Date)<2018)
test2 <-  dlply(dat.test2, .(LcNo), modelLR)
LRtest2.coefs <-ldply(test2, function(x) coef(x)[1:2])
LRtest2.fit <-ldply(test2, function(x) summary(x)$parameters)
test2.pred <- merge(dat.test2,LRtest2.coefs,by="LcNo",all.x=TRUE)
test2.pred <- test2.pred %>%
  mutate(ETR.pred = ((ETRmax*PAR)/Eopt) *exp((1-PAR)/Eopt))
test2.pred%>%filter(year(Date)==2012)%>%
ggplot(.)+
  geom_point(aes(PAR,ETR,colour=as.factor(month(Date))))+
  geom_line(aes(PAR, ETR.pred),colour="blue")+
  facet_wrap(.~LcNo)


dat.filter%>%filter(year(Date)==2012&month(Date)==8)%>%
  ggplot(.,aes(PAR,ETR,colour=as.factor(month(Date))))+
  geom_point()+
  geom_smooth(method=lm, formula=y~poly(x,2),colour="red")+
  facet_wrap(.~LcNo)

colnames(dat.filter)

dat.2000<- dat.filter%>%filter(PAR==2000)
ggplot(dat.2000,aes(month(Date),ETR))+
  geom_point()+
  facet_grid(year(Date)~Spp)
