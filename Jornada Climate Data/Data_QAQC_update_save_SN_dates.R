
##########################################
#   Jornada Sensor Network Data          #
#      code: M. Mauritz, 18 Nov 2021     #
##########################################

# This code will:
# 1. allow data to be checked
# 2. run standard range filters determined from 2010-2019 and input from Ameriflux
# 3. allow year-specific data removal based on visual checks
# 4. save data with date/time in file name of SensorNetwork/Data/QAQC folder on server
# 5. save a csv file of this code as Data_QAQC_Code_yyyy.csv (or with date/time) to SensorNetwork/Data/QAQC folder to record data filter steps


# Units of data:
# rain, mm
# pressure, mbar
# leaf wetness (lws), no unit
# par, uE
# solar radiation (solar), Wm-2
# soil moisture (moisture), m-3/m-3
# battery, V
# voltage, V
# current, mA

# Sensor network distances on tramline
# SN1: 104m
# SN2: 87.5m
# SN3: 64m
# SN4: 24m
# SN5: 36m
# SN6: 22m
# SN7: 79.5m
# SN8: 3m

# sensors at each sensor netowrk node
# SN1: rain (latr, prgl), lws (latr), solar rad (prgl, flce, flce104), PAR (prgl, flce, flce104)
# SN2: rain (bare), lws (prgl), solar rad (latr, prgl), PAR (latr, prgl)
# SN3: rain (latr, prgl), solar rad (dapu, up), PAr (dapu, up), soil moisture (prgl)
# SN4: rain (bare), lws (down, up), solar rad (bare), PAr (bare), soil moisture (bare)
# SN5: solar rad (latr), PAr (latr), soil moisture (latr)
# SN6: rain (bare), lws (latr), solar rad (mupo, dapu, latr), PAR ((mupo, dapu, latr)
# SN7: lws (fle, mupo), solar rad (flce, mupo), PAR (flce, mupo), soil moisture (mupo)
# SN8: lws (prgl), solar rad (prgl, mupo), PAR (prgl, mupo)


# load libraries
library(data.table)
library(ggplot2)
library(lubridate)
library(gridExtra)
library(lattice)

# Get sensor network data from server, using compiled files
setwd("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Bahada/SensorNetwork/Data/")

year_file <- 2023

SN <- fread(paste("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Bahada/SensorNetwork/Data/WSN_",year_file,".csv",sep=""),
              header = TRUE, sep=",",
            na.strings=c(-9999,-888.88,"#NAME?"))

# column names with units
colnames2019 <- fread(file="/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Bahada/SensorNetwork/MetaData/JER_SensorNetwork_ColumnNames_2019.csv",
                      sep=",",
                      header=TRUE) 

colnames2019[, ':=' (sensor = sapply(strsplit(as.character(R_column_names),"_"),"[",1),
                     SN = sapply(strsplit(as.character(R_column_names),"_"),"[",2),
                     veg = sapply(strsplit(as.character(R_column_names),"_"),"[",3),
                     depth = sapply(strsplit(as.character(R_column_names),"_"),"[",4))]

SN[, date_time := ymd_hms(Date)][, Date:=date_time][, date_time:=NULL]

########## FUNCTION: fix data ###########
# for some reason HOBO used 1,000 for numbers > 999 and R is reading those as a character
# find character columns, they are the ones with ',', and then fix the ',' only in those specific columns
# some columns are all NA and those are read as 'logical'. Turn those into numeric columns

# Get the column names of each sensor network data set and
# compare to the columns that should be in the data

################### Run: Fix Column Format Function ######################
fix_columns <- function(SNdata,colnames_data) {
  char <- melt.data.table(SNdata[,lapply(.SD, function(x) {is.character(x)==TRUE})])
  char_col <- as.character(droplevels(char[value == TRUE,]$variable))
  # columns that are all 'NA' are imported as logical
  logi_col <- melt.data.table(SNdata[,lapply(.SD, function(x) {is.logical(x)==TRUE})])
  logi_col2 <- as.character(droplevels(logi_col[value == TRUE,]$variable))
  # create combined list of all columns that need to be corrected
  remove_cols <- c(char_col, logi_col2)
  
  # fix the character columns
  char_fix <- SNdata[, lapply(.SD,function(x) {as.numeric(gsub(",", "", as.character(x)))}),
                     .SDcols=char_col]
  # fix the logical columns
  logi_fix <- SNdata[, lapply(.SD,function(x) {as.numeric(x)}),
                     .SDcols=logi_col2]
  
  # recombine fixed data with original data that has bad columns removed,
  # return.
  # return only data that combines columns that needed fixing
  # if a column didn't need to be fixed then char_fix or log_fix will be nrow==0
  
  # fix only character
  if(nrow(logi_fix)==0 & nrow(char_fix)!=0) 
  {fixed <- cbind(SNdata[,!..remove_cols],char_fix)}
  # fix only logical
  if(nrow(logi_fix)!=0 & nrow(char_fix)==0) 
  {fixed <- cbind(SNdata[,!..remove_cols],logi_fix)}
  # fix character and logical
  if(nrow(logi_fix)!=0 & nrow(char_fix)!=0) 
  {fixed <- cbind(SNdata[,!..remove_cols],char_fix,logi_fix)}  
  # nothing to fix
  if(nrow(logi_fix)==0 & nrow(char_fix)==0) 
  {fixed <- SNdata} 
  
  # get the column names of each sensor network data set and compare to the columns that should be in the data
  # check column names
  namecheck <- data.table(variable = (colnames(SNdata)))
  namechecka <- setdiff(namecheck[,variable], colnames_data[,variable])
  
  print(namechecka)
  
  fixed_data <- fixed
  name_check <- namechecka
  
  return_list <- return (list(fixed_data,name_check))
}
###########################

# some columns get imported as logical. No idea why. 
# fix logical columns here
SN_fx <- fix_columns(SN,colnames2019)
SN_fx_data <- SN_fx[[1]]
colcheck_logi <- SN_fx[[2]]

SN_long <- melt.data.table(SN_fx_data,c("Date"))

# merge the column names to the data and split into additional descriptors
# descriptors are: sensor, SN, veg, depth
SN_long <- merge(SN_long, colnames2019, by="variable")

# check start and end dates of data
startdate.check <- (min(SN_long$Date))
enddate.check <- (max(SN_long$Date))


# calculate half-hour means using ceiling date which takes each time to the next half-hour,
# ie: 15:00:01 goes to 15:30, etc. 
SN_30min <- SN_long[sensor!="rain", list(mean.val = mean(value, na.rm=TRUE),
                                         unit=unique(unit)),
                         by=.(SN,veg,depth,sensor,ceiling_date(Date,"30 minutes"))][,date_time := ceiling_date][,ceiling_date:=NULL]

# for precip calculate the sum. Ignore NA because the way the data is offloaded from the sensors creates lots of NAs when the timestamps
# don't perfectly match across all the SN. The result is that NAs get introduced where they shouldn't be, and I lose rain events.
SN_30min_rain <- SN_long[sensor=="rain", list(mean.val = sum(value, na.rm=TRUE),
                                              unit=unique(unit)),
                              by=.(SN,veg,depth,sensor,ceiling_date(Date,"30 min"))][,date_time := ceiling_date][,ceiling_date:=NULL]


SN_30min <- rbind(SN_30min, SN_30min_rain)

# break up the date column
SN_30min[,year:= year(date_time)]
SN_30min[,month:= month(date_time)]
SN_30min[,doy:= yday(date_time)]

# sensor options:
levels(as.factor(SN_30min$sensor))
# "battery"  "current"  "lws"      "moisture" "par"      "pressure" "rain"     "solar"    "voltage" 

# just check
levels(as.factor(SN_30min$veg))
levels(as.factor(SN_30min$depth))

# make a back-up 30min dataframe if I make mistakes during removing.
# Then I don't have to recalculate the means each time there's a mistake.
SN_30min1 <- copy(SN_30min)
# to fix errors:
# SN_30min <- copy(SN_30min1)

# battery voltage: that will tell me which networks are completely out.
ggplot(SN_30min[sensor=="battery",], aes(date_time, mean.val, colour=SN))+
  geom_line()+
  labs(title="Battery Voltage") +
  facet_grid(SN~., scales="free_y")

# 2022 (June 6): no voltage on SN5, SN6, SN7

# SN1: rain (latr, prgl), lws (latr), solar rad (prgl, flce, flce104), PAR (prgl, flce, flce104)
# SN2: rain (bare), lws (prgl), solar rad (latr, prgl), PAR (latr, prgl)
# SN3: rain (latr, prgl), solar rad (dapu, up), PAr (dapu, up), soil moisture (prgl)
# SN4: rain (bare), lws (down, up), solar rad (bare), PAr (bare), soil moisture (bare)
# SN5: solar rad (latr), PAr (latr), soil moisture (latr)
# SN6: rain (bare), lws (latr), solar rad (mupo, dapu, latr), PAR ((mupo, dapu, latr)
# SN7: lws (fle, mupo), solar rad (flce, mupo), PAR (flce, mupo), soil moisture (mupo)
# SN8: lws (prgl), solar rad (prgl, mupo), PAR (prgl, mupo)

# REMOVE OBVIOUSLY BAD DATA POINTS
# lws values should be between 0-100
# lws: remove values <0 and >100
SN_30min[sensor=="lws" & (mean.val<0 | mean.val>100), mean.val := NA]

# lws
# can't be negative or >100
ggplot(SN_30min[sensor=="lws",], aes(date_time, mean.val, colour=SN))+
  geom_line()+
  labs(title="Leaf Wetness") +
  facet_grid(veg~., scales="free_y")

# rain
# can't be negative
# can't be greater than 20, unlikely to be greater than 4
ggplot(SN_30min[sensor=="rain",], aes(date_time, mean.val, colour=SN))+
  geom_point()+
  geom_line()+
  labs(title="Rain") +
  facet_grid(veg~., scales="free_y")


# The maximum listed intensity in the specifications is 2 to 3 in./hr, which would give an accuracy of +0%, -5%.
# 1inch/hour = max accuracy
# 2-3 inch/hour ~ 50-75mm/hour ~ 25-48mm/30min
# https://s.campbellsci.com/documents/eu/manuals/te525.pdf
SN_30min[sensor=="rain" & (mean.val<0 | mean.val>50), mean.val := NA]

# on Nov 12th 2021 ~10:15 cleaned rain bucket under PRGl and move tipping scale. 
# remove "rain" on this day
# SN_30min[sensor=="rain" & date(date_time) == as.Date("2021-11-12") & veg=="PRGL" & mean.val>0 , mean.val := NA]

# 2022: max value filter removes value ~800 in PRGL.

# graph lws and rain, by cover type
ggplot(SN_30min[sensor=="rain" | sensor =="lws",], aes(date_time, mean.val, colour=SN))+
  geom_point()+
  geom_line()+
  labs(title="Leaf wetness and Rain") +
  facet_grid(sensor~veg, scales="free_y")

# graph lws and rain, grouped for all covers
ggplot(SN_30min[sensor=="rain" | sensor =="lws",], aes(date_time, mean.val, colour=SN))+
  geom_point()+
  geom_line()+
  labs(title="Leaf wetness and Rain") +
  facet_grid(sensor~., scales="free_y")

# lws should be between 0-100.
# 2022 has some values>100 in Aug/Sep
SN_30min[sensor=="lws" & (mean.val<0 | mean.val>100), mean.val := NA]

# graph again:  lws and rain, grouped for all covers
ggplot(SN_30min[sensor=="rain" | sensor =="lws",], aes(date_time, mean.val, colour=SN))+
  geom_point()+
  geom_line()+
  labs(title="Leaf wetness and Rain") +
  facet_grid(sensor~., scales="free_y")



# moisture
# remove periods when the sensors are bad. 
# plot
ggplot(SN_30min[sensor=="moisture",],
       aes(date_time, mean.val, colour=factor(depth)))+
  geom_line(aes(linetype=factor(depth)))+
  labs(title="Soil Moisture") +
  facet_grid(veg~., scales="free_y")

# plot soil moisture with rain
ggplot(SN_30min[sensor%in%c("moisture","rain"),], aes(date_time, mean.val, colour=factor(depth)))+
  geom_point(size=0.1)+
  labs(title="Soil Moisture, and Rain") +
  facet_grid(sensor+veg~., scales="free_y")+
  theme_bw()

# 2023: only BARE has soil moisture (SN4)
# 2023: from 22 may to 27 may, after a rain event the 5cm soil probe declines
# and fluctuates daily until it returns to 'baseline' (when the soil is dry?)

SN_30min[sensor=="moisture"&veg=="BARE"&depth==5&
           date_time >ymd_hms("2023-05-22 00:00:00") &
           date_time <ymd_hms("2023-05-28 00:00:00"), mean.val := NA]


# 2022: only BARE has soil moisture (SN4)
# LATR: SN5, SN data logger not working
# MUPO: SN7, , SN data logger not working
# PRGL: SN3, sensors got moved on ~19 June 2019

# 2022 Bare 10cm is very highin September 15 - 19 2022
# comes after some heavy rain but does not directly coincide with rain.
# other depths do not show the pattern.
# remove
# SN_30min[sensor=="moisture"&veg=="BARE"&depth==10&
# date_time >ymd_hms("2022-09-14 22:30:00") &
#   date_time <ymd_hms("2022-09-19 02:30:00"), mean.val := NA]


# MUPO 10cm: 13 July 2021 there was a baseline shift! Remove after this.
# SN_30min[sensor=="moisture"&veg=="MUPO"&depth==10&date_time>as.Date("2021-07-13"), mean.val := NA]

# MUPO 20cm: in October 2018 there was a baseline shift! Remove after this.
# SN_30min[sensor=="moisture"&veg=="MUPO"&depth==20, mean.val := NA]

# MUPO 30cm: 11 July 2021 there was a baseline shift! Remove after this.
# SN_30min[sensor=="moisture"&veg=="MUPO"&depth==30&date_time>as.Date("2021-07-11"), mean.val := NA]


# PRGL remove all after January 2019 (sensors got moved on ~19 June 2019)
# SN_30min[sensor=="moisture"&veg=="PRGL"&date_time>=as.Date("2019-01-01"),
#         mean.val := NA]


# LATR 5cm: probe goes bad after 12 Feb 2017
# SN_30min[sensor=="moisture"&veg=="LATR"&depth==5&
#           date_time >= as.Date("2017-02-12"), mean.val := NA]

# LATR 10cm: probe had a baseline shift 14 Feb 2019 at 23:30. Remove values after this.
# SN_30min[sensor=="moisture"&veg=="LATR"&depth==10&#
#           (date_time>=as.POSIXct("2019-02-14 23:00:00", tz="UTC")), mean.val := NA]

# LATR 20cm: had baseline shift after 28 Feb 2019 18:00 , remove
# SN_30min[sensor=="moisture"&veg=="LATR"&depth==20&
#           date_time >= as.POSIXct("2019-02-28 18:00:00", tz="UTC"), mean.val := NA]

# BARE 5cm: drifts after July 2021 rain event then looks Ok again. Remove between dates
# 1 July and 15 October
# SN_30min[sensor=="moisture"&veg=="BARE"&depth==5&
#           date_time >= as.Date("2021-07-01") & 
#           date_time <= as.Date("2021-10-15"), mean.val := NA]

# 01-09-2022 all bare look OK

# plot soil moisture after corrections/filters
ggplot(SN_30min[sensor=="moisture",],
       aes(date_time, mean.val, colour=factor(depth)))+
  geom_line(aes(linetype=factor(depth)))+
  labs(title="Soil Moisture") +
  facet_grid(veg~., scales="free_y")


# plot rain, lws, soil moisture together
ggplot(SN_30min[sensor%in%c("moisture","rain","lws"),], aes(date_time, mean.val, colour=factor(depth)))+
  geom_point(size=0.1)+
  labs(title="Leaf Wetness, Soil Moisture, and Rain") +
  facet_grid(sensor+veg~., scales="free_y")+
  theme_bw()

# par
# par can't be <0 
SN_30min[sensor=="par" & mean.val<0, mean.val := NA]
# downward-facing unlikely to be >1000
SN_30min[sensor=="par" & veg!="UP" & mean.val>1000, mean.val := NA]

# 2021 downward-facing sensors had sporadic outlying values >600. remove
# SN_30min[sensor=="par" & veg!="UP" & mean.val>500, mean.val := NA]
# In May 2021 FLCE high value was not removed by >500 but if I use a lower cutoff then I'll remove January high values that could be real due to snow
# SN_30min[sensor=="par" & veg=="FLCE" & month==5 & mean.val>400, mean.val := NA]

# graph
ggplot(SN_30min[sensor=="par",], aes(date_time, mean.val, colour=SN))+
  geom_line()+
  labs(title="PAR") +
  facet_grid(veg~., scales="free_y")

# 2022 PAR MUPO on SN8 is no longer good. remove all
SN_30min[sensor=="par" & veg=="MUPO" & SN=="SN8", mean.val := NA]

# pressure
# can't be negative
# pressure: can't be <0
SN_30min[sensor=="pressure" & mean.val<0, mean.val := NA]

# 2022: pressure on SN5 which isn't working

ggplot(SN_30min[sensor=="pressure",], aes(date_time, mean.val, colour=SN))+
  geom_line()+
  labs(title="Pressure") +
  facet_grid(veg~., scales="free_y")


# solar
# can't be negative
SN_30min[sensor=="solar" & mean.val<0, mean.val := NA]

ggplot(SN_30min[sensor=="solar",],
       aes(date_time, mean.val, colour=SN))+
  geom_line()+
  labs(title="Solar Radiation") +
  facet_grid(veg~., scales="free_y")

# plot all data in succession, after corrections

# battery/current: that will tell me which networks are completely out.
ggplot(SN_30min[sensor=="battery",], aes(date_time, mean.val, colour=SN))+
  geom_line()+
  labs(title="Battery Voltage") +
  facet_grid(SN~., scales="free_y")

# lws
# can't be negative or >100
ggplot(SN_30min[sensor=="lws",], aes(date_time, mean.val, colour=SN))+
  geom_line()+
  labs(title="Leaf Wetness") +
  facet_grid(veg~., scales="free_y")

# moisture
# remove periods when the sensors are bad. (see above)
ggplot(SN_30min[sensor=="moisture",], aes(date_time, mean.val, colour=factor(depth)))+
  geom_line(aes(linetype=factor(depth)))+
  labs(title="Soil Moisture") +
  facet_grid(veg~., scales="free_y")

# par
ggplot(SN_30min[sensor=="par",], aes(date_time, mean.val, colour=SN))+
  geom_line()+
  labs(title="PAR") +
  facet_grid(veg~., scales="free_y")

# pressure
# can't be negative
# not logged in 2022 because SN5 is down
ggplot(SN_30min[sensor=="pressure",], aes(date_time, mean.val, colour=SN))+
  geom_line()+
  labs(title="Pressure") +
  facet_grid(veg~., scales="free_y")

# rain
# can't be negative
# can't be greater than 20, unlikely to be greater than 4
ggplot(SN_30min[sensor=="rain",], aes(date_time, mean.val, colour=SN))+
  geom_point()+
  geom_line()+
  labs(title="Rain") +
  facet_grid(veg~., scales="free_y")

# solar
# can't be negative
ggplot(SN_30min[sensor=="solar",],
       aes(date_time, mean.val, colour=SN))+
  geom_line()+
  labs(title="Solar Radiation") +
  facet_grid(veg~., scales="free_y")

# put data back in wide format to save
SN_save <- copy(SN_30min[,variableID := paste(SN,sensor,unit,veg,depth, sep="_")])

SN_wide_save <- data.table:: dcast(SN_save[!is.na(date_time),
                                               .(date_time, variableID, mean.val)],
                                     date_time~variableID,
                                     value.var="mean.val")

# save to QAQC folder on data archive
startdate <- (min(SN_wide_save$date_time))
enddate <- (max(SN_wide_save$date_time))

# add comment about processing
print(paste("#",year(enddate.check), "data processed until",enddate.check,sep=" "))
# 2022 data processed until 2022-12-31 23:55:00

# # save in QAQC folder with start and end date in the file name
qaqc.path<- paste("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Bahada/SensorNetwork/Data/QAQC/", sep="")
setwd(qaqc.path)

# # FOR INCOMPLETE YEARS
# write.table(SN_wide_save,
#             paste("WSN_L2_",year(startdate),sprintf("%02d",(month(startdate))),sprintf("%02d",(day(startdate))),
#                   sprintf("%02d",(hour(startdate))),sprintf("%02d",(minute(startdate))),
#                   sprintf("%02d",(second(startdate))),
#                   "_",
#                   year(enddate),sprintf("%02d",(month(enddate))),sprintf("%02d",(day(enddate))),
#                   sprintf("%02d",(hour(enddate))),sprintf("%02d",(minute(enddate))),
#                   sprintf("%02d",(second(enddate))), ".csv",sep=""),
#             sep=",", dec=".", row.names=FALSE)


# save data with year in name to QAQC together with text file of date range
write.table(SN_wide_save,
            paste("WSN_L2_",year_file, ".csv",sep=""),
            sep=",", dec=".", row.names=FALSE)

# save a text file that says date that code was run (system time), start and end date of data
run.info <- data.frame(info=c("Data_start","Data_end","Date_processed"),
                       date_time=c(startdate,enddate,ymd_hms(Sys.time(),tz="UTC")))

write.table(run.info, paste("WSN_L2_DateRange_",year_file,"..csv",sep=""),
            sep=",", dec=".", row.names=FALSE)


# save the R script that went along with creating the file to have a record of QA/QC
# use rstudioapi to get the path of the current script and then copy it to the 
# server location

# http://theautomatic.net/2018/07/11/manipulate-files-r/ 
# file.copy("source_file.txt", "destination_folder")

# file.rename(from = rstudioapi::getActiveDocumentContext()$path,
#            # to = file.path("/Volumes/SEL_Data_Archive/Research Data/Desert/Jornada/Bahada/Tower/TowerClimate_met/2019/QAQC/",
#            to = file.path("~/Desktop",                
#            paste("Data_QAQC_update_save_Climate_",year(startdate),sprintf("%02d",(month(startdate))),sprintf("%02d",(day(startdate))),
#                                  sprintf("%02d",(hour(startdate))),sprintf("%02d",(minute(startdate))),
#                                 sprintf("%02d",(second(startdate))),
#                                 "_",
#                                  year(enddate),sprintf("%02d",(month(enddate))),sprintf("%02d",(day(enddate))),
#                                  sprintf("%02d",(hour(enddate))),sprintf("%02d",(minute(enddate))),
#                                 sprintf("%02d",(second(enddate))), ".csv",sep="")))


file.copy(from = rstudioapi::getActiveDocumentContext()$path,
          to = file.path(qaqc.path,
                         #to = file.path("~/Desktop",                
                         paste("Data_QAQC_Code_",year_file, ".csv",sep="")))

# If response: [TRUE] the code save worked. If [FALSE], the file already exists. Remove and run again. 


 