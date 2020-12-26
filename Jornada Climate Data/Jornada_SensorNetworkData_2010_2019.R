
##########################################
#   Jornada Sensor Network Data 2015     #
#      code: M. Mauritz, 21 Nov 2018     #
# modified April 2019 to add 2010-2019   #
##########################################

# 26 Dec 2020 update: add QA/QC from ameriflux to limit LWS below 100 and remove LATR 10,20cm and MUPO 20cm baseline shifts
# 8 Apr 2020 update: update to March 24 and add timestamp correction due to daylight savings changes
# JER_Tower_SN_TimestampMismatches.xlsx
# make the timestamp adjustments at the end, after filtering data. 


# Jornada Precip Data: import and explore

library(data.table)
library(ggplot2)
library(lubridate)
library(gridExtra)
library(lattice)


# copied files from server to computer
# server source: "/Volumes/SEL_Data_Archive/Research Data/Desert/Jornada/UtepJrnBahadaSite/"

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

########## FUNCTION: fix data ###########
# for some reason HOBO used 1,000 for numbers > 999 and R is reading those as a character
# find character columns, they are the ones with ',', and then fix the ',' only in those specific columns
# some columns are all NA and those are read as 'logical'. Turn those into numeric columns

# Also get the column names of each sensor network data set and compare to the columns that should be in the data

################# Fix columns function ########################
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


setwd("~/Desktop/TweedieLab/Projects/Jornada/Data/SensorNetwork/")

# fread imports as data table
# list all csv files in relevant folder
SNfiles <- list.files(path="~/Desktop/TweedieLab/Projects/Jornada/Data/SensorNetwork", full.names=TRUE, pattern="*.csv") 

# read metadata file to get the column names
# do not use column names direclty, instead merge with column names that are in the datafiles themselves
# column names for 2010 to 2014
colnames2010 <- fread(file="~/Desktop/TweedieLab/Projects/Jornada/Data/SensorNetwork/MetaData/JER_SensorNetwork_ColumnNames_2010_2014.csv",
                    sep=",",
                    header=TRUE) 

colnames2010[, ':=' (sensor = sapply(strsplit(as.character(R_column_names),"_"),"[",1),
                          SN = sapply(strsplit(as.character(R_column_names),"_"),"[",2),
                          veg = sapply(strsplit(as.character(R_column_names),"_"),"[",3),
                          depth = sapply(strsplit(as.character(R_column_names),"_"),"[",4))]


# column names for 2015 onward 
# after 2015 there are a few erroneous sensor columns that creep in. 
# these are either emty or contain some brief data, but we're not sure what that data is
# and it often doesn't look like good data
# colnames.

# 2019 is is a complete list of all columns that SHOULD be in the data and is an update on colnames2015
# use that to screen and remove extra columns
# 2019 also contains the names after moisture sensors were moved in June 2019 for full profiles in BARE and LATR
colnames2019 <- fread(file="~/Desktop/TweedieLab/Projects/Jornada/Data/SensorNetwork/MetaData/JER_SensorNetwork_ColumnNames_2019.csv",
                      sep=",",
                      header=TRUE) 

colnames2019[, ':=' (sensor = sapply(strsplit(as.character(R_column_names),"_"),"[",1),
                     SN = sapply(strsplit(as.character(R_column_names),"_"),"[",2),
                     veg = sapply(strsplit(as.character(R_column_names),"_"),"[",3),
                     depth = sapply(strsplit(as.character(R_column_names),"_"),"[",4))]


# read files and bind them into one file.
# read 2010 - 2014 (different format and column headers than files after 2016)
SN_2010_2014 <- do.call("rbind", lapply(SNfiles[1:5], header = TRUE, fread, sep=",",
                                   na.strings=c(-9999,-888.88,"#NAME?","NA")))

# after 2014 the format varies so read in each sensor network one by one
sink("/dev/null") # this command suppresses output, otherwise R prints all the content of the files
lapply(SNfiles[6:length(SNfiles)], function(fname){
  input<- fread(fname, sep=",", header=TRUE, na.strings=c(-9999,-888.88,"#NAME?","NA"))
  obj_name <- paste("SN",
                    strsplit(as.character(tools::file_path_sans_ext(basename(fname))),"_","[",2)[[1]][2],
                    sep="_")
  assign(obj_name, value=input, env = .GlobalEnv)
  #melt(obj_name, id.vars= c("plot_id", "fence", "plot", "WW", "SW", "date", "F_GW", "metric"),
  #variable.name = "species",
  #value.name = "obj_name")
})
sink()


# create date format, date formats vary over the years

# source("~/Desktop/R/R_programs/Functions/TimezoneConversion_DT_to_ST.R")
# 
# SN_2010_2014[, date_time := as.POSIXct(Date, format=c("%Y-%m-%d %H:%M:%S", tz="America/Denver"))][, Date:=date_time][, date_time:=NULL]
# SN_2015[, date_time := as.POSIXct(Date, format=c("%m/%d/%y %H:%M", tz="America/Denver"))][, Date:=date_time][, date_time:=NULL]
# SN_2016[, date_time := as.POSIXct(Date, format=c("%m/%d/%y %H:%M", tz="America/Denver"))][, Date:=date_time][, date_time:=NULL]
# SN_2017[, date_time := as.POSIXct(Date, format=c("%m/%d/%y %H:%M", tz="America/Denver"))][, Date:=date_time][, date_time:=NULL]
# SN_2018[, date_time := as.POSIXct(Date, format=c("%m/%d/%y %H:%M", tz="America/Denver"))][, Date:=date_time][, date_time:=NULL]
# SN_2019[, date_time := as.POSIXct(Date, format=c("%m/%d/%y %H:%M", tz="America/Denver"))][, Date:=date_time][, date_time:=NULL]

SN_2010_2014[, date_time := parse_date_time(Date, c("Ymd HMS","mdy HM"),tz="UTC")][, Date:=date_time][, date_time:=NULL]
SN_2015[, date_time := mdy_hm(Date,tz="UTC")][, Date:=date_time][, date_time:=NULL]
SN_2016[, date_time := mdy_hm(Date,tz="UTC")][, Date:=date_time][, date_time:=NULL]
SN_2017[, date_time := mdy_hm(Date,tz="UTC")][, Date:=date_time][, date_time:=NULL]
SN_2018[, date_time := mdy_hm(Date,tz="UTC")][, Date:=date_time][, date_time:=NULL]
SN_20190101000000[, date_time := mdy_hm(Date,tz="UTC")][, Date:=date_time][, date_time:=NULL]
SN_20190401000000[, date_time := ymd_hms(Date,tz="UTC")][, Date:=date_time][, date_time:=NULL]
SN_20190513130000[, date_time := ymd_hms(Date,tz="UTC")][, Date:=date_time][, date_time:=NULL]
SN_20200130125500[, date_time := ymd_hms(Date,tz="UTC")][, Date:=date_time][, date_time:=NULL]
SN_20200210081500[, date_time := ymd_hms(Date,tz="UTC")][, Date:=date_time][, date_time:=NULL]


# fix character and logical columns in each data set so that they can be melted
# melt.data.table will give an error. This shouldn't be anything to worry about. 
# check that output makes sense by looking at str() and seeing if everything is num or int
# Date should still be POSIX
# and check the column names

# 2010 to 2014
fix_2010 <- fix_columns(SN_2010_2014,colnames2010)
SN_2010_2014_fx <- fix_2010[[1]]
colcheck_2010 <- fix_2010[[2]]

# 2015
fix_2015 <- fix_columns(SN_2015,colnames2019)
SN_2015_fx <- fix_2015[[1]]
colcheck_2015 <- fix_2015[[2]]

# plot(SN_2015_fx[,("Solar Radiation (S-LIB 2384926:607214349-1), W/m^2, SN_01"),with=FALSE])
# plot(SN_2015_fx[,("PAR (S-LIA 2384926:2134851391-1), uE, SN_01"),with=FALSE])

# SN_2015_fx[,c(colcheck_2015) := NULL]

# # check the 2015 data a litle better. Weird stuff with time.
# SN_2015_long <- melt.data.table(SN_2015_fx,c("Date"))
# SN_2015_long <- merge(SN_2015_long, colnames2015, by="variable")
# 
# setorder(SN_2015_long, R_column_names,Date)
# 
# SN_2015_long[,timecheck:=Date-shift(Date),by=R_column_names]
# 
# ggplot(SN_2015_long[sensor=="battery",], aes(Date,timecheck))+geom_point()+facet_grid(R_column_names~.)

# 2016
fix_2016 <- fix_columns(SN_2016,colnames2019)
SN_2016_fx <- fix_2016[[1]]
colcheck_2016 <- fix_2016[[2]]

# plot(SN_2016_fx[,("PAR (S-LIA 2384926:2134851391-1), uE, SSn_2015_longN_01"),with=FALSE])

SN_2016_fx[,c(colcheck_2016) := NULL]

# 2017
fix_2017 <- fix_columns(SN_2017,colnames2019)
SN_2017_fx <- fix_2017[[1]]
colcheck_2017 <- fix_2017[[2]]

# plot(SN_2017_fx[,("PAR (S-LIA 2384926:2134851391-1), uE, SN_01"),with=FALSE])

SN_2017_fx[,c(colcheck_2017) := NULL]

# 2018
fix_2018 <- fix_columns(SN_2018,colnames2019)
SN_2018_fx <- fix_2018[[1]]
colcheck_2018 <- fix_2018[[2]]

# plot(SN_2018_fx[,("Solar Radiation (S-LIB 2384926:607214349-1), W/m^2, SN_01"),with=FALSE])

SN_2018_fx[,c(colcheck_2018) := NULL]

# 2019
fix_2019_1 <- fix_columns(SN_20190101000000,colnames2019)
SN_2019_1_fx <- fix_2019_1[[1]]
colcheck_2019_1 <- fix_2019_1[[2]]
SN_2019_1_fx[,c(colcheck_2019_1) := NULL]


fix_2019_2 <- fix_columns(SN_20190401000000,colnames2019)
SN_2019_2_fx <- fix_2019_2[[1]]
colcheck_2019_2 <- fix_2019_2[[2]]
SN_2019_2_fx[,c(colcheck_2019_2) := NULL]

fix_2019_3 <- fix_columns(SN_20190513130000,colnames2019)
SN_2019_3_fx <- fix_2019_3[[1]]
colcheck_2019_3 <- fix_2019_3[[2]]
SN_2019_3_fx[,c(colcheck_2019_3) := NULL]


fix_2020_1 <- fix_columns(SN_20200130125500,colnames2019)
SN_2020_1_fx <- fix_2020_1[[1]]
colcheck_2020_1 <- fix_2020_1[[2]]
SN_2020_1_fx[,c(colcheck_2020_1) := NULL]


fix_2020_2 <- fix_columns(SN_20200210081500,colnames2019)
SN_2020_2_fx <- fix_2020_2[[1]]
colcheck_2020_2 <- fix_2020_2[[2]]
SN_2020_2_fx[,c(colcheck_2020_2) := NULL]


# combine 2015-2020 
SN_2015_2020_fx <- rbind(SN_2015_fx,SN_2016_fx,SN_2017_fx,SN_2018_fx, SN_2019_1_fx, SN_2019_2_fx,
                         SN_2019_3_fx, SN_2020_1_fx, SN_2020_2_fx, fill=TRUE)


# change the format of the data from wide to long so it's easier to work with
# melt will give an error because of int and num column str
# all data will be in "value" column
SN_2010_2014_long <- melt.data.table(SN_2010_2014_fx,c("Date"))
SN_2015_2020_long <- melt.data.table(SN_2015_2020_fx,c("Date"))

# merge the column names to the data and split into additional descriptors
# descriptors are: sensor, SN, veg, depth
SN_2010_2014_long <- merge(SN_2010_2014_long, colnames2010, by="variable")
SN_2015_2020_long <- merge(SN_2015_2020_long, colnames2019, by="variable")

# # combine data from all years
 SN_2010_2020 <- rbind(SN_2010_2014_long, SN_2015_2020_long)

# remove duplicates from SN_2010_2020
SN_2010_2020 <- SN_2010_2020[!duplicated(SN_2010_2020),]

# calculate half-hour means using ceiling date which takes each time to the next half-hour,
# ie: 15:00:01 goes to 15:30, etc. 
SN_30min <- SN_2010_2020[sensor!="rain", list(mean.val = mean(value, na.rm=TRUE)),
by=.(SN,veg,depth,sensor,ceiling_date(Date,"30 minutes"))][,date_time := ceiling_date][,ceiling_date:=NULL]

# for precip calculate the sum. Ignore NA because the way the data is offloaded from the sensors creates lots of NAs when the timestamps
# don't perfectly match across all the SN. The result is that NAs get introduced where they shouldn't be, and I lose rain events.
SN_30min_rain <- SN_2010_2020[sensor=="rain", list(mean.val = sum(value, na.rm=TRUE)),
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


# make figures to figure out reasonable values and remove anything outside of range
# check data and remove complete outliers

# make a back-up 30min dataframe if I make mistakes during removing.
# Then I don't have to recalculate the means each time there's a mistake.
SN_30min1 <- copy(SN_30min)
# to fix errors:
# SN_30min <- copy(SN_30min1)

# REMOVE OBVIOUSLY BAD DATA POINTS
# lws values should be between 0-100
# lws: remove values <0 and >100
SN_30min[sensor=="lws" & (mean.val<0 | mean.val>100), mean.val := NA]


##### moisture: 
# MUPO 30cm: July 2013 it looks like MUPO 30cm had a baseline shift. And then measures until early 2015
#            data after the baseline shoft doesn't match patterns in any other sensors anymore.
SN_30min[sensor=="moisture"&veg=="MUPO"&depth==30&date_time>=as.Date("2013-07-01"),
         mean.val := NA]
# MUPO 20cm: 4 Sep to 23 Oct 2017; April 2018; from October 27 2018 to end of Feb 2018
#            in October 2018 there was a baseline shift! Remove after this.
SN_30min[sensor=="moisture"&veg=="MUPO"&depth==20&
                  (date_time>=as.Date("2017-09-04") & date_time<=as.Date("2017-10-23") |
                  date_time>=as.Date("2018-04-01") & date_time<=as.Date("2018-04-30") |
                  date_time>=as.Date("2018-10-27") & date_time<as.Date("2018-03-01") |
                    date_time>= as.Date("2018-10-23")), mean.val := NA]


# PRGL 5cm: high outlying value in August 2015 >0.4
SN_30min[sensor=="moisture"&veg=="PRGL"&depth==5&year==2015&month==8&mean.val>0.4,
         mean.val := NA]

# PRGL 5cm: probe goes bad after September 2015
SN_30min[sensor=="moisture"&veg=="PRGL"&depth==5&date_time>=as.Date("2015-09-01"),
         mean.val := NA]

# PRGL all: 
# 2018-02-20 to 2018-02-22 remove points < 0.05 from several sensors
SN_30min[sensor=="moisture"&veg=="PRGL"&date_time>=as.Date("2018-02-20")&
           date_time<=as.Date("2018-02-22")&mean.val<0.05,
         mean.val := NA]

# PRGL remove all after January 2019 (sensors got moved on ~19 June)
SN_30min[sensor=="moisture"&veg=="PRGL"&date_time>=as.Date("2019-01-01"),
         mean.val := NA]


# LATR 5cm: probe goes bad after 12 Feb 2017
SN_30min[sensor=="moisture"&veg=="LATR"&depth==5&
           date_time >= as.Date("2017-02-12"), mean.val := NA]


# LATR 10cm: probe is strange 23 Feb to 30 Apr 2015;
#            had a baseline shift 14 Feb 2019 at 23:30. Remove values after this.
SN_30min[sensor=="moisture"&veg=="LATR"&depth==10&
           (date_time>=as.Date("2015-02-23") & date_time<=as.Date("2015-04-30")| 
            (date_time_orig>=as.POSIXct("2019-02-14 23:00:00", tz="UTC"))), mean.val := NA]

# from 2018-05-22 to 2018-05-24 10cm LATR point>0.07
SN_30min[sensor=="moisture"&veg=="LATR"&depth==10&date_time>=as.Date("2018-05-22")&
           date_time<=as.Date("2018-05-24")&mean.val>0.07,
         mean.val := NA]

# LATR 20cm: had baseline shift after 28 Feb 2019 18:00 and does a few steps until reaching new baseline 3rd Apr 2019
#            remove after 3 March 2019. 
SN_30min[sensor=="moisture"&veg=="LATR"&depth==20&
              date_time >= as.POSIXct("2019-02-28 18:00:00", tz="UTC"), mean.val := NA]

# BARE 5cm 2019 June 20-30 remove all
SN_30min[sensor=="moisture"&veg=="BARE"&depth==5&
           (date_time>=as.Date("2019-06-20") & date_time<=as.Date("2019-06-30")),
         mean.val := NA]  

# BARE 10cm: strange 6 Aug to 16 Sep 2015; Jul 24 to Sep 5 2016;
#                    5 Oct 2018 (do just Oct in code) mean.val>0.4;
SN_30min[sensor=="moisture"&veg=="BARE"&depth==10&
           (date_time>=as.Date("2015-08-06") & date_time<=as.Date("2015-09-16") |
              date_time>=as.Date("2016-07-24") & date_time<=as.Date("2016-09-05")),
         mean.val := NA]                  

SN_30min[sensor=="moisture"&veg=="BARE"&depth==10&date_time>=as.Date("2018-10-05")&
           date_time<=as.Date("2018-10-06")&mean.val>0.4,
         mean.val := NA]
# Bare 10cm in 2017 values > 1
SN_30min[sensor=="moisture"&veg=="BARE"&depth==10&year==2017&mean.val>1,
         mean.val := NA]                  

# Bare 30cm 2018-10-09 to 2018-10-11 remove points<0
SN_30min[sensor=="moisture"&veg=="BARE"&depth==30&date_time>=as.Date("2018-10-09")&
           date_time<=as.Date("2018-10-11")&mean.val>0,
         mean.val := NA]                  


# moisture
# remove periods when the sensors are bad. (see above)
# by years
ggplot(SN_30min[sensor=="moisture"&date_time>=as.Date("2018-10-01") & date_time<=as.Date("2018-12-24"),],
       aes(date_time, mean.val, colour=factor(depth)))+
  geom_line(aes(linetype=factor(depth)))+
  labs(title="Soil Moisture") +
  facet_grid(veg~., scales="free_y")

ggplot(SN_30min[sensor%in%c("moisture","rain","lws")&year>=2018,], aes(date_time, mean.val, colour=factor(depth)))+
  geom_point(size=0.1)+
  labs(title="Soil Moisture") +
  facet_grid(sensor+veg~., scales="free_y")+
  theme_bw()

# by veg type
ggplot(SN_30min[sensor=="moisture"&veg=="MUPO",], aes(doy, mean.val, colour=factor(depth)))+
  geom_line(aes(linetype=factor(depth)))+
  labs(title="Soil Moisture") +
  facet_grid(year~., scales="free_y")


############

##### par
# par can't be <0
SN_30min[sensor=="par" & mean.val<0, mean.val := NA]

# SN1: 
# PRGL in SN1 is weird after 15 Sep 2013
# remove 17 April 2015 to 3 June 2015
# remove mean.val>400 in June 2015 (one point remains weird after filter from April - June)
# remove 7 Nov 2015 to 27 Jan 2016
# remove FLCE104 mean.val>400
# remove Oct 2015 mean.val>1000
# remove FLCE Sep 2017 mean.val>200
# 2018: remove FLCE mean.val > 400

SN_30min[sensor=="par"&SN=="SN1"&veg=="PRGL"&date_time>as.Date("2013-09-05"), mean.val := NA]
SN_30min[sensor=="par"&SN=="SN1"&date_time>as.Date("2015-04-16")&date_time<as.Date("2015-06-03"), mean.val := NA ]
SN_30min[sensor=="par"&SN=="SN1"&year==2015&month==6&mean.val>400, mean.val := NA ]
SN_30min[sensor=="par"&SN=="SN1"&date_time>as.Date("2015-11-07")&date_time<as.Date("2016-01-27"), mean.val := NA ]
SN_30min[sensor=="par"&SN=="SN1"&veg=="FLCE104"&year==2016&mean.val>400, mean.val := NA ]
SN_30min[sensor=="par"&SN=="SN1"&year==2015&month==10&mean.val>1000, mean.val := NA]
SN_30min[sensor=="par"&SN=="SN1"&veg=="FLCE"&year==2017&month==9&mean.val>200, mean.val := NA]
SN_30min[sensor=="par"&SN=="SN1"&veg=="FLCE"&year==2018&mean.val>400, mean.val := NA]

# SN2 LATR:
# remove 2019 mean.val>400
SN_30min[sensor=="par"&SN=="SN2"&veg=="LATR"&year==2019&mean.val>400, mean.val := NA]


# SN3 DAPU: 
# remove Aug/Sep 2015 mean.val>1000
# remove April 2017 mean.val>1000

SN_30min[sensor=="par"&SN=="SN3"&veg=="DAPU"&year==2015&month>=8&month<=9&mean.val>1000, mean.val := NA]
SN_30min[sensor=="par"&SN=="SN3"&veg=="DAPU"&year==2017&month==4&mean.val>1000, mean.val := NA]

# SN7 FLCE:
# remove 2020 mean.val>2000
SN_30min[sensor=="par"&SN=="SN7"&veg=="FLCE"&year==2020&mean.val>2000, mean.val := NA]


############

# pressure: can't be <0
SN_30min[sensor=="pressure" & mean.val<0, mean.val := NA]

# precip has to be >0 and 
# The maximum listed intensity in the specifications is 2 to 3 in./hr, which would give an accuracy of +0%, -5%.
# 1inch/hour = max accuracy
# 2-3 inch/hour ~ 50-75mm/hour ~ 25-48mm/30min
# https://s.campbellsci.com/documents/eu/manuals/te525.pdf
SN_30min[sensor=="rain" & (mean.val<0 | mean.val>50), mean.val := NA]

# solar radiation
# remove SN1 
# remove 17 April 2015 to 3 June 2015
# remove 7 Nov 2015 to 27 Jan 2016
SN_30min[sensor=="solar" & mean.val<0, mean.val := NA]

SN_30min[sensor=="solar"&SN=="SN1"&date_time>as.Date("2015-04-16")&date_time<as.Date("2015-06-03"), mean.val := NA ]
SN_30min[sensor=="solar"&SN=="SN1"&date_time>as.Date("2015-11-07")&date_time<as.Date("2016-01-27"), mean.val := NA ]


# look at data

# battery/current: that will tell me which networks are completely out.
ggplot(SN_30min[sensor=="current",], aes(date_time, mean.val, colour=SN))+
  geom_line()+
  labs(title="Current") +
  facet_grid(SN~., scales="free_y")


# lws
# can't be negative or >100
ggplot(SN_30min[sensor=="lws",], aes(date_time, mean.val, colour=SN))+
  geom_line()+
  labs(title="Leaf Wetness") +
facet_grid(veg~., scales="free_y")


# moisture
# remove periods when the sensors are bad. (see above)
# by years
ggplot(SN_30min[sensor=="moisture",], aes(date_time, mean.val, colour=factor(depth)))+
  geom_line(aes(linetype=factor(depth)))+
  labs(title="Soil Moisture") +
  facet_grid(veg~., scales="free_y")
# by veg type
ggplot(SN_30min[sensor=="moisture"&veg=="MUPO",], aes(doy, mean.val, colour=factor(depth)))+
   geom_line(aes(linetype=factor(depth)))+
   labs(title="Soil Moisture") +
   facet_grid(year~., scales="free_y")


# par
# In early and late 2011/2015 the high PAR in all veg types except UP is real. SNOW.
# 2014 all year looks low
ggplot(SN_30min[sensor=="par",], aes(date_time, mean.val, colour=SN))+
  geom_line()+
  labs(title="PAR") +
  facet_grid(veg~., scales="free_y")

# by veg type
ggplot(SN_30min[sensor=="par"&veg=="PRGL"&year==2013&month==9,], aes(date_time, mean.val))+
  geom_line()+
  labs(title="PAR") +
  facet_grid(SN~., scales="free_y")



# pressure
# can't be negative
# looks reasonable!! 
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

ggplot(SN_30min[sensor=="solar" & year==2018,],
       aes(date_time, mean.val, colour=SN))+
  geom_line()+
  labs(title="Solar Radiation") +
  facet_grid(SN~., scales="free_y")


# correct the timestamps
# keep the original timestamp and fix date_time column to make all times MST
# (use tz=UTC to prevent convervsion of data)
SN_long_corrected <- copy(SN_30min)
## rm(SN_30min)
## SN_30min <- copy(SN_long_corrected)

SN_30min[,date_time_orig:=date_time][,date_time:=NULL]

# do nothing
SN_30min <- SN_30min[date_time_orig<as.POSIXct("2013-04-04 23:00:00",tz="UTC"),
                                   date_time := date_time_orig]

# minus 30 mins
SN_30min <- SN_30min[(date_time_orig>=as.POSIXct("2013-04-05 00:00:00",tz="UTC") & 
                                         date_time_orig<=as.POSIXct("2013-12-01 02:00:00",tz="UTC")),
                         date_time := date_time_orig - hours(1)]

# do nothing
SN_30min <- SN_30min[(date_time_orig>=as.POSIXct("2013-12-01 02:30:00",tz="UTC") & 
                                         date_time_orig<as.POSIXct("2015-03-31 23:30:00",tz="UTC")),
                         date_time := date_time_orig]

# minus 1 hour
SN_30min <- SN_30min[(date_time_orig>=as.POSIXct("2015-04-01 00:30:00",tz="UTC") & 
                                         date_time_orig<=as.POSIXct("2015-11-06 02:00:00",tz="UTC")),
                         date_time := date_time_orig - hours(1)]

# do nothing
SN_30min <- SN_30min[(date_time_orig>=as.POSIXct("2015-11-06 02:30:00",tz="UTC") & 
                                         date_time_orig<as.POSIXct("2016-03-14 22:00:00",tz="UTC")),
                     date_time := date_time_orig]

# minus 1 hour
SN_30min <- SN_30min[(date_time_orig>=as.POSIXct("2016-03-14 23:00:00",tz="UTC") & 
                                         date_time_orig<=as.POSIXct("2016-11-08 02:00:00",tz="UTC")),
                         date_time := date_time_orig - hours(1)]

# do nothing
SN_30min <- SN_30min[(date_time_orig>=as.POSIXct("2016-11-08 02:30:00",tz="UTC") & 
                                         date_time_orig<as.POSIXct("2017-03-14 22:00:00",tz="UTC")),
                         date_time := date_time_orig]

# minus 30 mins
SN_30min <- SN_30min[(date_time_orig>=as.POSIXct("2017-03-14 23:00:00",tz="UTC") & 
                                         date_time_orig<=as.POSIXct("2017-11-08 02:00:00",tz="UTC")),
                         date_time := date_time_orig - hours(1)]


# do nothing
SN_30min <- SN_30min[(date_time_orig>=as.POSIXct("2017-11-08 02:30:00",tz="UTC") & 
                                         date_time_orig<as.POSIXct("2018-09-07 9:30:00",tz="UTC")),
                         date_time := date_time_orig]

# minus 1 hour
SN_30min <- SN_30min[(date_time_orig>=as.POSIXct("2018-09-07 10:30:00",tz="UTC") & 
                                         date_time_orig<=as.POSIXct("2018-11-08 02:00:00",tz="UTC")),
                          date_time := date_time_orig - hours(1)]

# do nothing
SN_30min <- SN_30min[(date_time_orig>=as.POSIXct("2018-11-08 02:30:00",tz="UTC") & 
                                         date_time_orig<as.POSIXct("2019-03-11 02:00:00",tz="UTC")),
                          date_time := date_time_orig]

# minus 1 hour
SN_30min <- SN_30min[(date_time_orig>=as.POSIXct("2019-03-11 03:00:00",tz="UTC") & 
                                         date_time_orig<=as.POSIXct("2019-11-03 02:00:00",tz="UTC")),
                          date_time := date_time_orig - hours(1)]

# do nothing
SN_30min <- SN_30min[(date_time_orig>=as.POSIXct("2019-11-03 02:30:00",tz="UTC")),
                                      date_time := date_time_orig]





# import SW potential to compare with adjusted timestamp
sw.pot <- fread("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/Ameriflux/QA_QC_Report_Ameriflux/US-Jo1_HH_2010_2019_SW_IN_pot.csv",
                sep=",",header=TRUE, na.strings=c("-9999"))

sw.pot[,date_time := parse_date_time(TIMESTAMP_END, "YmdHM",tz="UTC")]

sw.pot[,':=' (sensor="sw_pot",
              mean.val=SW_IN_POT,
              date_time_orig=date_time,
              year=year(date_time),
              month=month(date_time),
              doy=yday(date_time))][,SW_IN_POT:=NULL]

# combine 
SN_long_comp <- rbind(SN_30min[!is.na(date_time),], sw.pot, fill=TRUE)


# look at adjusment
# non adjusted, original
daycheck <- as.Date("2017-07-17", tz="UTC")

# original timestamps (uncorrected)
ggplot(SN_long_comp[sensor%in% c("solar", "sw_pot")& (veg=="BARE" | is.na(veg)) & 
                        as.Date(date_time)==daycheck])+
  geom_line(aes(date_time_orig,mean.val, colour=sensor))

# adjusted timesstamp
ggplot(SN_long_comp[sensor%in% c("solar", "sw_pot")& (veg=="BARE" | is.na(veg)) & 
                      as.Date(date_time)==daycheck])+
  geom_line(aes(date_time,mean.val, colour=sensor))

# compar PAR
ggplot(SN_long_comp[sensor%in% c("par", "sw_pot")& (veg=="BARE" | is.na(veg)) & 
                      as.Date(date_time)==daycheck])+
  geom_line(aes(date_time_orig,mean.val, colour=sensor))


ggplot(SN_long_comp[sensor%in% c("par", "sw_pot")& (veg=="BARE" | is.na(veg)) & 
                      as.Date(date_time)==daycheck])+
  geom_line(aes(date_time,mean.val, colour=sensor))

# check the same data on two date columns in SN_30min
ggplot(SN_30min[sensor%in% c("par")& (veg=="BARE" | is.na(veg)) & 
                  as.Date(date_time)==daycheck])+
  geom_line(aes(date_time, mean.val), colour="red")+
  geom_line(aes(date_time_orig, mean.val), colour="black")


# save half hour means
setwd("~/Desktop/TweedieLab/Projects/Jornada/Data/SensorNetwork/Combined")
## write.table(SN_30min, file="SensorNetwork_L1_2010_2019_30min.csv", sep=",", row.names = FALSE)
# fixed 30 min averages to ceiling_date: 2019-06-25
# write.table(SN_30min, file="SensorNetwork_L1_2010_2019_30min_20190625.csv", sep=",", row.names = FALSE)
# added additional data up to July 2019-07-09-1130, need to go back and fix sensor replacement info
# write.table(SN_30min, file="SensorNetwork_L1_2010_201907091130_30min.csv", sep=",", row.names = FALSE)
# save half hour means from 2010-2019 (ends on 11-26 due to system communication outage)
# write.table(SN_30min, file="SensorNetwork_L1_2010_20200210_30min.csv", sep=",", row.names = FALSE)
# 2020-02-13 updated to retain LATR 10cm and 20cm, MUPO 20cm. These probes had baseling shifts but otherwise useful dynamics
# write.table(SN_30min, file="SensorNetwork_L1_2010_20200213_30min.csv", sep=",", row.names = FALSE)

# 8 Apr 2020 update: 
# adjusted timestamps!!!! 
# save in long format. CHANGE NAME TO L2!!!!
# save SN_long_comp to try and solve the timestamp reverting issue.
# write.table(SN_long_comp[!(sensor %in% c("sw.pot")),], file="SensorNetwork_L2_2010_20200324_30min.csv", sep=",", dec='.', row.names = FALSE)

# 2020-12-26 updated to remove LATR 10cm and 20cm, MUPO 20cm after 2018/2019 baseline shifts.
# write.table(SN_long_comp[!(sensor %in% c("sw.pot")),], file="SensorNetwork_L2_2010_20201226_30min.csv", sep=",", dec='.', row.names = FALSE)


# AND save in wide format
# use SN_long_comp
# save by year
SN_long_comp[!is.na(veg)&!is.na(depth),sensorID:= paste(SN,sensor,veg,depth,sep="_")]
SN_long_comp[!is.na(veg)&is.na(depth),sensorID:= paste(SN,sensor,veg,sep="_")]
SN_long_comp[is.na(veg)&is.na(depth),sensorID:= paste(SN,sensor,sep="_")]


SN30_wide_save <- data.table:: dcast(SN_long_comp[!is.na(date_time) & !(sensor %in% c("sw_pot")),
                                              .(date_time, date_time_orig, sensorID,mean.val)],
                                      date_time+date_time_orig~sensorID,
                                      value.var="mean.val")

setnames(SN30_wide_save,c("date_time","date_time_orig"),
         c("timestamp","timestamp_orig"))

ggplot(SN30_wide_save, aes(x=timestamp))+
  geom_line(aes(y=SN7_moisture_MUPO_5)) # with NA

# save by year! (re-run 2020-12-26)
setwd("~/Desktop/TweedieLab/Projects/Jornada/Data/SensorNetwork/Yearly_QAQC_timestamp")

saveyears <- function(data,startyear,endyear) {
  data[is.na(data)] <- NA
  
  for (i in startyear:endyear) {
    data_save <- data[year(timestamp)==i,]
    
    write.table (data_save,
                 file=paste("dataL2_SensorNetwork",i, ".csv",sep="_"),
                 sep =',', dec='.', row.names=FALSE,quote=FALSE)
  }}


# saveyears(SN30_wide_save,2010,2020)



# FOR ANTHONY: save December 2018 to March 2019 soil moisture nad precip for Bare
setwd("~/Desktop/TweedieLab/Projects/Jornada/Anthony_soilCO2_fluxes")
 # write.table(SN_30min[date_time>=as.Date("2018-12-01") &date_time <= as.Date("2019-03-31") &
 # (sensor=="moisture" | (sensor=="rain"&veg=="BARE")),],
 # file="SEL_JER_SensorNetwork_30min_VWC_precip_20181201_20190331_20190508.csv", sep=",", row.names = FALSE)

# cast the SN_20190401000000 file into wide format and save that way as well
SN_20190401000000.wide <- dcast(SN_20190401000000[,.(Date,variable,value)],
                   Date ~ variable, fun.aggregate = mean, value.var = "value")

# setwd("~/Desktop/TweedieLab/Projects/Jornada/Data/SensorNetwork")
#  write.table(SN_20190401000000.wide, file="SensorNetwork_20190401000000_20190709113000_5min .csv",
# sep=",", row.names = FALSE)


