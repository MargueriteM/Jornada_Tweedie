#################################################
#    This code processes ECTM Data from JER     #
#                                               #
# Data come from:                               #
#                * ECTM data table              #
#                * Soil Moisture & Temp         #
#  Get data from SEL Drive, view time series,   #
# identify sensor gaps                          #
#                                               #
#    written by: Marguerite Mauritz             #
#                15 April, 2019                 #
#################################################

# load libraries
library(ggplot2) # library for making figures in ggplot package
library(lubridate) # library for easier date manipulation 
library(data.table) # library for data table which is more efficient with large data sets

# make sure all files to merge are in one folder. 

# set working directory where files are kept
setwd("~/Desktop/TweedieLab/Projects/Jornada/SoilSensor_ECTM")

# read all the files in the folder for merging
ectm_files <- list.files(path="~/Desktop/TweedieLab/Projects/Jornada/Data/SoilSensor_ECTM", full.names=TRUE, pattern=".csv") 
# import data, skip first 3 rows, define column names, tell R that -9999 is NA
ectm_data <- do.call("rbind", lapply(ectm_files,
                                     header = TRUE, fread, sep=",", skip = 3,fill=TRUE,
                                    na.strings=c(-9999,"#NAME?"),
                                     col.names=c("timestamp","record",
                                                 "vwc_1","vwc_2","vwc_3","vwc_4","vwc_5","vwc_6","vwc_7","vwc_8",
                                                 "ecp_1","ecp_2","ecp_3","ecp_4","ecp_5","ecp_6","ecp_7","ecp_8",
                                                 "t_1","t_2","t_3","t_4","t_5","t_6","t_7","t_8")))

# check to see if it's a data table
# is.data.table(ectm_data)

# select only temperature and vwc columns to keep
ectm_data [,':=' (ecp_1=NULL, ecp_2=NULL, ecp_3=NULL, ecp_4=NULL,
                  ecp_5=NULL, ecp_6=NULL, ecp_7=NULL, ecp_8=NULL)]

# delete the record column
ectm_data[,record:=NULL]

# create a date/time, date
ectm_data[,date_time:=parse_date_time(timestamp, c("%Y!-%m-%d %H:%M:%S",
                                                                  "%m-%d-%y %H:%M"))][,timestamp:=NULL]

# check that the files merged correctly and I think I have all the data there
# summary(ectm_data)

# calculate half-hourly means using ceiling_date
ectm_30min_rmna <- ectm_data[,lapply(.SD, function (x) {mean(x, na.rm=TRUE)}),
                          by=ceiling_date(date_time,"30 min")]

colnames(ectm_30min_rmna) <- paste(colnames(ectm_30min_rmna), 'mean_na', sep="_")

ectm_30min_rmna[,date_time := (ymd_hms(ceiling_date_mean_na))][,ceiling_date_mean_na := NULL]


# change the format of the data from wide to long so it's easier to work with
ectm_30min_long <- melt(ectm_30min_rmna,c("date_time"))


# make a column called measurement which will tell us what is being measured: vwc or temp
# make a column called rep which will tell us what rep number a sensor is, make it an integer
ectm_30min_long[,':=' (measurement = sapply(strsplit(as.character(variable),"_"),"[",1),
                 rep = as.integer(sapply(strsplit(as.character(variable),"_"),"[",2)),
                 year=year(date_time),
                 month=month(date_time),
                 date=as_date(date_time),
                 doy=yday(date_time))]


# create a column that calculates the difference between consectuve measurements to filter
# first make sure that the data is correctly ordered
setorder(ectm_30min_long, variable,date_time)
ectm_30min_long[, diff := value - shift(value), by=variable] 
ectm_30min_long[, diff2 := diff - shift(diff), by=variable] 

# create back-up data to revert to: 
# ectm_30min_long1 <- copy(ectm_30min_long)
# ectm_30min_long <- copy(ectm_30min_long1)

# Data to REMOVE due to probe failure:
# 2010:
# probe 1 remove all except August
ectm_30min_long[rep==1 & year==2010 & month!=8, value := NA]
# probe 2 remove August
ectm_30min_long[rep==2 & year==2010 & month==8, value := NA]

# 2011:
# remove probe 1
ectm_30min_long[rep==1 & year==2011, value := NA]

# 2012:
# remove probe 1
ectm_30min_long[rep==1 & year==2012, value := NA]

# probe 4 remove Aug 15 >12:00 & <15:00
ectm_30min_long[rep==4 & date_time>=as.POSIXct("2012-08-15 12:30",tz="UTC") &
                  date_time<=as.POSIXct("2012-08-15 14:30",tz="UTC"), value := NA]

# 2013:
# remove probe 8 March vwc value >0.2
ectm_30min_long[variable=="vwc_8_mean_na" & year==2013 & month ==3 & value>0.2, value := NA]

# 2014:
# probe 3 remove >18 Aug & < 1 Sep
ectm_30min_long[rep==3 & date_time>=as.Date("2014-08-18") & 
                  date_time<=as.Date("2014-09-01"), value := NA]

# probe 4 remove vwc value< -0.5
ectm_30min_long[variable=="vwc_4_mean_na"&year==2014&value<(-0.5), value := NA]

# 2015:
# probe 5 remove => Apr 5 12:00 and =< Apr 7 15:30
# probe 5 remove March 20 - 29
ectm_30min_long[rep==5 & (date_time>=as.POSIXct("2015-03-20 0:30",tz="UTC") &
                            date_time<=as.POSIXct("2015-03-28 23:30",tz="UTC") | 
                            date_time>=as.POSIXct("2015-04-05 12:00",tz="UTC") &
                  date_time<=as.POSIXct("2015-04-07 15:30",tz="UTC")), value := NA]

# probe 1, 4, 8 remove all: not working
ectm_30min_long[rep %in% c(1,4,8) & year==2015, value := NA]

# 2016:
# probe 4,8 remove all: not working
ectm_30min_long[rep %in% c(4,8) & year==2016, value := NA]

# 2017: 
# probe 4, 8 remove all: not working
ectm_30min_long[rep %in% c(4,8) & year==2017, value := NA]

# probe 2 vwc remove value >0.2 in August and September
ectm_30min_long[variable=="vwc_2_mean_na"&year==2017&month %in% c(8,9) &value>0.2, value := NA]

# probe 3 vwc remove value > 0.2 in September
ectm_30min_long[variable=="vwc_3_mean_na"&year==2017&month==9&value>0.2, value := NA]

# 2018:
# probe 1, 4, 8 remove all: not working
ectm_30min_long[rep %in% c(1,4,8) & year==2018, value := NA]

# probe 7 remove >July
ectm_30min_long[rep==7 & year==2018 & month>7, value := NA]

# probe 2, 3 filter by diff2 between -0.25 and 0.25,
# and values < 10 between June and October
# find the date indices so that the same points can be removed in t and vwc
diff_ind_2018 <- ectm_30min_long[rep %in% c(2,3) & measurement=="t" & year==2018 & 
                                  ((diff2<(-0.25) | diff2>0.25)), date_time]

ectm_30min_long[rep %in% c(2,3) & date_time %in% diff_ind_2018, value := NA ]
# do an additional filter for some vwc measurements with diff2 between -0.1 and 0.1
ectm_30min_long[rep %in% c(2,3) & year==2018 & measurement=="vwc" &
                  ((diff2<(-0.1) | diff2>0.1)), value := NA ]

# additional filter for probe 2 vwc>0.2 and vwc< -0.1 in June
ectm_30min_long[variable=="vwc_2_mean_na" & year==2018 & month==6 & 
                  ((value<(-0.1) | value>0.2)), value := NA ]
# probe 3 vwc>0.3 in July 
ectm_30min_long[variable=="vwc_3_mean_na" & year==2018 & month==7 & 
                  (value>0.3), value := NA ]

# probe 5 vwc<0.1 on October 14 and when vwc< -0.05
ectm_30min_long[variable=="vwc_5_mean_na" & ((date_time>as.Date("2018-10-13") &
                  date_time < as.Date("2018-10-15") & value < 0.1) |
                    value< (-0.05)), value := NA]

# probe 2 remove > 17 Sep and < 9 Oct
ectm_30min_long[rep==2 & date_time>as.Date("2018-09-17") & date_time < as.Date("2018-10-09"), value := NA]
# probe 3 remove November (has only 1 data point)
ectm_30min_long[rep==3 & year==2018 & month==11, value:=NA]

# 2019:
# probe 1 remove after 2019
ectm_30min_long[rep %in% c(1) & year==2019, value := NA]

# probe 2 remove diff2 between -0.25 and 0.25 and temps < -5 (because diff2 misses a few)
ectm_30min_long[rep %in% c(2) & year==2019 & 
                  ((diff2<(-0.25) | diff2>0.25) | value < (-05)), value := NA]

# probe 2 remove diff2 between -0.25 and 0.25 and temps < 10 after April (because diff2 misses a few)
ectm_30min_long[rep %in% c(2) & year==2019 & month>3 & value <= (10), value := NA]


# probe 3 remove after 2019
ectm_30min_long[rep %in% c(3) & year==2019, value := NA]

# probe 4 remove after 2019
ectm_30min_long[rep %in% c(4) & year==2019, value := NA]


# probe 5, 6 remove diff2 between -0.25 and 0.25 and temps < -5 (because diff2 misses a few)
ectm_30min_long[rep %in% c(5,6) & year==2019 & 
                  ((diff2<(-0.25) | diff2>0.25) | value < (-05)), value := NA]

# probe 5 temp gets weird after end of Mar Remove after
ectm_30min_long[variable=="t_5_mean_na" & year==2019 & month>=3, value:=NA]

# probe 5 remove after Mar 2019
ectm_30min_long[rep %in% c(5) & date_time>as.Date("2019-03-31") & date_time<as.Date("2020-01-13"), value := NA]


# probe 5 vwc gets weird after end of Jan. Remove feb and onward
ectm_30min_long[variable=="vwc_5_mean_na" & date_time>as.Date("2019-01-31") & date_time<as.Date("2020-01-13"), value:=NA]

# probe 6 remove <5 after March
ectm_30min_long[rep %in% c(6) & year==2019 & month>=3 & value < (5), value := NA]
# probe 6 remove after May 2019
ectm_30min_long[rep %in% c(6) & date_time>as.Date("2019-04-30")& date_time<as.Date("2020-01-13"), value := NA]

# probe 7, 8 remove all: not working
ectm_30min_long[rep %in% c(7,8) & year==2019, value := NA]


# remove vwc > 0.2 for all before April in 2019
ectm_30min_long[measurement == "vwc" & year==2019 & month<=4 & value>0.2, value:=NA]

# 2020
# remove 4, 7, 8 in moisture
ectm_30min_long[measurement == "vwc"& rep %in% c(4,7,8) & year==2020, value := NA]

# look at data in figures and remoce points that are obviously bad. 

# make a figure of temperature
ggplot(ectm_30min_long[measurement=="t"&year>=2019,],
       aes(date_time,value,colour=variable))+
  #geom_point()+
  geom_line()+
  geom_hline(yintercept=c(0,40))+
  facet_grid(variable~.)


# make a figure of VWC 
# look at summary of vwc data when there are no NAs
ggplot(ectm_30min_long[measurement=="vwc" & year>=2019,],
       aes(date_time,value,colour=variable))+
  geom_point()+
  geom_hline(yintercept=0)+
  facet_grid(variable~.)


# FOR ANTHONY: 
# look only at probe 5 and 6
# I feel fairly confident that 5 = 10cm open and 6 = 5cm shrub
# data looks good from 2018-12-01 to 2019-03-18, then each day at noon the probes seem to crash
# can gap-fill with air temperature relationships from the prior months. 
ggplot(ectm_30min_long[measurement=="t" & 
                         date_time >= as.Date("2018-12-01") & date_time <= as.Date("2019-03-31") &
                         rep %in% c(5,6),],
       aes(date_time,value,colour=variable))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept=c(0,40))+
  facet_grid(variable~.)

# save Dec 2018 to end of March 2019 data for Anthony
setwd("~/Desktop/TweedieLab/Projects/Jornada/Anthony_soilCO2_fluxes")
#  write.csv(ectm_30min_long[measurement=="t" & 
# date_time >= as.Date("2018-12-01") & date_time <= as.Date("2019-03-31") &
#   rep %in% c(5,6),], file='SEL_JER_SoilTemperature_5_10_NoFill_20181201_20190331_20190423.csv',
#           row.names=FALSE)


# save half hour means
setwd("~/Desktop/TweedieLab/Projects/Jornada/Data/SoilSensor_ECTM/Combined")
## write.table(ectm_30min_long, file="Soil_Temp_VWC_ECTM_L1_2010_2019_30min.csv", sep=",", row.names = FALSE)
# save with ceiling_date
# write.table(ectm_30min_long, file="Soil_Temp_VWC_ECTM_L1_2010_2019_30min_20190627.csv", sep=",", row.names = FALSE)

# save updated to 31 May 2019
# write.table(ectm_30min_long, file="Soil_Temp_VWC_ECTM_L1_2010_20190531_30min.csv", sep=",", row.names = FALSE)

# save updated to 12 Jan 2020
# write.table(ectm_30min_long, file="Soil_Temp_VWC_ECTM_L1_2010_20200112_30min.csv", sep=",", row.names = FALSE)


