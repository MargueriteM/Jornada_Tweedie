



##########################################
#   Jornada Sensor Network Data 2015     #
#      code: M. Mauritz, 21 Nov 2018     #
# modified April 2019 to add 2010-2019   #
##########################################

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

#########################################
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

setwd("Volumes/Users/jerTweWSN/Documents/delivery")

# fread imports as data table
# list all csv files in relevant folder
SNfiles_test <- list.files(path="/Volumes/SEL_Data_Archive/Research Data/Desert/Jornada/UtepJrnBahadaSite/SensorNetwork/2019/RawData",
                           full.names=TRUE) 

# column names for 2015 onward 
# after 2015 there are a few erroneous sensor columns that creep in. 
# these are either emty or contain some brief data, but we're not sure what that data is
# and it often doesn't look like good data
# colnames.

# 2015 is a complete list of all columns that SHOULD be in the data. 
# use that to screen and remove extra columns
colnames2015 <- fread(file="~/Desktop/TweedieLab/Projects/Jornada/Data/SensorNetwork/MetaData/JER_SensorNetwork_ColumnNames_2015.csv",
                      sep=",",
                      header=TRUE) 

colnames2015[, ':=' (sensor = sapply(strsplit(as.character(R_column_names),"_"),"[",1),
                     SN = sapply(strsplit(as.character(R_column_names),"_"),"[",2),
                     veg = sapply(strsplit(as.character(R_column_names),"_"),"[",3),
                     depth = sapply(strsplit(as.character(R_column_names),"_"),"[",4))]


# read files and bind them into one file.
SN_test <- do.call("rbind", c(fill=TRUE,lapply(SNfiles_test, header = TRUE, fread, sep=",",
                                        na.strings=c(-9999,-888.88,"#NAME?"))))


# fix character and logical columns in each data set so that they can be melted
# melt.data.table will give an error. This shouldn't be anything to worry about. 
# check that output makes sense by looking at str() and seeing if everything is num or int
# Date should still be POSIX
# and check the column names
SN_test[, date_time := ymd_hms(Date)][, Date:=date_time][, date_time:=NULL]
fix_test <- fix_columns(SN_test,colnames2015)
SN_test_fx <- fix_test[[1]]
colcheck_test <- fix_test[[2]]

# change the data into long format
SN_test_long <- melt.data.table(SN_test_fx,c("Date"))

# merge the column names to the data and split into additional descriptors
# descriptors are: sensor, SN, veg, depth
SN_test_long <- merge(SN_test_long, colnames2015, by="variable")

# save the 5 min data for later use
# save with start and end date of merged data files
setwd("/Volumes/SEL_Data_Archive/Research Data/Desert/Jornada/UtepJrnBahadaSite/SensorNetwork/2019")
# write.table(SN_test_long,file=paste(paste('SensorNetwork',
#    sprintf("%04d%02d%02d%02d%02d%02d", year(min(SN_test_long$Date)),month(min(SN_test_long$Date)),
#           day(min(SN_test_long$Date)),
#           hour(min(SN_test_long$Date)),
#           minute(min(SN_test_long$Date)),
#           second(min(SN_test_long$Date))),
#    sprintf("%04d%02d%02d%02d%02d%02d",year(max(SN_test_long$Date)),month(max(SN_test_long$Date)),day(max(SN_test_long$Date)),
#           hour(max(SN_test_long$Date)),minute(max(SN_test_long$Date)),second(max(SN_test_long$Date))),
#    "5min",sep="_"),".csv"),
#             sep=",",dec=".",row.names=FALSE)

# to visualise
# calculate 30min data
SN_30min_test <- SN_test_long[sensor!="rain", list(mean.val = mean(value, na.rm=TRUE)),
                         by=.(SN,veg,depth,sensor,cut(Date,"30 min"))][,date_time := (ymd_hms(cut))][,cut := NULL]

# for precip calculate the sum
SN_30min_rain_test <- SN_test_long[sensor=="rain", list(mean.val = sum(value, na.rm=TRUE)),
                              by=.(SN,veg,depth,sensor,cut(Date,"30 min"))][,date_time := (ymd_hms(cut))][,cut := NULL]


SN_30min_test <- rbind(SN_30min_test, SN_30min_rain_test)

# break up the date column
SN_30min_test[,year:= year(date_time)]
SN_30min_test[,month:= month(date_time)]
SN_30min_test[,doy:= yday(date_time)]

# make sample plots
ggplot(SN_30min_test, aes(date_time, mean.val, colour=paste(SN,veg,depth,sep=".")))+
  geom_line()+
  facet_grid(sensor~., scales="free_y")

ggplot(SN_30min_test, aes(date_time, mean.val, colour=paste(SN,veg,depth,sep=".")))+
  geom_line()+
  facet_grid(sensor~SN, scales="free_y")


