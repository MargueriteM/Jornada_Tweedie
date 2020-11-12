
##########################################
#   Jornada Sensor Network Data          #
#      code: M. Mauritz, 21 Nov 2018     #
# modified April 2019 to add 2010-2019   #
##########################################

# Jornada Sensor Network Data
# did manual transfer of files from delivery to archive. Need to update 2019 data, and 2020. 
# use this code to compile files
# On 2019-06-20 (~8am-10am) we removed some non-working sensors and moved sensors to create a complete soil moisture
# profile for bare ground and PRGL
# update column names to reflect move (colnames2019)

library(data.table)
library(lubridate)

# create directories for reading and moving files
# directory where new data comes in
data_dir <- "/Volumes/SEL_Data_Archive/Research Data/Desert/Jornada/Bahada/SensorNetwork/2020/RawData"

# directory where the final processed file goes
save_dir_final <- "/Volumes/SEL_Data_Archive/Research Data/Desert/Jornada/Bahada/SensorNetwork/2020"


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

# Get the column names of each sensor network data set and
# compare to the columns that should be in the data

################### Fix Column Format Function ######################
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

# fread imports as data table
# list all csv files in relevant folder
SNfiles_test <- list.files(path=data_dir,
                           full.names=TRUE, pattern="^all_nodes_hourly") 


# read files and bind them into one file.
SN_test <- do.call("rbind", c(fill=TRUE,lapply(SNfiles_test, header = TRUE, fread, sep=",",
                                               na.strings=c(-9999,-888.88,"#NAME?"))))



# column names for 2019 onward 
## On June 20 2019 between ~ 8am - 10am we did some sensor removal and moved 10cm PRGL to 5cm Bare
# after this the variable name for 5cm Bare changed.
# colnames2019 contains all columns from colnames2015 plus the new 5cm Bare variable
colnames2019 <- fread(file="/Volumes/SEL_Data_Archive/Research Data/Desert/Jornada/Bahada/SensorNetwork/MetaData/JER_SensorNetwork_ColumnNames_2019.csv",
                      sep=",",
                      header=TRUE) 

colnames2019[, ':=' (sensor = sapply(strsplit(as.character(R_column_names),"_"),"[",1),
                     SN = sapply(strsplit(as.character(R_column_names),"_"),"[",2),
                     veg = sapply(strsplit(as.character(R_column_names),"_"),"[",3),
                     depth = sapply(strsplit(as.character(R_column_names),"_"),"[",4))]




# fix character and logical columns in each data set so that they can be melted
# melt.data.table will give an error. This shouldn't be anything to worry about. 
# check that output makes sense by looking at str() and seeing if everything is num or int
# Date should still be POSIX
# and check the column names
SN_test[, date_time := ymd_hms(Date)][, Date:=date_time][, date_time:=NULL]
fix_test <- fix_columns(SN_test,colnames2019)
SN_test_fx <- fix_test[[1]]
colcheck_test <- fix_test[[2]]

# change the data into long format
SN_test_long <- melt.data.table(SN_test_fx,c("Date"))

# merge the column names to the data and split into additional descriptors
# descriptors are: sensor, SN, veg, depth
SN_test_long <- merge(SN_test_long, colnames2019, by="variable")


## save the 5 min data for later use. save the data in wide format, use the date info from the long file
## save with start and end date of merged data files
setwd(save_dir_final)
write.table(SN_test,file=paste(paste('SensorNetwork',
                                     sprintf("%04d%02d%02d%02d%02d%02d", year(min(SN_test_long$Date)),month(min(SN_test_long$Date)),
                                             day(min(SN_test_long$Date)),
                                             hour(min(SN_test_long$Date)),
                                             minute(min(SN_test_long$Date)),
                                             second(min(SN_test_long$Date))),
                                     sprintf("%04d%02d%02d%02d%02d%02d",year(max(SN_test_long$Date)),month(max(SN_test_long$Date)),day(max(SN_test_long$Date)),
                                             hour(max(SN_test_long$Date)),minute(max(SN_test_long$Date)),second(max(SN_test_long$Date))),
                                     "5min",sep="_"),".csv"),
            sep=",",dec=".",row.names=FALSE)


## save the 5 min data for later use in long format
## save with start and end date of merged data files
setwd(save_dir_final)
write.table(SN_test_long,file=paste(paste('SensorNetwork',
   sprintf("%04d%02d%02d%02d%02d%02d", year(min(SN_test_long$Date)),month(min(SN_test_long$Date)),
          day(min(SN_test_long$Date)),
          hour(min(SN_test_long$Date)),
          minute(min(SN_test_long$Date)),
          second(min(SN_test_long$Date))),
   sprintf("%04d%02d%02d%02d%02d%02d",year(max(SN_test_long$Date)),month(max(SN_test_long$Date)),day(max(SN_test_long$Date)),
          hour(max(SN_test_long$Date)),minute(max(SN_test_long$Date)),second(max(SN_test_long$Date))),
   "5min_long",sep="_"),".csv"),
            sep=",",dec=".",row.names=FALSE)

# Take a look at some of the data to make sure the name change on 2019-06-20 worked correctly

# ggplot(SN_test_long[sensor=="moisture" & month(Date)>=6 & month(Date)<=7,],
#        aes(Date,value,colour=depth))+geom_point(size=0.25)+facet_grid(veg~., scales = "free_y")
# 
# ggplot(SN_test_long[sensor=="moisture" & Date>=as.Date("2019-06-20")&Date<=as.Date("2019-06-21"),],
#         aes(Date,value,colour=depth))+geom_point()+facet_grid(veg~., scales = "free_y")
# 
# ggplot(SN_test_long[sensor=="moisture",],
#         aes(Date,value,colour=depth))+geom_point(size=0.25)+facet_grid(veg~., scales = "free_y")
# 
# ggplot(SN_test_long[sensor=="rain" & month(Date)>=6 & month(Date)<=7,],
#       aes(Date,value))+geom_point(size=0.25)+facet_grid(veg~., scales = "free_y")
# 
# 
# ggplot(SN_test_long[sensor=="par" & month(Date)>=6 & month(Date)<=7,],
#        aes(Date,value, colour=SN))+geom_point(size=0.25)+facet_grid(veg~., scales = "free_y")

