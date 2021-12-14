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

# This code will:
# 1. allow data to be checked
# 2. run standard range filters determined from 2010-2019 and input from Ameriflux
# 3. allow year-specific data removal based on visual checks
# 4. save data with date/time in file name of ECTM/year/QAQC folder on server
# 5. save a full year of data to Flux/Combined with only year in filename
# 6. save a csv file of this code as Data_QAQC_Code_yyyy.csv (or with date/time) to ECTM/year/QAQC folder to record data filter steps
# 7. ECTM data ends ~May 2021 and transition to CS650 


# load libraries
library(ggplot2) # library for making figures in ggplot package
library(lubridate) # library for easier date manipulation 
library(data.table) # library for data table which is more efficient with large data sets

year_file <- 2021

# import most recent file

ectm_data <- fread(paste("/Volumes/SEL_Data_Archive/Research Data/Desert/Jornada/Bahada/Tower/SoilSensor_ECTM/",year_file,"/Raw_Data/ASCII/dataL1_ECTM_",year_file,".csv",sep=""),
              header = TRUE, sep=",", skip = 3,fill=TRUE,
                                    na.strings=c(-9999,"#NAME?"),
                                     col.names=c("timestamp","record",
                                                 "vwc_1","vwc_2","vwc_3","vwc_4","vwc_5","vwc_6","vwc_7","vwc_8",
                                                 "ecp_1","ecp_2","ecp_3","ecp_4","ecp_5","ecp_6","ecp_7","ecp_8",
                                                 "t_1","t_2","t_3","t_4","t_5","t_6","t_7","t_8"))

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

# colnames(ectm_30min_rmna) <- paste(colnames(ectm_30min_rmna), 'mean_na', sep="_")

ectm_30min_rmna[,date_time := (ymd_hms(ceiling_date))][,ceiling_date := NULL]


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

# make a figure of temperature
ggplot(ectm_30min_long[measurement=="t",],
       aes(date_time,value,colour=variable))+
  #geom_point()+
  geom_line()+
  geom_hline(yintercept=c(0,40))+
  facet_grid(variable~.)


# make a figure of VWC 
# look at summary of vwc data when there are no NAs
ggplot(ectm_30min_long[measurement=="vwc" ,],
       aes(date_time,value,colour=variable))+
  geom_point()+
  geom_hline(yintercept=0)+
  facet_grid(variable~.)


# 2020 onward
# remove all except 2 in temp and moisture
ectm_30min_long[rep %in% c(1,3,4,5,6,7,8) & year>=2020, value := NA]

# look at data in figures and remove points that are obviously bad. 

# make a figure of temperature
ggplot(ectm_30min_long[measurement=="t",],
       aes(date_time,value,colour=variable))+
  #geom_point()+
  geom_line()+
  geom_hline(yintercept=c(0,40))+
  facet_grid(variable~.)

# make a figure of VWC 
# look at summary of vwc data when there are no NAs
ggplot(ectm_30min_long[measurement=="vwc" ,],
       aes(date_time,value,colour=variable))+
  geom_point()+
  geom_hline(yintercept=0)+
  facet_grid(variable~.)

# # AND save in wide format
# save by year
ectm_30min_long[,id := paste(measurement,rep,sep="_")]

ectm_wide_save <- data.table:: dcast(ectm_30min_long[!is.na(date_time),
                                               .(date_time, variable,value)],
                                     date_time~variable,
                                     value.var="value")

# quick graph just to make sure nothing silly happened
ggplot(ectm_wide_save, aes(x=date_time))+
  geom_line(aes(y=vwc_2)) # with NA

# save to QAQC folder on data archive
startdate <- (min(ectm_wide_save$date_time))
enddate <- (max(ectm_wide_save$date_time))

# # save in QAQC folder with start and end date in the file name
qaqc.path<- paste("/Volumes/SEL_Data_Archive/Research Data/Desert/Jornada/Bahada/Tower/SoilSensor_ECTM/",year_file,"/QAQC/", sep="")
setwd(qaqc.path)

###################### with start and end date in the file name ##################
# write.table(ectm_wide_save,
#             paste("dataL2_ECTM_",year(startdate),sprintf("%02d",(month(startdate))),sprintf("%02d",(day(startdate))),
#                   sprintf("%02d",(hour(startdate))),sprintf("%02d",(minute(startdate))),
#                   sprintf("%02d",(second(startdate))),
#                   "_",
#                   year(enddate),sprintf("%02d",(month(enddate))),sprintf("%02d",(day(enddate))),
#                   sprintf("%02d",(hour(enddate))),sprintf("%02d",(minute(enddate))),
#                   sprintf("%02d",(second(enddate))), ".csv",sep=""),
#             sep=",", dec=".", row.names=FALSE)
########################################################################################

# save to QAQC folder
write.table(ectm_wide_save,
            paste("dataL2_ECTM_",year_file, ".csv",sep=""),
            sep=",", dec=".", row.names=FALSE)

# save a text file that says date that code was run (system time), start and end date of data
run.info <- data.frame(info=c("Data_start","Data_end","Date_processed"),
                       date_time=c(startdate,enddate,ymd_hms(Sys.time(),tz="UTC")))

write.table(run.info, "dataL2_ECTM_DateRange.csv",
            sep=",", dec=".", row.names=FALSE)


# save to Combined folder with only year name
# difftime(startdate,enddate)

setwd("/Volumes/SEL_Data_Archive/Research Data/Desert/Jornada/Bahada/Tower/SoilSensor_ECTM/Combined")

write.table(ectm_wide_save,
            paste("dataL2_ECTM_",year_file, ".csv",sep=""),
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





