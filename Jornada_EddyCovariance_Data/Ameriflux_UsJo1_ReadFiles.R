
############################################
# Read US-Jo1 data: Sample Code            #
# written by: M. Mauritz                   #
# for Lindsey and Talveer                  #
############################################

# load libraries
library(data.table)
library(lubridate)
library(ggplot2)
library(bit64)

# set working directory to mapped one drive folder
setwd("~/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Tower Data/JER_Bajada/EddyCovarianceTower/Ameriflux")

data2010 <- fread("USJo1_HH_201001010000_201012312330_20200218submit.csv",na.strings="-9999")

data2011 <- fread("USJo1_HH_201101060000_201112312330_20200218submit.csv",na.strings="-9999")

# combine multiple years of data
data.all <- rbind(data2010, data2011, fill=TRUE)

# if TIMESTAMP_START and TIMESTAMP_END import as integer64, convert to character
data.all <- data.all[,':=' (TIMESTAMP_START = as.character(TIMESTAMP_START),
                            TIMESTAMP_END = as.character(TIMESTAMP_END))]

# convert to timestamp object, use tz=UTC to prevent timezone conversions
data.all <- data.all[,date_time:=ymd_hm(TIMESTAMP_END,tz="UTC")]

# create a year, month, and day of year column
data.all <- data.all[,':=' (Year = year(date_time),
                            Month = month(date_time),
                            DOY = yday(date_time))]
# view column names
colnames(data.all)

# graph columns by timestamp
ggplot(data.all, aes(date_time, SWC_1_1_1))+
  geom_point()

# graph columns by day of year and separate years in rows
ggplot(data.all, aes(date_time, SWC_1_1_1))+
  geom_point()+
  facet_grid(Year~., scales="free_x")

# graph columns by day of year and separate years in columns
ggplot(data.all, aes(DOY, SWC_1_1_1))+
  geom_point()+
  facet_grid(.~Year, scales="free_x")


