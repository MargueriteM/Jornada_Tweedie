
# look at 2010 full output to see if I can figure out any issues
# Jason Hupp suggested: check flag criteria like skewness, kurtosis

# 2020-02-03 & 04:
# PROBLEM: Many of the 2010 files had the Record column missing resulting in wrong column allocation (these are 14 columns long instead of 15)
# SOLUTION: Count columns and move files to a 14 and 15 length folder and change EddyPro to read different columns for the 15 length files


library(ggplot2) # library for making figures in ggplot package
library(lubridate) # library for easier date manipulation 
library(data.table) # library for data table which is more efficient with large data sets
library(reader)
library(tidyr)
library(lsr) # contains quantileCut function
library(gridExtra)
library(viridis)
#############
# IMPORT DATA
#############
# get header and unit info from the first rows of data files
# file info
fileinfo <- scan("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_2010/eddypro_JER_2010_full_output_2019-08-08T131210_adv.csv",
                 what='',sep=",",nlines=1)
fileinfo <- data.table(t(fileinfo))
# read only the first row to get the units
flux.units <- (fread("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_2010/eddypro_JER_2010_full_output_2019-08-08T131210_adv.csv",header=TRUE,skip=1))[1,]

# read the data, skippping the units row
flux1 <- fread("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_2010/eddypro_JER_2010_full_output_2019-08-08T131210_adv.csv", sep=",",skip=3,
               header=FALSE, na.strings=c("-9999","-9999.0","NAN","#NAME?"),col.names=colnames(flux.units))

flux1[,date_time := paste(date,time,sep=" ")]
flux1[,date_time := ymd_hm(date_time)]

flux1[,':=' (date=ymd(date), time=hm(time))][,date_time := as.POSIXct(paste(date,time,sep=" "))]

ggplot(flux1[DOY>=1 & DOY<=90], aes(DOY, co2_flux))+
  geom_line()+
  ylim(c(-5,5))

ggplot(flux1, aes(date_time, co2_flux))+
  geom_line()+
  ylim(c(-5,5))

ggplot(flux1, aes(DOY, skewness_kurtosis_hf))+
  geom_line()

ggplot(flux1[!is.na(co2_flux),], aes(DOY, skewness_kurtosis_hf, colour=factor(qc_co2_flux)))+
  geom_point()

flux1[,co2_flux2 := co2_flux]
flux1[is.na(co2_flux2), co2_flux2:=-9999]


ggplot(flux1, aes(skewness_kurtosis_hf, co2_flux2))+
  geom_point()

ggplot(flux1, aes(DOY, co2_flux2, colour=skewness_kurtosis_hf))+
  geom_point()

ggplot(flux1)+
  geom_line(aes(DOY, co2_mole_fraction), colour="green")+
  geom_line(aes(DOY, h2o_mole_fraction), colour="blue")


# get fluxnet data for 2010 because the full output is missing things
flux2010a <- fread("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_2010/eddypro_JER_2010_fluxnet_2019-08-08T131210_adv.csv",
                   sep=",", header=TRUE, na.strings=c("-9999"))

flux2010b <- fread("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_2010/20190806_2/eddypro_JER_2010_fluxnet_2019-08-09T172917_adv.csv",
                   sep=",", header=TRUE, na.strings=c("-9999"))

fluxfn <- rbind(flux2010a, flux2010b)

fluxfn[,':=' (date_time = parse_date_time(TIMESTAMP_END,"YmdHM",tz="UTC"))][
  ,':='(date=as.Date.POSIXct(date_time),month=month(date_time),year=year(date_time))]


# import from other days:
# 20190906
flux2010c <- fread("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_2010/20190906/eddypro_JER_2010_fluxnet_2019-09-06T123044_adv.csv",
                   sep=",", header=TRUE, na.strings=c("-9999"))


head(flux2010c)
tail(flux2010c)
# 20190908
flux2010d <- fread("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_2010/20190908/eddypro_JER_2010_fluxnet_2019-09-08T230223_adv.csv",
                   sep=",", header=TRUE, na.strings=c("-9999"))


head(flux2010d[,1])
tail(flux2010d[,1])

# 20191108
flux2010e <- fread("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_2010/20191108/eddypro_JER_2010_fluxnet_2019-11-08T165519_adv.csv",
                   sep=",", header=TRUE, na.strings=c("-9999"))


head(flux2010e[,1])
tail(flux2010e[,1])




fluxfn2 <- rbind(flux2010c, flux2010d, flux2010e)

fluxfn2[,':=' (date_time = parse_date_time(TIMESTAMP_END,"YmdHM",tz="UTC"))][
  ,':='(date=as.Date.POSIXct(date_time),month=month(date_time),year=year(date_time))]


# make figures
ggplot(fluxfn, aes(date_time, FC))+
  geom_line()+
  ylim(c(-5,5))

ggplot(fluxfn2[month(date_time)==3&year(date_time)==2010], aes(date_time, FC))+
  geom_line()+
  ylim(c(-5,5))

# check the flux file which has the datalogger preliminary processed data in it. 
fluxtablenames <- fread("~/Desktop/TweedieLab/Projects/Jornada/Data/Tower/Flux/dataL1_flux_2010.csv", sep=",",skip=1, header=TRUE)[1,]
fluxtable <- fread("~/Desktop/TweedieLab/Projects/Jornada/Data/Tower/Flux/dataL1_flux_2010.csv", sep=",",skip=4,
                   header=FALSE, na.string=c("-9999"), col.names=colnames(fluxtablenames))
  
fluxtable[, ':=' (date_time = ymd_hms(TIMESTAMP))]

ggplot(fluxtable, aes(date_time, Fc_irga))+
  geom_line()+
  ylim(c(-5,5))

ggplot(fluxtable[month(date_time)==3,], aes(date_time, Fc_irga))+
  geom_line()+
  ylim(c(-5,5))

# look at CO2, H2O, air temp, and other variables from flux table
ggplot(fluxtable, aes(date_time, agc_Avg, colour=factor(agc_thrshld_excded_Tot)))+
  geom_point()

ggplot(fluxtable[month(date_time)<3,], aes(date_time, CO2_mean))+
  geom_line()+
  ylim(c(500,700))

ggplot(fluxtable, aes(date_time, stdev_CO2))+
  geom_line()+
  ylim(c(-200,200))

ggplot(fluxtable, aes(date_time, stdev_CO2))+
  geom_line()+
  ylim(c(-25,25))


ggplot(fluxtable[month(date_time)==5,])+
  geom_line(aes(date_time, cov_CO2_Ux), colour="red")+
  geom_line(aes(date_time, cov_CO2_Uy), colour="blue")+
  geom_line(aes(date_time, cov_CO2_Uz), colour="green")+
  ylim(c(-10,100))

ggplot(fluxtable, aes(date_time, H2O_Avg))+
  geom_line()+
  ylim(c(0,25))


ggplot(fluxtable, aes(date_time, tau))+
  geom_line()+
  ylim(c(0,2))

ggplot(fluxtable, aes(date_time, Ts_mean))+
  geom_line()

ggplot(fluxtable)+
  geom_line(aes(date_time, cov_Ts_Ux), colour="red")+
  geom_line(aes(date_time, cov_Ts_Uy), colour="blue")+
  geom_line(aes(date_time, cov_Ts_Uz), colour="green")+
  ylim(c(-10,10))


ggplot(fluxtable, aes(date_time, fw_Avg))+
  geom_line()

ggplot(fluxtable)+
  geom_line(aes(date_time, cov_fw_Ux), colour="red")+
  geom_line(aes(date_time, cov_fw_Uy), colour="blue")+
  geom_line(aes(date_time, cov_fw_Uz), colour="green")


ggplot(fluxtable)+
  geom_line(aes(date_time, Ux_Avg), colour="red")+
  geom_line(aes(date_time, Uy_Avg), colour="blue")+
  geom_line(aes(date_time, Uz_Avg), colour="green")

ggplot(fluxtable, aes(date_time, hor_wnd_spd_mean_rslt))+
  geom_line()+
  ylim(c(0,50))

ggplot(fluxtable, aes(date_time, hor_wnd_dir_mean_rslt))+
  geom_line()

ggplot(fluxtable, aes(date_time, Rn_nr_Avg))+
  geom_line()

ggplot(fluxtable, aes(date_time, Hs))+
  geom_line()+
  ylim(c(0,500))

ggplot(fluxtable, aes(date_time, LE_irga))+
  geom_line()+
  ylim(c(-500,500))


# import the raw files that are used for EC processing to see where missing CO2, H2O, sonic air temp occurs
fl_raw <- list.files(path="~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_IN_EddyCovariance/2010_2012/2010/",
                           full.names=TRUE, pattern="^dataL1_ts_") 

# read length of first data row and bind them into one file.
fileinfo_fx <- function(x) {colnums <- scan(x,what='list',sep=",",skip=4,nlines=1)
pathname <- dirname(x)
datname <- basename(x)
filename <- tools::file_path_sans_ext(basename(x))
date <- strsplit(filename,"_")[[1]][3]
  return(data.frame(datname=datname,
                    date=ymd(date),
                      col_num=length(colnums)))}

filerow1_fx <- function(x) {colnames <- scan(x,what='list',sep=",",skip=1,nlines=1)
datrow <- scan(x,what='list',sep=",",skip=4,nlines=1)
datname <- basename(x)
return(rbind(c(datname,colnames),
             c(datname,datrow)))}
  
fileinfo_raw <- do.call("rbind",(lapply(fl_raw,fileinfo_fx)))

# make data tables
fileinfo_raw <- data.table(fileinfo_raw)

# quick plot of dates with 14 and 15 data rows
ggplot(fileinfo_raw, aes(date,col_num))+geom_line()

# save the information about file lengths
write.table(fileinfo_raw, file="~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_IN_EddyCovariance/2010_2012/2010/FileLengthInfo.csv",
            sep=",", dec=".", row.names=FALSE)

move14 <- as.vector(unlist(fileinfo_raw[col_num==14,datname]))
move15 <- as.vector(unlist(fileinfo_raw[col_num==15,datname]))

# copy all the files with 14 first data rows into length14 folder
setwd("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_IN_EddyCovariance/2010_2012/2010/")
file.copy(move14,
          "~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_IN_EddyCovariance/2010_2012/2010/length14")

file.remove(move14)

# copy all the files with 15 first data rows into length15 folder
file.copy(move15,
          "~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_IN_EddyCovariance/2010_2012/2010/length15")

file.remove(move15)

# see if it worked by scanning the moved files
fl_14 <- list.files(path="~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_IN_EddyCovariance/2010_2012/2010/length14",
                     full.names=TRUE, pattern="^dataL1_ts_") 

info_move14 <- fileinfo_raw <- do.call("rbind",(lapply(fl_14,filerow1_fx)))

fl_15 <- list.files(path="~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_IN_EddyCovariance/2010_2012/2010/length15",
                    full.names=TRUE, pattern="^dataL1_ts_") 

info_move15 <- fileinfo_raw <- do.call("rbind",(lapply(fl_15,filerow1_fx)))
