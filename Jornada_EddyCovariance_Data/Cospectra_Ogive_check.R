
library(ggplot2) # library for making figures in ggplot package
library(lubridate) # library for easier date manipulation 
library(data.table) # library for data table which is more efficient with large data sets
library(plyr)

spectrafiles <- list.files(path="~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/Test_20190617/eddypro_binned_cospectra", full.names = TRUE) 
ogivefiles <- list.files(path="~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/Test_20190617/eddypro_binned_ogives", full.names = TRUE) 


fread_colname <- function(colname){
  ret <- fread(colname, sep=",", header=TRUE, stringsAsFactors=FALSE, skip=11, na.strings=c("-9999"))
  obj_name <- tools::file_path_sans_ext(basename(colname))
  ret[,filename := obj_name]
  ret[,date_time:=sapply(strsplit(as.character(filename),"_"),"[",1)] #EDIT
  ret
}

spectra <- as.data.table(ldply(spectrafiles, fread_colname))
spectra[,date_time := as.POSIXct(date_time,format="%Y%m%d-%H%M")][,
':=' (date = as.Date(date_time),
      time = as.ITime(date_time))][,
      interval := cut(date_time,"3 hours")][,
      time.bin := as.ITime(interval)]

ogives <- as.data.table(ldply(ogivefiles, fread_colname))
ogives[,date_time := as.POSIXct(date_time,format="%Y%m%d-%H%M")][,
      ':=' (date = as.Date(date_time),
       time = as.ITime(date_time))][,
       interval := cut(date_time,"3 hours")][,
        time.bin := as.ITime(interval)]                                                                                                               time.bin := as.ITime(interval)]

ggplot(spectra[`f_nat*cospec(w_co2)/cov(w_co2)` > 0], aes(x=normalized_frequency)) + 
 # geom_line(aes(y = `f_nat*cospec(w_co2)/cov(w_co2)`, colour = "f_nat*cospec(w_co2)/cov(w_co2)"))+
  geom_point(aes(y = `f_nat*cospec(w_co2)/cov(w_co2)`, colour = "f_nat*cospec(w_co2)/cov(w_co2)"))+
#  geom_line(aes(y = `f_nat*cospec(w_ts)/cov(w_ts)`, colour = "f_nat*cospec(w_ts)/cov(w_ts)"))+
  geom_point(aes(y = `f_nat*cospec(w_ts)/cov(w_ts)`, colour = "f_nat*cospec(w_ts)/cov(w_ts)"))+
  scale_y_log10(breaks = c(10^-3, 10^-2, 10^-1, 10^-0))+
  scale_x_log10(breaks = c(10^-3, 10^-2, 10^-1, 10^-0))+
  annotation_logticks(scaled = T)+
  theme_bw()+
  facet_wrap(~time.bin)

# I don't think y-axis should be logarithmic for ogives?!?!
# plot ogives on normalized or natural frequency?? .... in cases where the ogive does converge to 1,
# it looks better with normalized. natural frequecy converges around 1.5

ggplot(ogives[`og(co2)`>(-1000) & `og(co2)`<15,], aes(x=normalized_frequency)) + 
  # geom_line(aes(y = `f_nat*cospec(w_co2)/cov(w_co2)`, colour = "f_nat*cospec(w_co2)/cov(w_co2)"))+
  geom_point(aes(y = `og(w_co2)`, colour = factor(time)))+
  #  geom_line(aes(y = `f_nat*cospec(w_ts)/cov(w_ts)`, colour = "f_nat*cospec(w_ts)/cov(w_ts)"))+
  #geom_point(aes(y = `f_nat*cospec(w_ts)/cov(w_ts)`, colour = "f_nat*cospec(w_ts)/cov(w_ts)"))+
  #scale_y_log10(breaks = c(10^-3, 10^-2, 10^-1, 10^-0))+
  scale_x_log10(breaks = c(10^-3, 10^-2, 10^-1, 10^-0))+
  annotation_logticks(scaled = T,sides="b")+
  theme_bw()+
  facet_wrap(~time.bin)+
  ylim(c(-25,25))


ggplot(ogives[normalized_frequency>(-2500)&month(date_time)==5,],
       aes((1/normalized_frequency)/60, `og(ts)`,colour=filename))+
  geom_line()+
  theme(legend.position="none")+
  facet_wrap(~time.bin)
