# June 2019 sensor check

# all tower data is measuring
# some issues with ECTM (don't know depths and some not working: fix after monsoon)

# sensor netork, after system cycle in late May
# Soil moisture:
# [] means sensor is measuring but data is bad
# PRGL: [5], 10, [20], _
# BARE: _, 10, 20, 30
# LATR: 5, 10, 20, 30
# MUPO: 5, 10, [20], _
# target: get 5 and 10cm everywhere. Have no new sensors so have to rearrange sensors to replace
# Do comparison between veg types at each depth to see which are most similar
# replace the sensors that are LEAST similar so that we are capturing as much variability as possible. 

# PAR: 
# Bare: missing
# MUPO: SN6 and SN7 are missing
# DAPU: 2 are missing. DAPU most likely captures bare ground at this point
# leave PAR as is. Don't have enough sensors to replace all, we are currently measuring some replication of everything. 
# BARE ground is missing, can be substituted by DAPU

# Solar radiation: 
# DAPU: 2 missing
# leave as is

# pressure
# good

# lws:
# many missing, too many to replace and we are sampling some variety. Replace later or decide whether to keep this measurement

# SN moisture comparison:

library(data.table)
library(ggplot2)
library(lubridate)

# Sensor network data:
setwd("~/Desktop/TweedieLab/Projects/Jornada/Data/SensorNetwork/Combined")
SN_30min <- fread("SensorNetwork_L1_2010_2019_30min.csv", sep=",", header=TRUE)
# format date and add column to deginate the data stream
SN_30min[, ':=' (date_time = ymd_hms(date_time), datastream = "SN", location = "SN")]
setnames(SN_30min, 'sensor', 'variable')
# change variable names to match other datastreams
SN_30min[variable == "rain", variable := "precip.tot"]
SN_30min[variable == "moisture", variable := "soilmoisture"]
SN_30min[variable == "pressure", variable := "atm_press"]

# make depth in SN negative to indicate 'below the surface'
SN_30min[variable=="soilmoisture" & depth=="5", height:="-5"]
SN_30min[variable=="soilmoisture" & depth=="10", height:="-10"]
SN_30min[variable=="soilmoisture" & depth=="20", height:="-20"]
SN_30min[variable=="soilmoisture" & depth=="30", height:="-30"]

# convert pressure from mbar to kPa (1mbar = 0.1kPa)
SN_30min[variable=="atm_press", mean.val := mean.val/10]

# select only soil moisture data and move to wide format for making correlations
SN_moisture <- copy(SN_30min[variable=="soilmoisture"&!is.na(mean.val)])
SN_moisture[,id := paste(variable,SN,veg,height, sep="_")]

SN_moisture <- dcast.data.table(SN_moisture,date_time~id, value.var = "mean.val")
SN_moisture [, ':=' (year=year(date_time), month=month(date_time))]
colnames(SN_moisture)
# [1] "date_time"                 "soilmoisture_SN3_PRGL_-10" "soilmoisture_SN3_PRGL_-20" "soilmoisture_SN3_PRGL_-30"
# [5] "soilmoisture_SN3_PRGL_-5"  "soilmoisture_SN4_BARE_-10" "soilmoisture_SN4_BARE_-20" "soilmoisture_SN4_BARE_-30"
# [9] "soilmoisture_SN4_BARE_-5"  "soilmoisture_SN5_LATR_-10" "soilmoisture_SN5_LATR_-20" "soilmoisture_SN5_LATR_-30"
# [13] "soilmoisture_SN5_LATR_-5"  "soilmoisture_SN7_MUPO_-10" "soilmoisture_SN7_MUPO_-20" "soilmoisture_SN7_MUPO_-30"
# [17] "soilmoisture_SN7_MUPO_-5" 

# main question for moving sensors: 
# is PRGL or MUPO more similar to LATR?
# assume BARE is quite different and don't need the comparison. 
# we want to know wether to drop PRGL or MUPO to get a full profile of LATR and BARE
# and a partial profile of either PRGL or MUPO
# plot regressions by year

# LATR vs PRGL 5 and 10cm 
ggplot(SN_moisture, aes(`soilmoisture_SN3_PRGL_-5`, `soilmoisture_SN5_LATR_-5`, colour=factor(year(date_time))))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_wrap(month(date_time)~.)

ggplot(SN_moisture, aes(`soilmoisture_SN3_PRGL_-10`, `soilmoisture_SN5_LATR_-10`, colour=factor(year(date_time))))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_wrap(month(date_time)~.)


# LATR vs MUPO 5 and 10cm 
ggplot(SN_moisture, aes(`soilmoisture_SN7_MUPO_-5`, `soilmoisture_SN5_LATR_-5`, colour=factor(year(date_time))))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_wrap(month(date_time)~.)

ggplot(SN_moisture, aes(`soilmoisture_SN7_MUPO_-10`, `soilmoisture_SN5_LATR_-10`, colour=factor(year(date_time))))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_wrap(month(date_time)~.)

# do regressions by year and time
moisture.model <- function(df,x,y) { 
  fit <- lm(`soilmoisture_SN5_LATR_-5` ~ `soilmoisture_SN7_MUPO_-5`,  data=df) # y=mx+b
  return(list(b=coef(fit)[1], # intercept
              m=coef(fit)[2], # slope
              R2=summary(fit)$r.squared)) }

# apply function to every fence and plot
latr.mupo.5 <- SN_moisture[!is.na(`soilmoisture_SN5_LATR_-5`)&!is.na(`soilmoisture_SN7_MUPO_-5`),
                           list( b = coef(lm(`soilmoisture_SN5_LATR_-5` ~ `soilmoisture_SN7_MUPO_-5`))[1],
                                  m = coef(lm(`soilmoisture_SN5_LATR_-5` ~ `soilmoisture_SN7_MUPO_-5`))[2],
                                  R2 = summary(lm(`soilmoisture_SN5_LATR_-5` ~ `soilmoisture_SN7_MUPO_-5`))$r.squared,
                                 compare = "latr.mupo.5"),
                         by="year,month"]

latr.mupo.10 <- SN_moisture[!is.na(`soilmoisture_SN5_LATR_-10`)&!is.na(`soilmoisture_SN7_MUPO_-10`),
                           list( b = coef(lm(`soilmoisture_SN5_LATR_-10` ~ `soilmoisture_SN7_MUPO_-10`))[1],
                                 m = coef(lm(`soilmoisture_SN5_LATR_-10` ~ `soilmoisture_SN7_MUPO_-10`))[2],
                                 R2 = summary(lm(`soilmoisture_SN5_LATR_-10` ~ `soilmoisture_SN7_MUPO_-10`))$r.squared,
                                 compare = "latr.mupo.10"),
                           by="year,month"]

latr.prgl.5 <- SN_moisture[!is.na(`soilmoisture_SN5_LATR_-5`)&!is.na(`soilmoisture_SN3_PRGL_-5`),
                           list( b = coef(lm(`soilmoisture_SN5_LATR_-5` ~ `soilmoisture_SN3_PRGL_-5`))[1],
                                 m = coef(lm(`soilmoisture_SN5_LATR_-5` ~ `soilmoisture_SN3_PRGL_-5`))[2],
                                 R2 = summary(lm(`soilmoisture_SN5_LATR_-5` ~ `soilmoisture_SN3_PRGL_-5`))$r.squared,
                                 compare = "latr.prgl.5"),
                           by="year,month"]

latr.prgl.10 <- SN_moisture[!is.na(`soilmoisture_SN5_LATR_-10`)&!is.na(`soilmoisture_SN3_PRGL_-10`),
                            list( b = coef(lm(`soilmoisture_SN5_LATR_-10` ~ `soilmoisture_SN3_PRGL_-10`))[1],
                                  m = coef(lm(`soilmoisture_SN5_LATR_-10` ~ `soilmoisture_SN3_PRGL_-10`))[2],
                                  R2 = summary(lm(`soilmoisture_SN5_LATR_-10` ~ `soilmoisture_SN3_PRGL_-10`))$r.squared,
                                  compare = "latr.prgl.10"),
                            by="year,month"]

comparison <- rbind(latr.mupo.5, latr.mupo.10, latr.prgl.5, latr.prgl.10)

# make histograms of the R2 to compare frequency of matches
ggplot(comparison, aes(R2))+
  geom_histogram()+
  facet_wrap(compare~.)

# compare time-courses:
# LATR vs PRGL 5 and 10cm 
ggplot(SN_moisture[year(date_time)==2015,], aes(x=date_time))+
  geom_point(aes(y=`soilmoisture_SN3_PRGL_-5`, colour = "PRGL 5"), size=2)+
  geom_point(aes(y=`soilmoisture_SN5_LATR_-5`, colour= "LATR 5"),size=2)

ggplot(SN_moisture[year(date_time)==2015,], aes(x=date_time))+
  geom_point(aes(y=`soilmoisture_SN3_PRGL_-10`, colour = "PRGL 10"), size=2)+
  geom_point(aes(y=`soilmoisture_SN5_LATR_-10`, colour= "LATR 10"),size=2)


# LATR vs MUPO 5 and 10cm 
ggplot(SN_moisture[year(date_time)==2015,], aes(x=date_time))+
  geom_point(aes(y=`soilmoisture_SN7_MUPO_-5`, colour = "MUPO 5"), size=2)+
  geom_point(aes(y=`soilmoisture_SN5_LATR_-5`, colour= "LATR 5"),size=2)

ggplot(SN_moisture[year(date_time)==2015,], aes(x=date_time))+
  geom_point(aes(y=`soilmoisture_SN7_MUPO_-10`, colour = "MUPO 10"), size=2)+
  geom_point(aes(y=`soilmoisture_SN5_LATR_-10`, colour= "LATR 10"),size=2)


# calculate difference at each time and plot differences
SN_moisture[, ':=' (latr.prgl.5 = `soilmoisture_SN5_LATR_-5` - `soilmoisture_SN3_PRGL_-5`,
                    latr.prgl.10 = `soilmoisture_SN5_LATR_-10` - `soilmoisture_SN3_PRGL_-10`,
                    latr.mupo.5 = `soilmoisture_SN5_LATR_-5` - `soilmoisture_SN7_MUPO_-5`,
                    latr.mupo.10 = `soilmoisture_SN5_LATR_-10` -`soilmoisture_SN7_MUPO_-10`)]

# graph differences
# between LATR and PRGL or MUPO at 5
ggplot(SN_moisture, aes(x=date_time))+
  geom_point(aes(y=latr.prgl.5, colour = "prgl 5"))+
  geom_point(aes(y=latr.mupo.5, colour = "mupo 5"))

ggplot(SN_moisture)+
  geom_histogram(aes(latr.prgl.5, fill = "prgl 5"))+
  geom_histogram(aes(latr.mupo.5, fill = "mupo 5"))


# between LATR and PRGL or MUPO at 10
ggplot(SN_moisture)+
  geom_histogram(aes(latr.prgl.10, fill = "prgl 10"))+
  geom_histogram(aes(latr.mupo.10, fill = "mupo 10"))

