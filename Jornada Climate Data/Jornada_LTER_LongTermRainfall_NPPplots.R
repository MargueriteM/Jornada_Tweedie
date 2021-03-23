# Using R code from https://portal.edirepository.org/nis/codeGeneration?packageId=knb-lter-jrn.210425001.75&statisticalFileType=r
# Modifying to make graphs and use data
# Modifications by: Marguerite Mauritz, 23 March 2021

# load libraries
library(tidyverse)
library(data.table)


# Package ID: knb-lter-jrn.210425001.75 Cataloging System:https://pasta.edirepository.org.
# Data set title: Gap-filled daily precipitation at the 15 long-term NPP sites at Jornada Basin LTER, 1980-ongoing.
# Data set creator:  Jin Yao - USDA-ARS Jornada Experimental Range 
# Data set creator:  John Anderson - Jornada Basin LTER 
# Data set creator:  Heather Savoy - USDA-ARS Jornada Experimental Range 
# Data set creator:  Debra Peters - USDA-ARS Jornada Experimental Range 
# Contact:  John Anderson -  Jornada Basin LTER  - 
# Contact:   Data Manager -  Jornada Basin LTER  - datamanager.jrn.lter@gmail.com
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-jrn/210425001/75/cdb08f51343547c9cc7080e80cd72bba" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               , col.names=c(
                 "date",     
                 "zone",     
                 "site",     
                 "est_precp"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

# attempting to convert dt1$date dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1date<-as.Date(dt1$date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1date) == length(tmp1date[!is.na(tmp1date)])){dt1$date <- tmp1date } else {print("Date conversion failed for dt1$date. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1date) 
if (class(dt1$zone)!="factor") dt1$zone<- as.factor(dt1$zone)
if (class(dt1$site)!="factor") dt1$site<- as.factor(dt1$site)
if (class(dt1$est_precp)=="factor") dt1$est_precp <-as.numeric(levels(dt1$est_precp))[as.integer(dt1$est_precp) ]               
if (class(dt1$est_precp)=="character") dt1$est_precp <-as.numeric(dt1$est_precp)

# Convert Missing Values to NA for non-dates



# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(date)
summary(zone)
summary(site)
summary(est_precp) 
# Get more details on character variables

summary(as.factor(dt1$zone)) 
summary(as.factor(dt1$site))
detach(dt1)               


inUrl2  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-jrn/210425001/75/6d819d09b13a21dcb2cf89add2fd694b" 
infile2 <- tempfile()
try(download.file(inUrl2,infile2,method="curl"))
if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")


dt2 <-read.csv(infile2,header=F 
               ,skip=1
               ,sep=","  
               , col.names=c(
                 "site",     
                 "priority",     
                 "gauge",     
                 "distance",     
                 "affiliation",     
                 "type",     
                 "start",     
                 "end"    ), check.names=TRUE)

unlink(infile2)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt2$site)!="factor") dt2$site<- as.factor(dt2$site)
if (class(dt2$priority)=="factor") dt2$priority <-as.numeric(levels(dt2$priority))[as.integer(dt2$priority) ]               
if (class(dt2$priority)=="character") dt2$priority <-as.numeric(dt2$priority)
if (class(dt2$gauge)!="factor") dt2$gauge<- as.factor(dt2$gauge)
if (class(dt2$distance)=="factor") dt2$distance <-as.numeric(levels(dt2$distance))[as.integer(dt2$distance) ]               
if (class(dt2$distance)=="character") dt2$distance <-as.numeric(dt2$distance)
if (class(dt2$affiliation)!="factor") dt2$affiliation<- as.factor(dt2$affiliation)
if (class(dt2$type)!="factor") dt2$type<- as.factor(dt2$type)                                   
# attempting to convert dt2$start dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp2start<-as.Date(dt2$start,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp2start) == length(tmp2start[!is.na(tmp2start)])){dt2$start <- tmp2start } else {print("Date conversion failed for dt2$start. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp2start)                                    
# attempting to convert dt2$end dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp2end<-as.Date(dt2$end,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp2end) == length(tmp2end[!is.na(tmp2end)])){dt2$end <- tmp2end } else {print("Date conversion failed for dt2$end. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp2end) 

# Convert Missing Values to NA for non-dates



# Here is the structure of the input data frame:
str(dt2)                            
attach(dt2)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(site)
summary(priority)
summary(gauge)
summary(distance)
summary(affiliation)
summary(type)
summary(start)
summary(end) 
# Get more details on character variables

summary(as.factor(dt2$site)) 
summary(as.factor(dt2$gauge)) 
summary(as.factor(dt2$affiliation)) 
summary(as.factor(dt2$type))
detach(dt2)               

# save the dt1 data
# save(dt1, file= "JER_longterm_Rainfall_NPPplots.Rdata")

# Graph long-term precipitation at multiple locations
ggplot(dt1, aes(date, est_precp, colour=site))+geom_line()

# calculate monthly sums
dt1 <- as.data.table(dt1)
dt1[,':='(year=year(date),month=month(date),doy=yday(date))]

dt1.month <- dt1[,.(precp.month=sum(est_precp)),
              by="site,year,month"]

# calculate daily cumulative per year and monthly
dt1[,precp_cum := cumsum(est_precp), by="site,year"]
dt1.month[,precp_cum := cumsum(precp.month), by="site,year"]

# add labels at the end of the year
dt1[,year_lab := ifelse(doy==365, year, NA)]
dt1.month[,year_lab := ifelse(month==12, year, NA)]

# calculate annual sums
dt1.year <- dt1[,.(precp.ann=sum(est_precp)),
                by="site,year"]

# graph monthly rainfall since 1980 for all locations
ggplot(dt1.month, aes(year+month/10,precp.month, colour=site))+geom_line()

# graph monthly rainfall since 2010 for all locations
dt1.month %>%
  filter(year>2009)%>%
ggplot(., aes(year+month/10,precp.month, colour=site))+geom_line()

# graph daily cumulative by site since 2010
dt1 %>%
  filter(year>2009)%>%
  ggplot(., aes(doy,precp_cum, colour=factor(year)))+geom_line()+facet_wrap(site~.)

# graph monthly cumulative by site since 2010
dt1.month %>%
  filter(year>2009)%>%
  ggplot(., aes(month,precp_cum, colour=factor(year)))+
  geom_line()+
  geom_label(aes(label=year_lab))+
  labs(y="Cummulative Rainfall (mm)", x="Month")+
  scale_x_continuous(breaks=c(seq(1,12,1)),
                     labels=c("J","F","M","A","M","J","J","A","S","O","N","D"))+
  facet_wrap(site~.)+
  theme_bw()

# graph only for Taylor Well
dt1.month %>%
  filter(year>2009 & site=="TAYL")%>%
  ggplot(., aes(month,precp_cum, colour=factor(year)))+
  geom_line()+
  geom_label(aes(label=year_lab))+
  labs(y="Cummulative Rainfall (mm)", x="Month")+
  scale_x_continuous(breaks=c(seq(1,12,1)),
                     labels=c("J","F","M","A","M","J","J","A","S","O","N","D"))+
  theme_bw()

# graph annual total precipitation for each year and location
ggplot(dt1.year, aes(year, precp.ann))+
  geom_col()+
  facet_wrap(site~.)+
  labs(y="Cummulative Rainfall (mm)", x="Year")+
  theme_bw()

# graph annual total precipitation since 2010 at Taylor
dt1.year %>%
  filter(year>2009 & site=="TAYL")%>%
  ggplot(., aes(year, precp.ann))+
  geom_col()+
  facet_wrap(site~.)+
  labs(y="Cummulative Rainfall (mm)", x="Year")+
  scale_x_continuous(breaks=seq(2010,2019,1))+
  theme_bw()

