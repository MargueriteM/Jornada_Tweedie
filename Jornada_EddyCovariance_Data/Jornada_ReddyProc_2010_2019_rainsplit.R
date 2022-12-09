############################################
#  Process filtered EC data in ReddyProc   #
#           written by: M. Mauritz         #
#             August 2019                  #
############################################

library(REddyProc)
library(data.table)
library(lubridate)
library(gridExtra)
library(dplyr)
library(ggplot2)
library(gtable)
library(grid)
library(zoo)
library(bit64)
library(cowplot)

# import filtered flux data file from Eddy Pro as data table
# filtered in: Jornada_EddyPro_Output_Fluxnext_2010_2019.R
setwd("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_EddyPro_filtered")
# load("JER_flux_2010_2018_EddyPro_Output_filtered_20190929.Rdata")

# 29 Jan 2020
# import data that was filtered by 3SD filter
# load("JER_flux_2010_2019_EddyPro_Output_filtered_SD_20200128.Rdata")
# load("JER_flux_2010_2019_EddyPro_Output_filtered_SD_20200212.Rdata")


# 27 Apr 2020
# import data that's timestamp corrected
load("JER_flux_2010_2019_EddyPro_Output_filtered_SD_TIMEcorr_20200427.Rdata")

# convert date to POSIXct and get a year, day, hour column
# if this step doesn't work, make sure bit64 library is loaded otherwise the timestamps importa in a non-sensical format
flux_filter_sd[,':=' (date_time = parse_date_time(TIMESTAMP_END_correct,"YmdHM",tz="UTC"),
            date_time_start = parse_date_time(TIMESTAMP_START_correct,"YmdHM",tz="UTC"))][
  ,':='(Year=year(date_time),DoY=yday(date_time),
        hours = hour(date_time), mins = minute(date_time))]

# there's duplicated data in 2012 DOY 138
flux_filter <- (flux_filter_sd[!(duplicated(flux_filter_sd, by=c("TIMESTAMP_START_correct")))])

# exclude FC, LE, H data where FC_SSITC_TEST==1 because that data should only be used for budgets, not gap-filling

ggplot(flux_filter, aes(date_time,FC,colour=factor(FC_SSITC_TEST)))+
  geom_point()


ggplot(flux_filter, aes(date_time,LE,colour=factor(LE_SSITC_TEST)))+
  geom_point()

ggplot(flux_filter, aes(date_time,H,colour=factor(H_SSITC_TEST)))+
  geom_point()


# remove 1s
edata <- copy(flux_filter)

edata[FC_SSITC_TEST==1, FC := NA]
edata[LE_SSITC_TEST==1, LE := NA]
edata[H_SSITC_TEST==1, H := NA]

# format data columns for ReddyProc
# Year	DoY	Hour	NEE	LE	H	Rg	Tair	Tsoil	rH	VPD	Ustar 

edata[mins==0, Hour := hours+0.0]
edata[mins==30, Hour := hours+0.5]

edata <- edata[,.(Year,
                 DoY,
                 Hour,
                 FC,
                 LE,
                 H,
                 SW_IN_1_1_1,
                 TA_1_1_1,
                 RH_1_1_1,
                 USTAR,
                 P_RAIN_1_1_1)] # 23 June 2022: include P_RAIN to allow data processing with/without rain event split

ggplot(edata, aes(DoY,FC))+
  geom_line()+
  facet_grid(Year~.)

ggplot(edata, aes(DoY,LE))+
  geom_line()+
  facet_grid(Year~.)

ggplot(edata, aes(DoY,P_RAIN_1_1_1))+
  geom_line()+
  facet_grid(Year~.)

setnames(edata,c("FC","SW_IN_1_1_1","TA_1_1_1","RH_1_1_1","USTAR"),
         c("NEE","Rg","Tair","rH","Ustar"))

# make all Rg<0 equal to 0 becuase ReddyProc won't accept values <0
 edata[Rg<0, Rg:=0]

 # remove 2019 because that belongs to the following year
 # edata <- edata[Year!=2019,]
 
 # 
 
 # create a grid of full dates and times
 filled <- expand.grid(date=seq(as.Date("2010-01-01"),as.Date("2019-12-31"), "days"),
                       Hour=seq(0,23.5, by=0.5))
 filled$Year <- year(filled$date)
 filled$DoY <- yday(filled$date)
 
 filled$date <- NULL
 
 edata <- merge(edata,filled,by=c("Year","DoY","Hour"), all=TRUE)

  # and remove the 00:00 first time point in 2010. It's all NA and Reddyproc is getting upset
 # edata <- edata[!(Year==2010 & DoY == 1 & Hour == 0.0)]
 
 
 # online tool says hours must be between 0.5 and 24.0 
 # therefore add 0.5 to each hour
 edata[,Hour := Hour+0.5]
 
  # check that all days have 48 points
 daylength <- edata[,list(daylength=length(Hour)),by="Year,DoY"]
 
 ggplot(daylength, aes(DoY, daylength))+geom_point()+facet_wrap(Year~.)
 
 # convert edata to data frame for ReddyProc
 edata <- as.data.frame(edata)
 
# calculate VPD from rH and Tair in hPa (mbar), at > 10 hPa the light response curve parameters change
edata$VPD <- fCalcVPDfromRHandTair(edata$rH, edata$Tair)

# remove the first 8 days of 2010 which have NA data
edata2010 <- edata %>%
  filter(Year==2010&DoY>=9) %>%
  select(Year,
                                          DoY,
                                          Hour,
                                          NEE,
                                           LE,
                                           H,
                                           Rg,
                                           Tair,
                                          rH,
                                            Ustar)

edata2011 <- edata %>%
  filter(Year>=2011) %>%
  select(Year,
         DoY,
         Hour,
         NEE,
         LE,
         H,
         Rg,
         Tair,
         rH,
         Ustar)

edata1 <- rbind(edata2010,edata2011)


# look at 2010-2020 to determine how to split data by rain events
edata %>% 
  filter(Year==2010 & P_RAIN_1_1_1<39) %>%
  ggplot(., aes(DoY,P_RAIN_1_1_1))+
  geom_line()+
  facet_grid(Year~.)

edata %>% 
  filter(Year==2010 & P_RAIN_1_1_1<39) %>%
  ggplot(., aes(DoY+Hour/100,NEE))+
  geom_line()+
  facet_grid(Year~.)

# plot rain and NEE, aligned
plot_grid(edata %>% 
            filter(Year==2010 & P_RAIN_1_1_1<39&(DoY>150&DoY<250)) %>%
            ggplot(., aes(DoY+Hour/100,P_RAIN_1_1_1))+
            geom_line()+
            facet_grid(Year~.),
          edata %>% 
            filter(Year==2010 & P_RAIN_1_1_1<39&(DoY>150&DoY<250)) %>%
            ggplot(., aes(DoY+Hour/100,NEE))+
            geom_line()+
            facet_grid(Year~.), 
          nrow=2,ncol=1)

# determine rain events that last more than 6 hours (=12 rows)
# https://stackoverflow.com/questions/51371155/r-select-rainfall-events-and-calculate-rainfall-event-total-from-time-series-da
  flags <- edata  %>% 
  filter(Year==2010 & P_RAIN_1_1_1<39&(DoY>150&DoY<250))%>%
  # Set a rain flag if there is rain registered on the gauge
  mutate(rainflag = ifelse(P_RAIN_1_1_1 > 0, 1, 0)) %>% 
  # Create a column that contains the number of consecutive times there was rain or not.
  # Use `rle`` which indicates how many times consecutive values happen, and `rep`` to repeat it for each row.
  ##mutate(rainlength = rep(rle(rainflag)$lengths, rle(rainflag)$lengths)) %>% 
  # MM modify: sequence counts number of days with no rain fore ach individual day rather than the total days of a rain event for all rows
   # https://predictivehacks.com/count-the-consecutive-events-in-r/
       mutate(rainlength = sequence(rle(rainflag)$lengths)) %>%  
  # Set a flag for an event happening, when there is rain there is a rain event, 
  # when it is 0 but not for six consecutive times, it is still a rain event
  mutate(
    eventflag = ifelse(
      rainflag == 1, 
      1, 
      ifelse(
        rainflag == 0 & rainlength < 12, 
        1, 
        0
      )
    )
  ) %>% 
  # Correct for the case when the dataset starts with no rain for less than six consecutive times
  # If within the first six rows there is no rain registered, then the event flag should change to 0
  mutate(eventflag = ifelse(row_number() < 12 & rainflag == 0, 0, eventflag)) %>% 
  # Add an id to each event (rain or not), to group by on the pivot table
  mutate(eventid = case_when(eventflag==1 ~rep(seq(1,length(rle(eventflag)$lengths)), rle(eventflag)$lengths)))


  # plot rain and NEE, aligned with rain events
  plot_grid(flags %>% 
              filter(Year==2010 & P_RAIN_1_1_1<39&(DoY>150&DoY<250)) %>%
              ggplot(., aes(DoY+Hour/100,P_RAIN_1_1_1,color=factor(eventid)))+
              geom_line()+
              facet_grid(Year~.),
            flags %>% 
              filter(Year==2010 & P_RAIN_1_1_1<39&(DoY>150&DoY<250)) %>%
              ggplot(., aes(DoY+Hour/100,NEE,color=factor(eventid)))+
              geom_line()+
              facet_grid(Year~.), 
            nrow=2,ncol=1)
  
# online tool says missing values must be -9999, convert all NA to -9999
edata[is.na(edata)]=-9999

# export data for online tool of ReddyProc,

# subset data without rain splits
edata.noRain <- edata[,.(Year,
                                  DoY,
                                  Hour,
                                  NEE,
                                  LE,
                                  H,
                                  Rg,
                                  Tair,
                                  rH,
                                  Ustar)]

# with timesstamp corrected
# write.table(edata.noRain, file="~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/ReddyProc/20200427/JER_ReddyProc_Input_2011_2019_20200427.txt", sep=" ", dec=".",row.names=FALSE)


#write.table(edata.noRain, file="~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/ReddyProc/20203001/JER_ReddyProc_Input_2011_2019_20200131.txt", sep=" ", dec=".",row.names=FALSE)
# saved before rain/no rain split was created
#write.table(edata2011, file="~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/ReddyProc/20200212/JER_ReddyProc_Input_2011_2019_20200212.txt", sep=" ", dec=".",row.names=FALSE)

# Process with all the SSITC_TEST==1 removed
# saved before rain/no rain split was created
#write.table(edata2011, file="~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/ReddyProc/20200220/JER_ReddyProc_Input_2011_2019_20200220.txt", sep=" ", dec=".",row.names=FALSE)

# Run ReddyProc

EddyDataWithPosix <- subset(edata,Year==2010&DoY>=9) %>% 
  fConvertTimeToPosix('YDH', Year = 'Year', Day = 'DoY', Hour = 'Hour')

EddyDataWithPosix$sDateTime <- EddyDataWithPosix$DateTime

EProc <- sEddyProc$new('JERbajada',EddyDataWithPosix,c('NEE','Rg','Tair','VPD',"Ustar"))

# set location and time zone for site to gap-fill Met data during partitioning
# (updated to correct coords 20190924)
EProc$sSetLocationInfo(LatDeg = 32.581954, LongDeg = -106.635017, TimeZoneHour = -6)

# estimate Ustar thresholds (https://github.com/bgctw/REddyProc/blob/master/vignettes/useCase.md)
ustar_dat <- EddyDataWithPosix[,c("sDateTime","Ustar","NEE","Tair","Rg")]

ustar_dat <- subset(ustar_dat, !is.na(NEE)&!is.na(Tair)&!is.na(Rg))

ustar_est <- usEstUstarThreshold(ustar_dat, isCleaned=TRUE)

EProc$sEstimateUstarScenarios(
  nSample = 100L, probs = c(0.025, 0.05, 0.5, 0.95, 0.975))

# look at Ustar estimates
EProc$sGetEstimatedUstarThresholdDistribution()

# Ustar values
EProc$sGetUstarScenarios()

# # show ustar figure
# EProc$sPlotNEEVersusUStarForSeason()
# 
# EProc$sPlotNEEVersusUStarForSeason(season = "2015001",
#                                    format = "pdf", dir = "/Users/memauritz/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_EddyPro_filtered",
#                                    UstarColName = "Ustar",
#                                    NEEColName = "NEE",
#                                    TempColName = "Tair",
#                                    WInch = 16 * 0.394, HInchSingle = 6 * 0.394,
#                                    data = cbind(EProc$sDATA, EProc$sTEMP,
#                                                 EProc$sUSTAR_DETAILS$bins[, c("uStarBin",
#                                                                               "tempBin")]))
# 
# sEddyProc_sPlotNEEVersusUStarForSeason(season = "2015001",
#                                        format = "pdf", dir = "/Users/memauritz/Desktop/TweedieLab/Projects/Jornada/LTAR_Synthesis_Browning",
#                                        UstarColName = "Ustar",
#                                        NEEColName = "NEE",
#                                        TempColName = "Tair",
#                                        WInch = 16 * 0.394, HInchSingle = 6 * 0.394,
#                                        data = cbind(EProc$sDATA, EProc$sTEMP,
#                                                     EProc$sUSTAR_DETAILS$bins[, c("uStarBin",
#                                                                                   "tempBin")]))
# 
# plotNEEVersusUStarTempClass(NEEUStar.F = cbind(EProc$sDATA, EProc$sTEMP,
#                                                EProc$sUSTAR_DETAILS$bins[, c("uStarBin",
#                                                                              "tempBin")]),
#                             uStarTh=0.105,
#                             UstarColName = "Ustar",
#                             NEEColName = "NEE",
#                             TempColName = "Tair")


# use MDS for gap-filling
EProc$sMDSGapFillUStarScens('NEE')
# EProc$sMDSGapFillUStarScens('NEE')


# column names that denote Ustar uncertainty
grep("NEE_.*_f$",names(EProc$sExportResults()), value = TRUE)
grep("NEE_.*_fsd$",names(EProc$sExportResults()), value = TRUE)

#Look at NEE filtered at different Ustar levels
EProc$sPlotFingerprintY('NEE', Year = 2011)
EProc$sPlotFingerprintY('NEE_U2.5_f', Year = 2011)
EProc$sPlotFingerprintY('NEE_U05_f', Year = 2011)
EProc$sPlotFingerprintY('NEE_U50_f', Year = 2011)
EProc$sPlotFingerprintY('NEE_U97.5_f', Year = 2011)


EProc$sPlotFingerprintY('NEE', Year = 2012)
EProc$sPlotFingerprintY('NEE_U50_f', Year = 2012)

EProc$sPlotFingerprintY('NEE', Year = 2013)
EProc$sPlotFingerprintY('NEE_U50_f', Year = 2013)

EProc$sPlotFingerprintY('NEE', Year = 2014)
EProc$sPlotFingerprintY('NEE_U50_f', Year = 2014)

EProc$sPlotFingerprintY('NEE', Year = 2015)
EProc$sPlotFingerprintY('NEE_U50_f', Year = 2015)

EProc$sPlotFingerprintY('NEE', Year = 2016)
EProc$sPlotFingerprintY('NEE_U50_f', Year = 2016)

EProc$sPlotFingerprintY('NEE', Year = 2017)
EProc$sPlotFingerprintY('NEE_U50_f', Year = 2017)

EProc$sPlotFingerprintY('NEE', Year = 2018)
EProc$sPlotFingerprintY('NEE_U50_f', Year = 2018)

EProc$sPlotFingerprintY('NEE', Year = 2018)
EProc$sPlotFingerprintY('NEE_U50_f', Year = 2019)


# Partition NEE data and gap-fill Met data
EProc$sMDSGapFill('Tair', FillAll = FALSE,  minNWarnRunLength = NA)     
EProc$sMDSGapFill('VPD', FillAll = FALSE,  minNWarnRunLength = NA)     
EProc$sMDSGapFill('Rg', FillAll = FALSE,  minNWarnRunLength = NA)     

# Night-time partitioning
EProc$sMRFluxPartitionUStarScens()

# column names
grep("GPP.*_f$|Reco",names(EProc$sExportResults()), value = TRUE)

# fingerprint plot of night-time partitioned GPP and Reco
EProc$sPlotFingerprintY('GPP_U50_f', Year = 2014)
EProc$sPlotFingerprintY('GPP_U50_f', Year = 2015)
EProc$sPlotFingerprintY('GPP_U50_f', Year = 2016)

EProc$sPlotFingerprintY('Reco_uStar', Year = 2015)

# estimate uncertainty in GPP associated with using different Ustar thresholds
tmp <- names(EProc$sGetUstarScenarios())
uStarScencs = tmp[-1]

  
# day-time partitioning
EProc$sGLFluxPartitionUStarScens()
grep("GPP.*_DT$|Reco.*_DT",names(EProc$sExportResults()), value = TRUE)

# fingerprint plot of daytime-time partitioned GPP and Reco
EProc$sPlotFingerprintY('GPP_DT_U50', Year = 2015)
EProc$sPlotFingerprintY('Reco_DT_U50', Year = 2015)


# put EProc into a dataframe
results = EProc$sExportResults()
CombinedData = cbind(edata,results)

# estimate uncertainty due to u* thresholds
uStarTh <- EProc$sEstUstarThresholdDistribution( nSample = 100L, probs = c(0.025,0.05, 0.5, 0.95, 0.975))

uStarTh %>%
  filter( aggregationMode == "year") %>% select( uStar, "2.5%","5%", "50%", "95%", "97.5%")

uStarThAnnual <- usGetAnnualSeasonUStarMap(uStarTh)[-2]
uStarSuffixes <- colnames(uStarThAnnual)[-1]
print(uStarThAnnual)

sfx <- uStarSuffixes[1:4]

GPPAgg <- sapply(uStarSuffixes[1:4], function(sfx){
  GPPHalfHour <- CombinedData[[paste0("GPP_",sfx,"_f")]]
  mean(GPPHalfHour, na.rm = TRUE) })

print(GPPAgg)

# difference between mean of all U* threshold estimates is the approximate uncertainty
(max(GPPAgg) - min(GPPAgg)) / median(GPPAgg)

# estimate uncertainty for Reco
RecoAgg <- sapply(uStarSuffixes[1:4], function(sfx){
  RecoHalfHour <- CombinedData[[paste0("Reco_",sfx)]]
  mean(RecoHalfHour, na.rm = TRUE) })

print(RecoAgg)

# difference between mean of all U* threshold estimates is the approximate uncertainty
(max(RecoAgg) - min(RecoAgg)) / median(RecoAgg)


# export the results to a csv file
# fWriteDataframeToFile(CombinedData, 'JER_2014_2016_ReddyProc_20190828.txt', Dir = "~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/ReddyProc/")

# setwd("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/ReddyProc/")
# combined_colnames <- colnames((fread('JER_2014_2016_ReddyProc_20190828.txt',header=TRUE))[1,])

#CombinedData <- fread('JER_2014_2016_ReddyProc_20190828.txt', dec=",",
#                      na.strings=c("-9999","-9999.0","NAN","#NAME?"),
#                      header=FALSE,skip=2,col.names=combined_colnames,
# colClasses = "numeric)


ggplot(subset(CombinedData,DoY==50), aes(Hour,PotRad_U95))+geom_line()

# plot with U* filter and no gapfill
ggplot(subset(CombinedData), aes(DoY,NEE_U50_orig))+
  geom_point()+
  facet_grid(.~Year)

fig_nee <- ggplot(subset(CombinedData), aes(DoY,NEE_U50_orig))+
  geom_line()+
  geom_point(aes(y=NEE_U50_f),data=subset(CombinedData, is.na(NEE_U50_orig)),colour="red",size=1)+
  facet_grid(.~Year)#+
  ylim(c(-10,10))

  fig_nee_fill <- ggplot(subset(CombinedData), aes(DoY,NEE_U50_f))+
    geom_line()+
    facet_grid(.~Year)+
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank())#+
  ylim(c(-10,10))
  
fig_reco <- ggplot(subset(CombinedData), aes(DoY,Reco_U50))+geom_line()+
  geom_line(aes(y=Reco_DT_U50),colour="blue")+
  facet_grid(.~Year)+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())#+
  ylim(c(-10,10))

  # plot daytime Reco with qc code
  ggplot(subset(CombinedData), aes(DoY,Reco_DT_U50, colour=factor(FP_qc)))+
    geom_point(size=1)+
    facet_grid(.~Year)
  
fig_gpp <- ggplot(subset(CombinedData), aes(DoY,GPP_U50_f))+geom_line()+
  geom_line(aes(y=GPP_DT_U50),colour="blue")+
  facet_grid(.~Year)+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())#+
  ylim(c(-10,10))

fig_vpd <- ggplot(subset(CombinedData), aes(DoY,VPD))+geom_line()+
geom_hline(yintercept=10, colour="red")+
  facet_grid(.~Year)
  
grid.arrange(fig_nee, fig_reco, fig_gpp, fig_vpd, nrow=4)

grid.arrange(fig_nee_fill, fig_reco, fig_gpp, nrow=3)

grid.arrange(fig_nee, fig_gpp, fig_vpd, nrow=3)
grid.arrange(fig_nee, fig_reco, fig_vpd, nrow=3)

# plot measured vs modeled NEE at U50
ggplot(subset(CombinedData), aes(NEE_U50_orig, NEE_U50_fall))+geom_point()+
geom_abline(intercept=0, slope=1)+
  facet_grid(.~Year)

# plot NEE with different U* quantiles
ggplot(subset(CombinedData), aes(x=DoY))+
  geom_line(aes(y=NEE_U2.5_f, colour="NEE_U2.5_f"))+
  geom_line(aes(y=NEE_U50_f, colour="NEE_U50_f"))+
  geom_line(aes(y=NEE_U97.5_f, colour="NEE_U97.5_f"))+
  facet_grid(.~Year)


# heat map of gap-filled NEE data
ggplot(CombinedData,
       aes(DoY,Hour,fill=NEE_U50_f))+
  geom_tile(color= "white",size=0.1) + 
  scale_fill_viridis(name=expression(paste('C',O[2],' flux',sep='')))+
  facet_grid(Year~.)+
  scale_y_continuous(breaks=c(0,12,23),
                     labels=c("00:00","12:00","23:00"),
                     expand=c(0,0))+
  scale_x_continuous(breaks =c(31,61,91,121,151,181,211,241,271,301,331,361),limits=c(1,367),
                     labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
                     expand=c(0,0))+
  labs(title= expression("Half-hourly flux (μmol C" *O[2]* m^-2* "se" *c^-1*")"), x="Day", y="Half-hour") +
  theme(
    plot.title=element_text(size = 14),
    axis.text.x=element_text(size=10),
    axis.text.y=element_text(size=8),
    axis.title=element_text(size=11),
    strip.background = element_blank(),
    strip.text=element_text(size=10),
    axis.ticks=element_blank(),
    legend.title=element_text(size=10),
    legend.text=element_text(size=9),
    panel.grid=element_blank(),
    panel.background=element_rect(fill="white"))





# calculate daily sums of Co2 flux in umol/m2/sec converted to gC/m2/day

daily_sum_dt <- as.data.table(subset(CombinedData))
daily_sum <- daily_sum_dt[,list(NEE_daily = sum(NEE_U50_f*1800*1*10^-6*12.01),
                                GPP_daily = sum(GPP_U50_f*1800*1*10^-6*12.01),
                                Reco_daily = sum(Reco_U50*1800*1*10^-6*12.01),
                                Tair_mean = mean(Tair)), 
                          by="Year,DoY"]

# add a date variable to daily_sum
daily_sum[Year==2011,date:= as.Date(DoY-1, origin = "2011-01-01")]
daily_sum[Year==2012,date:= as.Date(DoY-1, origin = "2012-01-01")]
daily_sum[Year==2013,date:= as.Date(DoY-1, origin = "2013-01-01")]
daily_sum[Year==2014,date:= as.Date(DoY-1, origin = "2014-01-01")]
daily_sum[Year==2015,date:= as.Date(DoY-1, origin = "2015-01-01")]
daily_sum[Year==2016,date:= as.Date(DoY-1, origin = "2016-01-01")]
daily_sum[Year==2017,date:= as.Date(DoY-1, origin = "2017-01-01")]
daily_sum[Year==2018,date:= as.Date(DoY-1, origin = "2018-01-01")]


# calculate cumulative sums
daily_cum_sum <- daily_sum[,list(NEE_cum = cumsum(NEE_daily),
                                GPP_cum = cumsum(GPP_daily),
                                 Reco_cum = cumsum(Reco_daily)),
                           by="Year"]

annual_sum <- daily_sum[,list(NEE_annual = sum(NEE_daily)),
                        by="Year"]

# save daily and annual sums
#setwd("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/ReddyProc/")
#write.table(daily_sum, "JER_ReddyProc_daily_sum_CO2_2011_2018.csv", sep=",", dec=".", row.names=FALSE)
#write.table(annual_sum, "JER_ReddyProc_annual_sum_CO2_2011_2018.csv", sep=",", dec=".", row.names=FALSE)


fig_daily_NEE <- ggplot(daily_sum, aes(DoY, NEE_daily))+
  geom_line(colour="blue")+
  geom_hline(yintercept=0)+
  labs(y="Daily cumulative NEE gC/m2")+
  facet_grid(.~Year)+
  theme_bw()

ggplot(annual_sum, aes(factor(Year),NEE_annual))+
  geom_bar(stat="identity")+
  labs(y="Annual cumulative NEE gC/m2")+
  theme_bw()

# plot daily precip
precip_daily <- flux_filter[,list(precip.tot = sum(P_RAIN_1_1_1)),
                            by="Year,DoY"]

# add a date variable to daily precip
precip_daily[Year==2011,date:= as.Date(DoY-1, origin = "2011-01-01")]
precip_daily[Year==2012,date:= as.Date(DoY-1, origin = "2012-01-01")]
precip_daily[Year==2013,date:= as.Date(DoY-1, origin = "2013-01-01")]
precip_daily[Year==2014,date:= as.Date(DoY-1, origin = "2014-01-01")]
precip_daily[Year==2015,date:= as.Date(DoY-1, origin = "2015-01-01")]
precip_daily[Year==2016,date:= as.Date(DoY-1, origin = "2016-01-01")]
precip_daily[Year==2017,date:= as.Date(DoY-1, origin = "2017-01-01")]
precip_daily[Year==2018,date:= as.Date(DoY-1, origin = "2018-01-01")]


fig_daily_rain <- ggplot(precip_daily[Year!=2010,], aes(DoY, precip.tot))+
  geom_line(colour="blue")+
  labs(y="Daily Total Rain (mm)")+
  facet_grid(.~Year)+
  theme_bw()

grid.arrange(fig_daily_NEE,fig_daily_rain, nrow=2)


# plot the daily NEE values by month and year
ggplot(daily_sum, aes(factor(DoY), NEE_daily))+
  geom_boxplot()+
  geom_hline(yintercept=0)+
  scale_x_discrete(breaks =c("31","61","91","121","151","181","211","241","271","301","331","361"),
                   labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
  labs(y="Daily cumulative NEE gC/m2",x="Month")+
  theme_bw()

# look just at May - July in 2015
# look at measured NEE for May to June 
ggplot(subset(CombinedData,Year==2015 & DoY>=121 & DoY<=212),
       aes(x=DoY+Hour/24))+
  geom_line(aes(y=NEE_U50_orig))+
  geom_point(aes(y=NEE_U50_f),
             data=subset(CombinedData,Year==2015 & DoY>=121 & DoY<=212 & is.na(NEE_U50_orig)),
             colour="red",size=1)+
  geom_vline(xintercept=175)

ggplot(subset(CombinedData,Year==2015 & DoY>=121 & DoY<=212),
       aes(x=DoY+Hour/24))+
  # geom_line(aes(y=NEE_U50_orig))+
  geom_line(aes(y=NEE_U50_f))+
  geom_hline(yintercept=0)



ggplot()+
  #geom_line(aes(x=DoY,y=NEE_daily), data=subset(daily_sum,Year==2015))+
  geom_line(aes(x=DoY,y=NEE_daily),
            data=subset(daily_sum,Year==2015 & DoY>=121 & DoY<=212), colour="blue")+
  geom_line(aes(x=DoY,y=-GPP_daily),
              data=subset(daily_sum,Year==2015 & DoY>=121 & DoY<=212), colour="green")+
  geom_line(aes(x=DoY,y=Reco_daily),
            data=subset(daily_sum,Year==2015 & DoY>=121 & DoY<=212), colour="brown")+
  geom_hline(yintercept=0)+
  labs(y="Daily cumulative NEE gC/m2")+
  geom_vline(xintercept=175)+
  jorn15_scale()

jorn15_scale <- function() scale_x_continuous(
  breaks = c(155, 168, 175, 182, 189, 196),
  #limits = c(150, 198),
  labels = c("Jun 4", "Jun 17", "Jun 24", "Jul 1", "Jul 8", "Jul 15"))


# for CZO and to match the Reynolds Creek data plot Oct 2014 - Oct 2015
fig_nee_czo <- ggplot(daily_sum[(Year==2014&DoY>=274) | (Year==2015&DoY<274)], aes(date, NEE_daily))+
  geom_line(colour="black")+
  geom_hline(yintercept=0)+
  labs(y="Daily NEP (gC/m2/dy)")+
  scale_x_date(name="Water Year 2015",date_breaks = "1 month",
                     date_labels="%b")+
  annotate(geom="text", x=as.Date("2015-09-28"), y=0.67, label="(b)",
           color="black")+
  theme(axis.text.y = element_text(margin=unit(c(2,2,2,2),"mm")),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.length=unit(-1,"mm"),
        panel.background = element_rect(fill="white"),
        panel.border = element_rect(colour="black", fill=NA),
        panel.grid = element_blank(),
        plot.margin=unit(c(0.5,2,0.5,2.8),"mm"))

fig_gpp_reco_czo <- ggplot(daily_sum[(Year==2014&DoY>=274) | (Year==2015&DoY<274)])+
  geom_line(aes(date, GPP_daily),colour="#009933",size=0.4)+
  geom_line(aes(date, Reco_daily),colour="#CC6600",linetype="dashed",size=0.7)+
  geom_hline(yintercept=0)+
  labs(y="Daily GEP and ER (gC/m2/dy)")+
  scale_x_date(name="Water Year 2015",date_breaks = "1 month",
                                                     date_labels="%b")+
  annotate(geom="text", x=as.Date("2015-09-28"), y=2.5, label="(c)",
           color="black")+
  theme(axis.text.x = element_text(margin=unit(c(2,2,2,2),"mm")),
        axis.text.y = element_text(margin=unit(c(2,2,2,2),"mm")),
        axis.ticks.length=unit(-1,"mm"),
        panel.background = element_rect(fill="white"),
            panel.border = element_rect(colour="black", fill=NA),
            panel.grid = element_blank(),
        plot.margin=unit(c(0.5,2,2,3.2),"mm"))


# fig_reco_czo <- ggplot(daily_sum[(Year==2014&DoY>=274) | (Year==2015&DoY<274)], aes(date, Reco_daily))+
#   geom_line(colour="blue")+
#   geom_hline(yintercept=0)+
#   labs(y="Daily cumulative ER (gC/m2/dy)")+
#   scale_x_date(name="Water Year 2015",date_breaks = "1 month",
#                date_labels="%b")+
#   theme_bw()

fig_precip_czo <- ggplot(precip_daily[(Year==2014&DoY>=274) | (Year==2015&DoY<274)], aes(date, precip.tot))+
  geom_bar(colour="#3333CC",stat="identity")+
  labs(y="Total Rain (mm/dy)")+
  scale_x_date(name="Water Year 2015",date_breaks = "1 month",
               date_labels="%b")+
  annotate(geom="text", x=as.Date("2015-09-28"), y=12.3, label="(a)",
                                          color="black")+
  theme(axis.text.y = element_text(margin=unit(c(2,2,2,2),"mm")),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.length=unit(-1,"mm"),
        panel.background = element_rect(fill="white"),
        panel.border = element_rect(colour="black", fill=NA),
        panel.grid = element_blank(),
        plot.margin=unit(c(1,2,0.5,2),"mm"))


# arrange the figures making them all the same size:
g1 <- ggplotGrob(fig_precip_czo)
g2 <- ggplotGrob(fig_nee_czo)
g3 <- ggplotGrob(fig_gpp_reco_czo)

g <- rbind(g1, g2, g3, size = "first")
g$widths <- unit.pmax(g1$widths, g2$widths, g3$widths)
grid.newpage()
grid.draw(g)

# save figure
# ggsave("Fig_NEE_GPP_ER_PPT_2015_CZO", plot = g, device = "jpeg", path = "~/Desktop/TweedieLab/Proposals/2019/CZO",
#        scale = 1,height=6.83, width=4.83,
#        dpi = 500)


# make a figure that zooms in on only June
fig_nee_czo_jun <- ggplot(daily_sum[(Year==2015&month(date)==6)], aes(date, NEE_daily))+
  geom_line(colour="black")+
  geom_hline(yintercept=0)+
  labs(y="Daily NEP (gC/m2/dy)")+
  scale_x_date(name="June 2015",date_breaks = "5 days",
               date_labels="%d")+
  annotate(geom="text", x=as.Date("2015-06-28"), y=0.67, label="(b)",
           color="black")+
  theme(axis.text.y = element_text(margin=unit(c(2,2,2,2),"mm")),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.length=unit(-1,"mm"),
        panel.background = element_rect(fill="white"),
        panel.border = element_rect(colour="black", fill=NA),
        panel.grid = element_blank(),
        plot.margin=unit(c(0.5,2,0.5,2.8),"mm"))

fig_gpp_reco_czo_jun <- ggplot(daily_sum[(Year==2015&month(date)==6)])+
  geom_line(aes(date, GPP_daily),colour="#009933",size=0.4)+
  geom_line(aes(date, Reco_daily),colour="#CC6600",linetype="dashed",size=0.7)+
  geom_hline(yintercept=0)+
  labs(y="Daily GEP and ER (gC/m2/dy)")+
  scale_x_date(name="June 2015",date_breaks = "5 days",
               date_labels="%d")+
  annotate(geom="text", x=as.Date("2015-06-28"), y=2.5, label="(c)",
           color="black")+
  theme(axis.text.x = element_text(margin=unit(c(2,2,2,2),"mm")),
        axis.text.y = element_text(margin=unit(c(2,2,2,2),"mm")),
        axis.ticks.length=unit(-1,"mm"),
        panel.background = element_rect(fill="white"),
        panel.border = element_rect(colour="black", fill=NA),
        panel.grid = element_blank(),
        plot.margin=unit(c(0.5,2,2,3.2),"mm"))



fig_precip_czo_jun <- ggplot()+
  geom_bar(aes(date, precip.tot),fill="#3333CC",stat="identity",data=precip_daily[(Year==2015&month(date)==6)])+
  geom_hline(yintercept=0,colour="#3333CC")+
  geom_line(aes(date, Tair_mean/10),data=daily_sum[(Year==2015&month(date)==6)])+
  labs(y="Total Rain (mm/dy)")+
 scale_y_continuous(sec.axis = sec_axis(~.*10, name = expression("Temperature ("~degree~"C)")))+
  scale_x_date(name="June 2015",date_breaks = "5 days",
               date_labels="%d")+
  annotate(geom="text", x=as.Date("2015-06-28"), y=3.8, label="(a)",
           color="black")+
  theme(axis.text.y = element_text(margin=unit(c(2,2,2,2),"mm")),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.length=unit(-1,"mm"),
        panel.background = element_rect(fill="white"),
        panel.border = element_rect(colour="black", fill=NA),
        panel.grid = element_blank(),
        plot.margin=unit(c(1,2,0.5,2),"mm"))


# arrange the figures making them all the same size:
g4 <- ggplotGrob(fig_precip_czo_jun)
g5 <- ggplotGrob(fig_nee_czo_jun)
g6 <- ggplotGrob(fig_gpp_reco_czo_jun)

g.jun <- rbind(g4, g5, g6, size = "first")
g.jun$widths <- unit.pmax(g4$widths, g5$widths, g6$widths)
grid.newpage()
grid.draw(g.jun)

# save figure
# ggsave("Fig_NEE_GPP_ER_PPT_June_2015_CZO", plot = g.jun, device = "jpeg", path = "~/Desktop/TweedieLab/Proposals/2019/CZO",
#        scale = 1,height=6.83, width=4.83,
#        dpi = 500)

# create a running mean 
daily_run <- daily_sum[,list(date=unique(date),
                             NEE_daily_roll= rollmean(x=NEE_daily,
                            k=7,
                            fill=NA))]

# plot to see what it looks like
ggplot()+
  geom_line(data=daily_run[(year(date)==2014&yday(date)>=274) | (year(date)==2015&yday(date)<274)], aes(date,NEE_daily_roll),size=2)+
  geom_line(data=daily_sum[(Year==2014&DoY>=274) | (Year==2015&DoY<274)], aes(date, NEE_daily),colour="red")

# save as csv
#setwd("~/Desktop/TweedieLab/Proposals/2019/CZO")
#write.table(daily_run[(year(date)==2014&yday(date)>=274) | (year(date)==2015&yday(date)<274),],
#            "Daily_NEE_runningmean_water2015.csv", sep=",", dec=".", row.names=FALSE)

# for Anthony and Isabelle's nutrient paper, exclude the long gap-filled days
# exclude days 169 to 176 (18 to 25 June)
ggplot()+
  #geom_line(aes(x=DoY,y=NEE_daily), data=subset(daily_sum,Year==2015))+
  geom_line(aes(x=DoY,y=NEE_daily),
            data=subset(daily_sum,Year==2015 & (DoY>=121 & DoY<=170 | DoY>178 & DoY<212)), colour="blue")+
  geom_line(aes(x=DoY,y=-GPP_daily),
            data=subset(daily_sum,Year==2015 & DoY>=121 & DoY<=212), colour="green")+
  geom_line(aes(x=DoY,y=Reco_daily),
            data=subset(daily_sum,Year==2015 & DoY>=121 & DoY<=212), colour="brown")+
  geom_hline(yintercept=0)+
  labs(y="Daily cumulative NEE gC/m2")+
  geom_vline(xintercept=175)+
  jorn15_scale()

# subset data for Anthony
daily_MJJ <- copy(daily_sum[Year==2015 & (DoY>=121 & DoY<=168 | DoY>=177 & DoY<=212),])

ggplot()+
  #geom_line(aes(x=DoY,y=NEE_daily), data=subset(daily_MJJ,Year==2015))+
  geom_line(aes(x=DoY,y=NEE_daily),
            data=subset(daily_MJJ,Year==2015 & (DoY>=121 & DoY<=170 | DoY>178 & DoY<212)), colour="blue")+
  geom_line(aes(x=DoY,y=-GPP_daily),
            data=subset(daily_MJJ,Year==2015 & DoY>=121 & DoY<=212), colour="green")+
  geom_line(aes(x=DoY,y=Reco_daily),
            data=subset(daily_MJJ,Year==2015 & DoY>=121 & DoY<=212), colour="brown")+
  geom_point(aes(x=DoY,y=NEE_daily),
            data=subset(daily_MJJ,Year==2015 & (DoY>=121 & DoY<=170 | DoY>178 & DoY<212)), colour="blue")+
  geom_point(aes(x=DoY,y=-GPP_daily),
            data=subset(daily_MJJ,Year==2015 & DoY>=121 & DoY<=212), colour="green")+
  geom_point(aes(x=DoY,y=Reco_daily),
            data=subset(daily_MJJ,Year==2015 & DoY>=121 & DoY<=212), colour="brown")+
  geom_hline(yintercept=0)+
  labs(y="Daily cumulative NEE gC/m2")+
  geom_vline(xintercept=175)+
  jorn15_scale()

# save the data as csv
# setwd("~/Desktop/TweedieLab/Projects/Jornada/Anthony_nutrients_fluxes/")
# write.table(daily_MJJ,file='JER_NEE_Reco_GPP_MJJ_2015_20190830.csv', sep=',', dec='.', row.names=FALSE)
