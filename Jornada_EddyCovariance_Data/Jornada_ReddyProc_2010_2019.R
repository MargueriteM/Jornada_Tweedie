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

# import filtered flux data file from Eddy Pro as data table
# filtered in: Jornada_EddyPro_Output_Fluxnext_2010_2019.R
setwd("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_EddyPro_filtered")
load("JER_flux_2010_2018_EddyPro_Output_filtered_20190929.Rdata")


# convert date to POSIXct and get a year, day, hour column
flux_filter[,':=' (date_time = parse_date_time(TIMESTAMP_START,"YmdHM",tz="UTC"),
            date_time_end = parse_date_time(TIMESTAMP_END,"YmdHM",tz="UTC"))][
  ,':='(Year_end = year(date_time_end),Year=year(date_time),DoY=yday(date_time),
        hours = hour(date_time), mins = minute(date_time))]

# there's duplicated data in 2012 DOY 138
flux_filter <- (flux_filter[!(duplicated(flux_filter, by=c("TIMESTAMP_START")))])


# format data columns for ReddyProc
# Year	DoY	Hour	NEE	LE	H	Rg	Tair	Tsoil	rH	VPD	Ustar 

flux_filter[mins==0, Hour := hours+0.0]
flux_filter[mins==30, Hour := hours+0.5]

edata <- flux_filter[Year>2010,.(Year,
                 DoY,
                 Hour,
                 FC,
                 LE,
                 H,
                 SW_IN_1_1_1,
                 TA_1_1_1,
                 RH_1_1_1,
                 USTAR)]

ggplot(edata, aes(DoY,FC))+
  geom_line()+
  facet_grid(Year~.)

setnames(edata,c("FC","SW_IN_1_1_1","TA_1_1_1","RH_1_1_1","USTAR"),
         c("NEE","Rg","Tair","rH","Ustar"))

# make all Rg<0 equal to 0 becuase ReddyProc won't accept values <0
 edata[Rg<0, Rg:=0]

 # remove 2019 because that belongs to the following year
 # edata <- edata[Year!=2019,]
 
 
 # create a grid of full dates and times
 filled <- expand.grid(date=seq(as.Date("2011-01-01"),as.Date("2018-12-31"), "days"),
                       Hour=seq(0,23.5, by=0.5))
 filled$Year <- year(filled$date)
 filled$DoY <- yday(filled$date)
 
 filled$date <- NULL
 
 edata <- merge(edata,filled,by=c("Year","DoY","Hour"), all=TRUE)

  # and remove the 00:00 first time point in 2010. It's all NA and Reddyproc is getting upset
 # edata <- edata[!(Year==2010 & DoY == 1 & Hour == 0.0)]
 
 # convert edata to data frame for ReddyProc
 edata <- as.data.frame(edata)
 
 
# calculate VPD from rH and Tair in hPa (mbar), at > 10 hPa the light response curve parameters change
edata$VPD <- fCalcVPDfromRHandTair(edata$rH, edata$Tair)

EddyDataWithPosix <- edata %>% 
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

# show ustar figure
EProc$sPlotNEEVersusUStarForSeason(season = "2015001",
                                   format = "pdf", dir = "/Users/memauritz/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_EddyPro_filtered",
                                   UstarColName = "Ustar",
                                   NEEColName = "NEE",
                                   TempColName = "Tair",
                                   WInch = 16 * 0.394, HInchSingle = 6 * 0.394,
                                   data = cbind(EProc$sDATA, EProc$sTEMP,
                                                EProc$sUSTAR_DETAILS$bins[, c("uStarBin",
                                                                              "tempBin")]))

sEddyProc_sPlotNEEVersusUStarForSeason(season = "2015001",
                                       format = "pdf", dir = "/Users/memauritz/Desktop/TweedieLab/Projects/Jornada/LTAR_Synthesis_Browning",
                                       UstarColName = "Ustar",
                                       NEEColName = "NEE",
                                       TempColName = "Tair",
                                       WInch = 16 * 0.394, HInchSingle = 6 * 0.394,
                                       data = cbind(EProc$sDATA, EProc$sTEMP,
                                                    EProc$sUSTAR_DETAILS$bins[, c("uStarBin",
                                                                                  "tempBin")]))

plotNEEVersusUStarTempClass(NEEUStar.F = cbind(EProc$sDATA, EProc$sTEMP,
                                               EProc$sUSTAR_DETAILS$bins[, c("uStarBin",
                                                                             "tempBin")]),
                            uStarTh=0.105,
                            UstarColName = "Ustar",
                            NEEColName = "NEE",
                            TempColName = "Tair")


# use MDS for gap-filling
EProc$sMDSGapFillUStarScens('NEE')
EProc$sMDSGapFillUStarScens('NEE')


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

#setwd("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/ReddyProc/")
#combined_colnames <- colnames((fread('JER_2014_2016_ReddyProc_20190828.txt',header=TRUE))[1,])

#CombinedData <- fread('JER_2014_2016_ReddyProc_20190828.txt', dec=",",
#                      na.strings=c("-9999","-9999.0","NAN","#NAME?"),
#                      header=FALSE,skip=2,col.names=combined_colnames)


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
                                Reco_daily = sum(Reco_U50*1800*1*10^-6*12.01)), 
                          by="Year,DoY"]

daily_cum_sum <- daily_sum[,list(NEE_cum = cumsum(NEE_daily),
                                GPP_cum = cumsum(GPP_daily),
                                 Reco_cum = cumsum(Reco_daily)),
                           by="Year"]

annual_sum <- daily_sum[,list(NEE_annual = sum(NEE_daily)),
                        by="Year"]

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
