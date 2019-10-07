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

# import file as data table
setwd("~/Desktop/TweedieLab/Projects/Jornada/LTAR_Synthesis_Browning")

flux <- fread(file="FluxData_jerbajada_20190827.csv",
 sep=",", dec=".", header=TRUE, na.strings='-9999')

# convert date to POSIXct and get a year, day, hour column
flux[,':=' (date_time = parse_date_time(TIMESTAMP_END,"YmdHM",tz="UTC"),
            date_time_start = parse_date_time(TIMESTAMP_START,"YmdHM",tz="UTC"))][
  ,':='(Year_start = year(date_time_start),Year=year(date_time),DoY=yday(date_time),
        hours = hour(date_time), mins = minute(date_time))]

# format data columns for ReddyProc
# Year	DoY	Hour	NEE	LE	H	Rg	Tair	Tsoil	rH	VPD	Ustar 

flux[mins==0, Hour := hours+0.0]
flux[mins==30, Hour := hours+0.5]

edata <- flux[Year_start %in% c(2014,2015,2016),.(Year,
                 DoY,
                 Hour,
                 FC,
                 LE,
                 H,
                 SW_IN_1_1_1,
                 TA_1_1_1,
                 RH_1_1_1,
                 USTAR)]

setnames(edata,c("FC","SW_IN_1_1_1","TA_1_1_1","RH_1_1_1","USTAR"),
         c("NEE","Rg","Tair","rH","Ustar"))

# make all Rg<0 equal to 0 becuase ReddyProc won't accept values <0
# edata[Rg<0, Rg:=0]

# calculate VPD from rH and Tair in hPa (mbar), at > 10 hPa the light response curve parameters change
edata$VPD <- fCalcVPDfromRHandTair(edata$rH, edata$Tair)

EddyDataWithPosix <- edata %>% 
  fConvertTimeToPosix('YDH', Year = 'Year', Day = 'DoY', Hour = 'Hour')


EProc <- sEddyProc$new('JERbajada',EddyDataWithPosix,c('NEE','Rg','Tair','VPD',"Ustar"))

# set location and time zone for site to gap-fill Met data during partitioning
EProc$sSetLocationInfo(LatDeg = 32.5665, LongDeg = -106.6598, TimeZoneHour = -6)

# estimate Ustar thresholds (https://github.com/bgctw/REddyProc/blob/master/vignettes/useCase.md)
EProc$sEstimateUstarScenarios(
  nSample = 100L, probs = c(0.025, 0.05, 0.5, 0.95, 0.975))

# look at Ustar estimates
EProc$sGetEstimatedUstarThresholdDistribution()

# Ustar values
EProc$sGetUstarScenarios()

# use MDS for gap-filling
EProc$sMDSGapFillUStarScens('NEE')

# column names that denote Ustar uncertainty
grep("NEE_.*_f$",names(EProc$sExportResults()), value = TRUE)
grep("NEE_.*_fsd$",names(EProc$sExportResults()), value = TRUE)

#Look at NEE filtered at different Ustar levels
EProc$sPlotFingerprintY('NEE_U2.5_f', Year = 2015)
EProc$sPlotFingerprintY('NEE_U05_f', Year = 2015)
EProc$sPlotFingerprintY('NEE_U50_f', Year = 2015)
EProc$sPlotFingerprintY('NEE_U97.5_f', Year = 2015)


EProc$sPlotFingerprintY('NEE', Year = 2014)
EProc$sPlotFingerprintY('NEE_U50_f', Year = 2014)

EProc$sPlotFingerprintY('NEE', Year = 2015)
EProc$sPlotFingerprintY('NEE_U50_f', Year = 2015)

EProc$sPlotFingerprintY('NEE', Year = 2016)
EProc$sPlotFingerprintY('NEE_U50_f', Year = 2016)

# show ustar figure
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

fig_nee <- ggplot(subset(CombinedData,Year<2017), aes(DoY,NEE_U50_orig))+geom_line()+
  geom_point(aes(y=NEE_U50_f),data=subset(CombinedData, is.na(NEE_U50_orig)),colour="red",size=1)+
  facet_grid(.~Year)#+
  ylim(c(-10,10))

fig_reco <- ggplot(subset(CombinedData,Year<2017), aes(DoY,Reco_U50))+geom_line()+
  geom_line(aes(y=Reco_DT_U50),colour="blue")+
  facet_grid(.~Year)#+
  ylim(c(-10,10))

  # plot daytime Reco with qc code
  ggplot(subset(CombinedData,Year<2017), aes(DoY,Reco_DT_U50, colour=factor(FP_qc)))+
    geom_point(size=1)+
    facet_grid(.~Year)
  
fig_gpp <- ggplot(subset(CombinedData,Year<2017), aes(DoY,GPP_U50_f))+geom_line()+
  geom_line(aes(y=GPP_DT_U50),colour="blue")+
  facet_grid(.~Year)#+
  ylim(c(-10,10))

fig_vpd <- ggplot(subset(CombinedData,Year<2017), aes(DoY,VPD))+geom_line()+
geom_hline(yintercept=10, colour="red")+
  facet_grid(.~Year)
  
grid.arrange(fig_nee, fig_reco, fig_gpp, fig_vpd, nrow=4)

grid.arrange(fig_nee, fig_gpp, fig_vpd, nrow=2)
grid.arrange(fig_nee, fig_reco, fig_vpd, nrow=3)

# plot measured vs modeled NEE at U50
ggplot(subset(CombinedData,Year<2017), aes(NEE_U50_orig, NEE_U50_fall))+geom_point()+
geom_abline(intercept=0, slope=1)+
  facet_grid(.~Year)

# plot NEE with different U* quantiles
ggplot(subset(CombinedData,Year<2017), aes(x=DoY))+
  geom_line(aes(y=NEE_U2.5_f),colour="red")+
  geom_line(aes(y=NEE_U50_f))+
  geom_line(aes(y=NEE_U97.5_f),colour="red")

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


# calculate daily sums of Co2 flux in umol/m2/sec converted to gC/m2/day

daily_sum_dt <- as.data.table(subset(CombinedData,Year<2017))
daily_sum <- daily_sum_dt[,list(NEE_daily = sum(NEE_U50_f*1800*1*10^-6*12.01),
                                GPP_daily = sum(GPP_U50_f*1800*1*10^-6*12.01),
                                Reco_daily = sum(Reco_U50*1800*1*10^-6*12.01)), 
                          by="Year,DoY"]

annual_sum <- daily_sum[,list(NEE_annual = sum(NEE_daily)),
                        by="Year"]

ggplot(daily_sum, aes(DoY, NEE_daily))+
  geom_line(colour="blue")+
  geom_hline(yintercept=0)+
  labs(y="Daily cumulative NEE gC/m2")+
  facet_grid(.~Year)

ggplot(annual_sum, aes(Year,NEE_annual))+geom_point()

# look just at May - July in 2015
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
setwd("~/Desktop/TweedieLab/Projects/Jornada/Anthony_nutrients_fluxes/")
write.table(daily_MJJ,file='JER_NEE_Reco_GPP_MJJ_2015_20190830.csv', sep=',', dec='.', row.names=FALSE)
