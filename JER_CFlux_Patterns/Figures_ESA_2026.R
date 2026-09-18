# combine and prep data for ESA presentation (July 2026)
# based on script for Figures_20230103_Manuscript_ScottCorrect

# Create R script to make manuscript figures for Bajada C Flux data
# add Scott et al 2015 corrected data from ReddyProc - gapfill & partition seperately


# use filtered and gap-filled NEE data

# load libraries
library(REddyProc)
library(data.table)
library(lubridate)
library(gridExtra)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggh4x) # hacks for ggplot
library(gtable)
library(grid)
library(zoo)
library(bit64)
library(viridis)
library(cowplot)
library(scales)
library(tidyr)
library(egg) # for tag_facet function to add a,b,c etc to individual facets in ggplot
library(bigleaf) # for ET conversion
library(forcats)
library(rstatix)
library(colorspace)
library(MuMIn)

# create a path for saving figures
figpath <- "/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/MauritzLab_Personal/Conferences/2026/ESA"

######
# custom function from tag_facet to add a,b,c plot labels to individual facets
# without removing the strip text
tag_facet_custom <- function (p, open = "(", close = ")", tag_pool = letters, x = -Inf, 
                              y = Inf, hjust = -0.5, vjust = 1.5, fontface = 2, family = "", 
                              ...) 
{
  gb <- ggplot_build(p)
  lay <- gb$layout$layout
  tags <- cbind(lay, label = paste0(open, tag_pool[lay$PANEL], 
                                    close), x = x, y = y)
  p + geom_text(data = tags, aes_string(x = "x", y = "y", label = "label"), 
                ..., hjust = hjust, vjust = vjust, fontface = fontface, 
                family = family, inherit.aes = FALSE)
}
######

# import 
# Read Reddyproc output data and compile into one file
# gapfill has qc==1 included in flux columns

# fread imports as data table
# list all files in relevant folder
REgapfiles <- list.files(path="~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/ReddyProc/20230115_ScottCorrect/Results_Gapfill",
                         full.names=TRUE, pattern = "output_USJo1_Gapfill") 

# read in column names and units
units.gap <-fread(REgapfiles[1],
                  header=TRUE)[1,]

# # after 2022 the data were processed in 2026 and I think ReddyProc changed the output format
# # columns range from 83-136. Have to read each file with own column names and then merge. 
# read_column_number <- function(colname){
#   ret <- ncol(fread(colname, sep="\t", dec=".", header=TRUE, skip=0)[1,])
#   obj_name <- tools::file_path_sans_ext(basename(colname))
#   out <- data.frame(file=obj_name, colnumber=ret)
#   out
# }
# 
# data1 <- plyr::ldply(REgapfiles, read_column_number)


# # read files and bind them into one file. fill=TRUE because of the missing columns in 2011
# won't work with files added in July 2026
# flux.gap <- do.call("rbind", lapply(REgapfiles, header = FALSE, fread, skip = 2,
#                                    na.strings=c("-9999", "NA","-"),
#                                    col.names=colnames(units.gap)))

# create list of files with column names read and assigned from each individual file
flux.list <- lapply(REgapfiles, function(f) {
  # Read just the header
  hdr <- names(fread(f, nrows = 0))
  # Read the data, skipping header + units row
  dt <- fread(
    f,
    skip = 2,
    header = FALSE,
    col.names = hdr,
    na.strings = c("-9999", "NA", "-")
  )
  dt
})

# combine files
flux.gap <- rbindlist(flux.list, fill = TRUE)

# for some reason na.strings won't recognize the -9999
flux.gap[flux.gap == -9999] <- NA

# quick graph to check import
# 2024 didn't produce NEE_U50_f. It didn't estimate u* quantiles
ggplot(flux.gap, aes(DoY, NEE_U50_f))+
  geom_line()+
  facet_grid(.~Year)

# quick graph to check import
ggplot(flux.gap, aes(DoY, NEE))+
  geom_line()+
  facet_grid(.~Year)

# get the 'edata' to add 2010 to the timeseries eventhough 2010 won't gap fill.... 
setwd("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/JER_Out_EddyPro_filtered")

# import data that was filtered by 3SD filter
# with Scott corrected column included 
load("JER_flux_2010_2022_EddyPro_FullOutput_filterSD_20230115.Rdata")
flux.2010.2022 <- copy(flux_filter_sd)
rm(flux_filter_sd)
load("JER_flux_202210_202405_EddyPro_FullOutput_filterSD_20240623.Rdata")
flux.2022.2024 <- copy(flux_filter_sd)
rm(flux_filter_sd)
load("JER_flux_202406_202512_EddyPro_FullOutput_filterSD_20260417.Rdata")
flux.2024.2025 <- copy(flux_filter_sd)
rm(flux_filter_sd)

# combine
flux_filter_sd <- rbind(flux.2010.2022[as.Date(date_time)<as.Date("2022-10-01"),], flux.2022.2024, flux.2024.2025,fill=TRUE)
rm(flux.2010.2022, flux.2022.2024, flux.2024.2025)

# convert date to POSIXct and get a year, day, hour column
# if this step doesn't work, make sure bit64 library is loaded otherwise the timestamps importa in a non-sensical format
flux_filter_sd[,':='(Year=year(date_time),DoY=yday(date_time),
                     hours = hour(date_time), mins = minute(date_time))]

# check there's no duplicated data
flux_filter <- (flux_filter_sd[!(duplicated(flux_filter_sd, by=c("date_time")))])

# format data columns for ReddyProc to match & merge
edata <- copy(flux_filter)

# Year	DoY	Hour	NEE	LE	H	Rg	Tair	Tsoil	rH	VPD	Ustar 
edata[mins==0, Hour := hours+0.0]
edata[mins==30, Hour := hours+0.5]

##### USE Scott et al 2015 corrected co2 flux !!! #######
edata[mins==0, Hour := hours+0.0]
edata[mins==30, Hour := hours+0.5]

edata <- edata[,.(Year,
                  DoY,
                  Hour,
                  fc_wpl_adjust,
                  LE,
                  H,
                  Rg_1_1_1,
                  Ta_1_1_1,
                  RH_1_1_1,
                  `u*`,
                  P_rain_1_1_1,
                  qc_co2_flux,
                  qc_LE,
                  qc_H)] # 23 June 2022: include P_RAIN to allow data processing with/without rain event split

setnames(edata,c("fc_wpl_adjust","Rg_1_1_1","Ta_1_1_1","RH_1_1_1","u*"),
         c("NEE","Rg","Tair","rH","Ustar"))

# make all Rg<0 equal to 0 becuase ReddyProc won't accept values <0
edata[Rg<0, Rg:=0]

# When running only 1 year & the complete year, remove max year because that belongs to the following year
#  edata <- edata[Year!=max(edata$Year),]

# create a grid of full dates and times
filled <- expand.grid(date=seq(as.Date("2010-01-01"),as.Date("2025-12-31"), "days"),
                      Hour=seq(0,23.5, by=0.5))
filled$Year <- year(filled$date)
filled$DoY <- yday(filled$date)

filled$date <- NULL

edata <- merge(edata,filled,by=c("Year","DoY","Hour"), all=TRUE)

# graph for quick check
ggplot(edata, aes(DoY,NEE))+
  geom_line()+
  facet_grid(Year~.)

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

# get only 2010 and go back to data table
edata2010 <- as.data.table(subset(edata,Year==2010))
#edata2020 <- as.data.table(subset(edata,Year==2020))
# edata2021 <- as.data.table(subset(edata,Year==2021))

flux.ep <- rbind(edata2010,flux.gap, fill=TRUE)

# check there's no duplicated data
flux.ep <- (flux.ep[!(duplicated(flux.ep, by=c("Year","DoY","Hour")))])


# save the data to have a compiled file easy to access
# setwd("~/Desktop/TweedieLab/Projects/Jornada/EddyCovariance/ReddyProc/20230115_ScottCorrect/")
# save(file="REddyResults_2010_2025_Compiled_ScottCorrect_Gap.Rdata",flux.ep)
# load("REddyResults_2010_2025_Compiled_ScottCorrect_Gap.Rdata")

# plot to check
# NEE_U95_f graph should have 2010 missing (and 2024)
ggplot(flux.ep, aes(DoY, NEE_U50_f))+geom_line()+facet_grid(.~Year)
# NEE graph should have all years present
ggplot(flux.ep, aes(DoY, NEE))+geom_line()+facet_grid(.~Year)

# calculate ET using big leaf function
flux.ep <- as.data.table(flux.ep)
flux.ep[,':='(ET.bl= LE.to.ET(LE,Tair))]

# calculate the percent of data gap-filled for NEE and ET
gapfill.perc.f.nee <- flux.ep[!is.na(NEE_U50_f),list(NEE_U50_count = length(NEE_U50_f)), 
                              by="Year"]
gapfill.perc.nee <- flux.ep[!is.na(NEE), list(NEE_count = length(NEE)), 
                            by="Year"]

gapfill.perc.f.et <- flux.ep[!is.na(LE_f),list(LE_f_count = length(LE_f)), 
                             by="Year"]
gapfill.perc.et <- flux.ep[!is.na(LE), list(LE_count = length(LE)), 
                           by="Year"]

gapfill.perc <- merge(gapfill.perc.nee,gapfill.perc.f.nee,all.x=TRUE)
gapfill.perc <- merge(gapfill.perc,gapfill.perc.et,all.x=TRUE)
gapfill.perc <- merge(gapfill.perc,gapfill.perc.f.et,all.x=TRUE)

# create column for total half hours in the entire year
# leap years: 2012, 2016, 2020, 2024
gapfill.perc[,total_count := c(17520,17520,17568,17520,17520,17520,17568,17520,17520,17520,17568,17520,17520,
                               17520,17568,17520)]

gapfill.perc <- gapfill.perc[,':='(NEE_meas_gap_perc = round(NEE_count/NEE_U50_count,2)*100,
                                   LE_meas_gap_perc = round(LE_count/LE_f_count,2)*100,
                                   NEE_unfilled_gap_perc = round(NEE_U50_count/total_count,2)*100,
                                   LE_unfilled_gap_perc = round(LE_f_count/total_count,2)*100)]

# Plot the measured and U50 gap-filled C flux data with precip
plot.nee.hh <- ggplot(flux.ep, aes(DoY,NEE))+
  geom_point(colour="#000000", size=0.17)+
  geom_point(aes(y=NEE_U50_f),data=subset(flux.ep, is.na(NEE)),colour="#808080",size=0.1)+
  facet_grid(.~Year)+
  ylim(c(-15,15))+
  scale_x_continuous(breaks =c(31,211,361),limits=c(1,367),
                     labels=c("Jan","Jul","D"),
                     minor_breaks =c(31,61,91,121,151,181,211,241,271,301,331,361),
                     guide="axis_minor",
                     expand=c(0,0))+
  labs(y=expression("NEE (μmol C" *O[2]*" "*m^-2* "se" *c^-1*")"),
       x="Month")+
  theme_bw(base_size=14)+
  theme(strip.background = element_blank(),
        # panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.length =  unit(-0.2,"cm"),
        ggh4x.axis.ticks.length.minor = rel(0.7))

plot.precip.hh <- ggplot(flux_filter_sd, aes(DoY, P_rain_1_1_1))+
  geom_hline(yintercept=0, color="darkgrey")+
  geom_line()+
  facet_grid(.~Year)+
  scale_x_continuous(breaks =c(31,211,361),limits=c(1,367),
                     labels=c("Jan","Jul","D"),
                     minor_breaks =c(31,61,91,121,151,181,211,241,271,301,331,361),
                     guide="axis_minor",
                     expand=c(0,0))+
  labs(y="Precipitation (mm)",
       x="Month")+
  theme_bw(base_size=14)+
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.length =  unit(-0.2,"cm"),
        ggh4x.axis.ticks.length.minor = rel(0.7),
        axis.text.x = element_text(vjust=-0.7))

plot_grid(plot.nee.hh,
          plot.precip.hh, nrow=2,
          labels="auto",
          align="v")

# Plot the measured and gap-filled LE data in mm
plot.et.d <- ggplot(flux.ep, aes(DoY,(LE/2454000)*1800))+
  geom_point(colour="#000000", size=0.17)+
  geom_point(aes(y=(LE_f/2454000)*1800),data=subset(flux.ep, is.na((LE/2454000)*1800)),
             colour="#808080",size=0.1)+
  scale_x_continuous(breaks =c(31,211,361),limits=c(1,367),
                     labels=c("Jan","Jul","D"),
                     expand=c(0,0))+
  labs(y=expression("Daily total ET (mm)"),
       x = "Month")+
  facet_grid(.~Year)+
  #ylim(c(-10,10))+
  theme_bw()+
  theme(strip.background = element_blank())

plot_grid(plot.et.d,
          plot.precip.hh, nrow=2,
          labels="auto",
          align="v")


# count consecutive NA values in NEE and LE and calculate the length of NA gaps to remove gap-filled data

# Generate unique group IDs for consecutive NA segments per Year
flux.ep[, na_grp_nee := rleid(is.na(NEE)), by = Year]
flux.ep[, na_grp_le := rleid(is.na(LE)), by = Year]

# Filter for NA rows, group by ID and the NA segment, then extract metrics of NA segment length and start/end timestamp
na_gaps_nee <- flux.ep[is.na(NEE), .(
  gap_length     = .N,
  start_time = min(`Date Time`),
  end_time   = max(`Date Time`)
), by = .(Year, na_grp_nee)]

View(na_gaps_nee[gap_length>672,])

na_gaps_le <- flux.ep[is.na(LE), .(
  gap_length     = .N,
  start_time = min(`Date Time`),
  end_time   = max(`Date Time`)
), by = .(Year, na_grp_le)]

View(na_gaps_le[gap_length>672,])

# -------------------------------------------------------------------------
# DEFINE THE DYNAMIC PURGE FUNCTION TO REMOVE NA GAPS > 672 HALF-HOURS (=48*14=2 WEEKS) FROM SPECIFIED COLUMNS
# -------------------------------------------------------------------------
remove_large_gaps <- function(target_dt, gaps_dt, gap_allowed, target_col) {
  
  # Filter the gaps table for streaks longer than 672
  large_gaps <- gaps_dt[gap_length > gap_allowed]
  
  # Ensure the target column is evaluated as a real/double type to avoid type warnings
  target_dt[, (target_col) := as.numeric(get(target_col))]
  
  # Use the 'env' argument to dynamically assign the target column name inside the join
  target_dt[
    large_gaps, 
    on = .(Year == Year, `Date Time` >= start_time, `Date Time` <= end_time), 
    env = list(col = target_col),
    col := NA_real_
  ]
  
  return(target_dt)
}

# apply remove large gaps to NEE_U50_f and LE_f
remove_large_gaps(flux.ep, na_gaps_nee, gap_allowed = 672, target_col = "NEE_U50_f")
remove_large_gaps(flux.ep, na_gaps_le, gap_allowed = 672, target_col = "LE_f")

# plot to see
ggplot(flux.ep[Year>2020,], aes(DoY,NEE))+
  geom_point(colour="#000000", size=0.17)+
  geom_point(aes(y=NEE_U50_f),data=subset(flux.ep, is.na(NEE)&Year>2020),colour="#808080",size=0.1)+
  facet_grid(.~Year)+
  ylim(c(-15,15))+
  scale_x_continuous(breaks =c(31,211,361),limits=c(1,367),
                     labels=c("Jan","Jul","D"),
                     minor_breaks =c(31,61,91,121,151,181,211,241,271,301,331,361),
                     guide="axis_minor",
                     expand=c(0,0))+
  labs(y=expression("NEE (μmol C" *O[2]*" "*m^-2* "se" *c^-1*")"),
       x="Month")+
  theme_bw(base_size=14)+
  theme(strip.background = element_blank(),
        # panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.length =  unit(-0.2,"cm"),
        ggh4x.axis.ticks.length.minor = rel(0.7))

ggplot(flux.ep[Year>2020,], aes(DoY,(LE/2454000)*1800))+
  geom_point(colour="#000000", size=0.17)+
  geom_point(aes(y=(LE_f/2454000)*1800),data=subset(flux.ep, is.na((LE/2454000)*1800)&Year>2020),
             colour="#808080",size=0.1)+
  scale_x_continuous(breaks =c(31,211,361),limits=c(1,367),
                     labels=c("Jan","Jul","D"),
                     expand=c(0,0))+
  labs(y=expression("ET (mm/30-min)"),
       x = "Month")+
  facet_grid(.~Year)+
  #ylim(c(-10,10))+
  theme_bw()+
  theme(strip.background = element_blank())


# calculate daily means and 7-day running means from gap-filled data
# Daily cummulative amount of carbon exchange (gC) and 7-day running mean

# calculate daily sums of Co2 flux in umol/m2/sec converted to gC/m2/day
# include 2010 data by calculating a daily mean and multiplying by seconds/day
daily_sum_dt <- as.data.table((flux.ep))
daily_sum_ec <- daily_sum_dt[,list(NEE_daily = sum(NEE_U50_f*1800*1*10^-6*12.01),
                                   NEE_daily_mean = mean(NEE, na.rm=TRUE)*86400*1*10^-6*12.01, # scale the mean to daily
                                  #GPP_daily = sum(GPP_U50_f*1800*1*10^-6*12.01),
                                  # Reco_daily = sum(Reco_U50*1800*1*10^-6*12.01),
                                   ET_daily = sum((LE_f/2454000)*1800), # (amount of energy to evaporate a unit weight of water; 2454000 J kg-1).
                                   ET_daily_mean = mean((LE/2454000),na.rm=TRUE)*86400,
                                   Tair_mean = mean(Tair),
                                   Tair_max=max(Tair),
                                   Tair_min=min(Tair),
                                   VPD_f_mean = mean(VPD_f),
                                   VPD_f_max=max(VPD_f),
                                   VPD_f_min=min(VPD_f),
                                   VPD_mean = mean(VPD),
                                   VPD_max=max(VPD),
                                   VPD_min=min(VPD)), 
                             by="Year,DoY"][,list(DoY,
                                                  NEE_daily, 
                                                  NEE_daily_mean,
                                                   #GPP_daily,
                                                   #Reco_daily,
                                                  ET_daily,
                                                  ET_daily_mean,
                                                  Tair_mean,
                                                  Tair_max,
                                                  Tair_min,
                                                  VPD_f_mean,
                                                  VPD_f_max,
                                                  VPD_f_min,
                                                  VPD_mean,
                                                  VPD_max,
                                                  VPD_min,
                                                  NEE_cum = cumsum(NEE_daily),
                                                  NEE_cum_mean = (cumsum(ifelse(is.na(NEE_daily_mean), 0, NEE_daily_mean)) + NEE_daily_mean*0),
                                                  # GPP_cum = cumsum(GPP_daily),
                                                  # Reco_cum = cumsum(Reco_daily),
                                                  ET_cum = cumsum(ET_daily),
                                                  ET_cum_mean = (cumsum(ifelse(is.na(ET_daily_mean), 0, ET_daily_mean)) + ET_daily_mean*0)),
                                            by="Year"]
# calculate mean nightime NEE ~ Reco and scale out to full day
daily_sum_night <- daily_sum_dt[Rg_f<5 | Rg<5,list(NEE_night_mean = mean(NEE, na.rm=TRUE)*86400*1*10^-6*12.01), # scale the mean to daily
                                            by="Year,DoY"]

daily_sum_ec <- left_join(daily_sum_ec,daily_sum_night)

# create a running mean 
daily_sum_ec[,':=' (NEE_daily_roll = rollmean(x=NEE_daily,
                                              k=7,
                                              fill=NA),
                    ET_daily_roll = rollmean(x=ET_daily,
                                             k=7,
                                             fill=NA))]

# add definition of hydrological year (1 Nov - 31 Oct)
# winter: 1 Nov - 31 May (not using)
# summer: 1 Jun - 31 Oct (not using)
# Based on Biederman et al 2018: https://www.tucson.ars.ag.gov/unit/publications/PDFfiles/2394.pdf 

# seasons
daily_sum_ec[DoY>=1 & DoY<=90, ':=' (season="Winter", seasonlength=(90-1)+1)] # 1 Nov - 31 Mar (~Mesquite senesence - Mesquite budbreak)
daily_sum_ec[DoY>90 & DoY<166, ':=' (season="Pre-Monsoon", seasonlength=(165-91)+1)] # 1 Apr - 14 Jun
daily_sum_ec[DoY>=166 & DoY<=273, ':=' (season="Monsoon", seasonlength=(273-166)+1)] # 15 June - 31 Sep
daily_sum_ec[DoY>273 & DoY<=304, ':=' (season="Post-Monsoon", seasonlength=(304-274)+1)] # 1 Oct - 31 Oct .... still hot but no longer monsoon definition?
daily_sum_ec[DoY>304 & DoY<=366, ':=' (season="Winter")] # 1 Nov - 31 Mar (~Mesquite senesence - Mesquite budbreak)

daily_sum_ec[, season := factor(season, levels=c("Winter","Pre-Monsoon","Monsoon","Post-Monsoon"))]

# hydrological year: 1 Nov - 31 Oct
daily_sum_ec[DoY>=1 & DoY<305, Hydroyear:=Year] # 1 Jan - 31 Oct
daily_sum_ec[DoY>=305 & DoY<=366, Hydroyear:=Year+1] # 1 Nov - 31 Dec

# create a hydro DoY
# leap years: 2012, 2016, 2020, 2024
leapyears <- c(2012L,2016L,2020L,2024L)

daily_sum_ec[Year %in% leapyears & (DoY>=305 & DoY<=366), DoYh := DoY - 304]
daily_sum_ec[Year %in% leapyears & (DoY>=1 & DoY<305), DoYh := DoY + 62]
daily_sum_ec[!(Year %in% leapyears) & (DoY>=305 & DoY<=365), DoYh := DoY - 304]
daily_sum_ec[!(Year %in% leapyears) & (DoY>=1 & DoY<305), DoYh := DoY + 61]

# add seasonlength for winter
daily_sum_ec[Year %in% leapyears & season %in% "Winter", seasonlength := (366-305)+1]
daily_sum_ec[!(Year %in% leapyears) & season %in% "Winter", seasonlength := (365-305)+1]

# graphically check hydroyear definitions
ggplot(daily_sum_ec, aes(DoY, NEE_daily_mean, color=factor(Hydroyear)))+
  geom_line()+
  facet_grid(.~Year,scales="free_x")

# graphically check season definitions
ggplot(daily_sum_ec, aes(DoYh, NEE_daily_mean, color=season))+
  geom_line()+
  facet_grid(.~Year,scales="free_x")

# graphically check hydroyear with adjusted DoY and season definitions
ggplot(daily_sum_ec, aes(DoYh, NEE_daily_mean, color=season))+
  geom_line()+
  facet_grid(Hydroyear~.,scales="free_x")

# plot season lengths
ggplot(daily_sum_ec, aes(season, seasonlength))+geom_point()

# graph NEE, GPP, Reco
ggplot(daily_sum_ec, aes(x=DoYh))+
  geom_line(aes(y=NEE_daily_mean,color="NEE"))+
  geom_line(aes(y=GPP_daily,color="GPP"))+
  geom_line(aes(y=Reco_daily,color="Reco"))+
  facet_grid(.~Year,scales="free_x")


# look at the gap-filled daily sums vs the mean calculation for all years
daily.comp.co2 <- ggplot(daily_sum_ec,aes(x=NEE_daily, y=NEE_daily_mean))+
  geom_point()+
  geom_abline(slope=1,intercept=0)+
  geom_smooth(method="lm")+
  labs(x = "Daily Cummulative NEE (ReddyProc gap-filled)",
       y = "Daily Cummulative NEE (scaled from mean daily flux) ")+
  annotate("text", x=0, y=3, label = expression("y = -0.02 + 1.09x, " *R^2*" = 0.93"))+
  theme_bw()+
  theme(axis.ticks.length =  unit(-0.2,"cm"))+
  lims(x=c(-2,3),y=c(-2,3))

lm.daily.co2 <- lm(daily_sum_ec$NEE_daily_mean~daily_sum_ec$NEE_daily)
summary(lm.daily.co2)

daily.comp.et <- ggplot(daily_sum_ec,aes(x=ET_daily, y=ET_daily_mean))+
  geom_point()+
  geom_abline(slope=1,intercept=0)+
  geom_smooth(method="lm", alpha=0)+
  labs(x = "Daily Cummulative ET (ReddyProc gap-filled)",
       y = "Daily Cummulative ET (scaled from mean daily flux) ")+
  annotate("text", x=3, y=8, label = expression("y = -0.03 + 1.13x, " *R^2*" = 0.92"))+
  theme_bw()+
  theme(axis.ticks.length =  unit(-0.2,"cm"))+
  lims(x=c(-1,8),y=c(-1,8))


lm.daily.et <- lm( daily_sum_ec$ET_daily_mean~daily_sum_ec$ET_daily)
summary(lm.daily.et)


# timeseries
daily.comp.co2.ts <- ggplot(daily_sum_ec)+
  geom_point(aes(x=DoY, y=NEE_daily),colour="darkgrey",size=0.8)+
  geom_point(aes(x=DoY, y=NEE_daily_mean),colour="lightblue",size=0.5)+
  facet_grid(.~Year)+
  labs(y = "Daily Cummulative NEE", x="Day Of Year")+
  theme_bw()+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.length =  unit(-0.2,"cm"),
        strip.background = element_blank())

daily.comp.et.ts <-ggplot(daily_sum_ec)+
  geom_point(aes(x=DoY, y=ET_daily),colour="darkgrey",size=0.8)+
  geom_point(aes(x=DoY, y=ET_daily_mean),colour="lightblue",size=0.5)+
  facet_grid(.~Year)+
  labs(y = "Daily Cummulative ET",x="Day Of Year")+
  theme_bw()+
  theme(axis.ticks.length =  unit(-0.2,"cm"),
        strip.background = element_blank())

plot_grid(daily.comp.co2.ts, daily.comp.et.ts, nrow=2)

# add a date variable to daily_sum_ec
daily_sum_ec[Year==2010,date:= as.Date(DoY-1, origin = "2010-01-01")]
daily_sum_ec[Year==2011,date:= as.Date(DoY-1, origin = "2011-01-01")]
daily_sum_ec[Year==2012,date:= as.Date(DoY-1, origin = "2012-01-01")]
daily_sum_ec[Year==2013,date:= as.Date(DoY-1, origin = "2013-01-01")]
daily_sum_ec[Year==2014,date:= as.Date(DoY-1, origin = "2014-01-01")]
daily_sum_ec[Year==2015,date:= as.Date(DoY-1, origin = "2015-01-01")]
daily_sum_ec[Year==2016,date:= as.Date(DoY-1, origin = "2016-01-01")]
daily_sum_ec[Year==2017,date:= as.Date(DoY-1, origin = "2017-01-01")]
daily_sum_ec[Year==2018,date:= as.Date(DoY-1, origin = "2018-01-01")]
daily_sum_ec[Year==2019,date:= as.Date(DoY-1, origin = "2019-01-01")]
daily_sum_ec[Year==2020,date:= as.Date(DoY-1, origin = "2020-01-01")]
daily_sum_ec[Year==2021,date:= as.Date(DoY-1, origin = "2021-01-01")]
daily_sum_ec[Year==2022,date:= as.Date(DoY-1, origin = "2022-01-01")]
daily_sum_ec[Year==2023,date:= as.Date(DoY-1, origin = "2023-01-01")]
daily_sum_ec[Year==2024,date:= as.Date(DoY-1, origin = "2024-01-01")]
daily_sum_ec[Year==2025,date:= as.Date(DoY-1, origin = "2025-01-01")]


# calculate daily and cumulative precip
precip_daily <- flux_filter[,date:=as.Date(date_time)][!is.na(P_rain_1_1_1),
                                                       list(precip.tot = sum(P_rain_1_1_1)),
                                                       by="date,Year"][
                                                         Year>2010,precip.cum:=cumsum(precip.tot),by="Year"][
                                                           Year==2010, precip.cum := (cumsum(ifelse(is.na(precip.tot), 0, precip.tot)) + precip.tot*0)]



# combine daily fluxes with daily precip
daily_sum <- full_join(daily_sum_ec,precip_daily)

# re-add hydrological year: 1 Nov - 31 Oct
daily_sum[is.na(Hydroyear) & yday(date)>=1 & yday(date)<305, Hydroyear:=Year] # 1 Jan - 31 Oct
daily_sum[is.na(Hydroyear) & yday(date)>=305 & yday(date)<=366, Hydroyear:=Year+1] # 1 Nov - 31 Dec

# create a hydro DoY
# leap years: 2012, 2016, 2020, 2024
daily_sum[is.na(DoYh) & Year %in% leapyears & (yday(date)>=305 & yday(date)<=366), DoYh := DoY - 304]
daily_sum[is.na(DoYh) & Year %in% leapyears & (yday(date)>=1 & yday(date)<305), DoYh := DoY + 62]
daily_sum[is.na(DoYh) & !(Year %in% leapyears) & (yday(date)>=305 & yday(date)<=365), DoYh := DoY - 304]
daily_sum[is.na(DoYh) & !(Year %in% leapyears) & (yday(date)>=1 & yday(date)<305), DoYh := DoY + 61]


# seasons
daily_sum[DoY>=1 & DoY<=90, ':=' (season="Winter", seasonlength=(90-1)+1)] # 1 Nov - 31 Mar (~Mesquite senesence - Mesquite budbreak)
daily_sum[DoY>90 & DoY<166, ':=' (season="Pre-Monsoon", seasonlength=(165-91)+1)] # 1 Apr - 14 Jun
daily_sum[DoY>=166 & DoY<=273, ':=' (season="Monsoon", seasonlength=(273-166)+1)] # 15 June - 31 Sep
daily_sum[DoY>273 & DoY<=304, ':=' (season="Post-Monsoon", seasonlength=(304-274)+1)] # 1 Oct - 31 Oct .... still hot but no longer monsoon definition?
daily_sum[DoY>304 & DoY<=366, ':=' (season="Winter")] # 1 Nov - 31 Mar (~Mesquite senesence - Mesquite budbreak)

daily_sum[, season := factor(season, levels=c("Winter","Pre-Monsoon","Monsoon","Post-Monsoon"))]

# add seasonlength for winter
daily_sum[Year %in% leapyears & season %in% "Winter", seasonlength := (366-305)+1]
daily_sum[!(Year %in% leapyears) & season %in% "Winter", seasonlength := (365-305)+1]

# graph daily precip
ggplot(daily_sum, aes(DoYh, precip.tot,color=season))+
  geom_line()+
  facet_grid(.~Hydroyear)

# add a year label to day 365 of each year
daily_sum <- daily_sum[,year_lab := ifelse(yday(date)==360, Year, NA)]

# calculate cumulative sums by hydroyear

# move NEE_daily (from NEE_U50) and NEE_daily_mean (from NEE) to same column for cumulative calculations
daily_sum[Year %in% c(2010),':=' (NEE_daily_calcs = NEE_daily_mean,
                                  NEE_night_calcs = NEE_night_mean,
                                  ET_daily_calcs = ET_daily_mean,
                                  VPD_mean_calcs = VPD_mean,
                                  VPD_min_calcs = VPD_min,
                                  VPD_max_calcs = VPD_max)][
  Year>2010,':=' (NEE_daily_calcs = NEE_daily,
                  NEE_night_calcs = NEE_night_mean,
                  ET_daily_calcs = ET_daily,
                  VPD_mean_calcs = VPD_f_mean,
                  VPD_min_calcs = VPD_f_min,
                  VPD_max_calcs = VPD_f_max)][
  Year %in% c(2024),':=' (NEE_daily_calcs = NEE_daily_mean)]

annual_na_nee <- daily_sum[is.na(NEE_daily_calcs),list(NA.count.nee = .N),by="Hydroyear"]
annual_na_et <- daily_sum[is.na(ET_daily_calcs),list(NA.count.et = .N),by="Hydroyear"]
annual_na_precip <- daily_sum[is.na(precip.tot),list(NA.count.precip = .N),by="Hydroyear"]
annual_days_precip <- daily_sum[precip.tot>0,list(day.count.precip = .N),by="Hydroyear"]

annual_cum <- daily_sum[
  ,list(NEE_cum.ann = (sum(ifelse(is.na(NEE_daily_calcs), 0, NEE_daily_calcs))),
                              ET_cum.ann = (sum(ifelse(is.na(ET_daily_calcs), 0, ET_daily_calcs))),
                              precip_cum.ann = (sum(ifelse(is.na(precip.tot), 0, precip.tot))),
                              temp_mean.ann = mean(Tair_mean, na.rm=TRUE),
                              temp_min.ann = mean(Tair_min, na.rm=TRUE),
                              temp_max.ann = mean(Tair_max, na.rm=TRUE)),
                        by="Hydroyear"]

annual_cum <- left_join(annual_cum,annual_na_nee)
annual_cum <- left_join(annual_cum,annual_na_et)
annual_cum <- left_join(annual_cum,annual_na_precip)
annual_cum <- left_join(annual_cum,annual_days_precip)

# add source/sink column to color
annual_cum[NEE_cum.ann>0 , source_sink := "Source"]
annual_cum[NEE_cum.ann<=0 , source_sink := "Sink"]

# exclude hydro year 2026 and years with more than 36 NA
# graph some annuals just to check
p10 <- ggplot(annual_cum[(NA.count.nee<36 | is.na(NA.count.nee)) &  Hydroyear<2026,],
                     aes(Hydroyear, NEE_cum.ann, fill=source_sink))+
  geom_col(stat="identity")+
  #geom_text(aes(label=NA.count.nee))+
  geom_hline(yintercept=0)+
  xlim(2009,2026)+
  geom_hline(yintercept = 0,
                             linewidth = 0.5,
                             color = "grey40") +
  scale_fill_manual(values = c("Sink" = "green4", "Source" = "salmon4"), name="") +
  labs(y=expression("Annual NEE (gC" *m^-2*")"), x="Hydroyear")+
  theme_bw(base_size=20)+
  theme(
    # Direction: Negative length forces ticks inside the plot area
    axis.ticks.length = unit(-0.15, "cm"),
    
    # Padding: Push text away from the new inward ticks to avoid overlap
    axis.text.x = element_text(margin = margin(t = 0.3, unit = "cm")),
    axis.text.y = element_text(margin = margin(r = 0.3, unit = "cm"))
  )

# annual precip
# from daily graph above, exclude years: 2010, 2011, 2026
pl.ann.precip <- ggplot(annual_cum[!(Hydroyear %in% c(2010, 2011 ,2026))&!is.na(Hydroyear),], aes(Hydroyear, precip_cum.ann))+
  geom_col(stat="identity")+
  geom_hline(yintercept=241)+
  xlim(2009,2026)


p10 + coord_flip()

# annual temperature
# from daily graph above, exclude years: 2010, 2011, 2026
ggplot(annual_cum[!(Hydroyear %in% c(2010, 2017, 2018, 2026))&!is.na(Hydroyear),], aes(Hydroyear, temp_mean.ann))+
  geom_point()+
  geom_line()+
  xlim(2009,2026)

# calculate seasonal cumulative by hydroyear
seasonal_cum <- daily_sum[
  ,list(seasonlength = mean(seasonlength),
        NEE_cum = (sum(ifelse(is.na(NEE_daily_calcs), 0, NEE_daily_calcs))),
        ET_cum = (sum(ifelse(is.na(ET_daily_calcs), 0, ET_daily_calcs))),
        precip_cum = (sum(ifelse(is.na(precip.tot), 0, precip.tot))),
        temp_mean = mean(Tair_mean, na.rm=TRUE),
        temp_min = mean(Tair_min, na.rm=TRUE),
        temp_max = mean(Tair_max, na.rm=TRUE),
        VPD_mean = mean(VPD_mean_calcs, na.rm=TRUE),
        VPD_max=mean(VPD_max_calcs, na.rm=TRUE),
        VPD_min=mean(VPD_min_calcs, na.rm=TRUE)),
  by="Hydroyear,season"]

seasonal_na_nee <- daily_sum[is.na(NEE_daily_calcs),list(NA.count.nee.s = .N),by="Hydroyear,season"]
seasonal_na_et <- daily_sum[is.na(ET_daily_calcs),list(NA.count.et.s = .N),by="Hydroyear,season"]
seasonal_na_precip <- daily_sum[is.na(precip.tot),list(NA.count.precip.s = .N),by="Hydroyear,season"]
seasonal_days_precip <- daily_sum[precip.tot>0,list(day.count.precip.s = .N),by="Hydroyear,season"]

# add counts of NAs and number of rain days to seasonal
seasonal_cum <- left_join(seasonal_cum, seasonal_na_nee)
seasonal_cum <- left_join(seasonal_cum, seasonal_na_et)
seasonal_cum <- left_join(seasonal_cum, seasonal_na_precip)
seasonal_cum <- left_join(seasonal_cum, seasonal_days_precip)

# calculate rainfall intensity: precip_cum/day.count.precip.s
seasonal_cum[,precip_intensity := precip_cum/day.count.precip.s]

# calculate na count/seasonlength
seasonal_cum[, ":=" (na_nee_prop = NA.count.nee.s/seasonlength,
                     na_et_prop = NA.count.et.s/seasonlength,
                     na_precip_prop = NA.count.precip.s/seasonlength)]

# create wide format for season
seasonal_cum_wide <- seasonal_cum %>%
  select(Hydroyear, season, NEE_cum, ET_cum, precip_cum, temp_mean, temp_max, temp_min, VPD_mean, VPD_max, VPD_min,
         precip_intensity, na_nee_prop, na_et_prop, na_precip_prop) %>%
  pivot_wider(names_from = season,
              names_sep="_",
              values_from = c(NEE_cum, ET_cum, precip_cum, temp_mean, temp_max, temp_min, VPD_mean, VPD_max, VPD_min,
                              precip_intensity, na_nee_prop, na_et_prop, na_precip_prop))

# join annual cum and seasonal wide, if needed
 seasonal_ann_cum_wide <- left_join(annual_cum, seasonal_cum_wide)

# add annual cum to seasonal cum
seasonal_ann_cum <- left_join(seasonal_cum, annual_cum, relationship = "many-to-many")

# graph daily seasonals

# define season colors
season_cols <- c(
  "Winter" = "#4C78A8",
  "Pre-Monsoon" = "#C8A165",
  "Monsoon" = "#2A9D55",
  "Post-Monsoon" = "#C65D2E")

season_outline <- lighten(season_cols, amount = 0.35)

# NEE

# as mean
p3<-  daily_sum %>%
    group_by(DoYh)%>%
    summarise(NEE_mean = mean (NEE_daily_calcs, na.rm=TRUE),
              NEE_sd = sd (NEE_daily_calcs, na.rm=TRUE),
              season=unique(season))%>%
    ggplot(., aes(x=DoYh, y=NEE_mean, color=season, fill=season))+
    geom_ribbon(aes(ymin=NEE_mean-NEE_sd, ymax=NEE_mean+NEE_sd),alpha = 0.18, color = NA) +
  geom_line(linewidth = 1.2) +
  geom_hline(yintercept = 0,
             linewidth = 0.5,
             color = "grey40") +
    scale_color_manual(values = season_cols) +
    scale_fill_manual(values = season_cols) +
    scale_x_continuous(
      breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
      labels = month.abb[c(11,12,1:10)],
      expand = c(0,0)
    )+
    labs(y=expression("Mean Daily NEE (gC" *m^-2*")"), x="Month")+
    theme_bw(base_size=16)
  
# save
ggsave(plot=p3+ theme(legend.position = "none"), filename="Fig_NEE_seasonal.pdf", path=figpath, width=20, height=5, units="in", scale=0.78)


# night-time NEE
ggplot(daily_sum, aes(factor(DoYh), NEE_night_calcs))+
  geom_boxplot()+
  geom_hline(yintercept=0, linewidth=1.5)


# rain and temperature combined
p1 <- daily_sum %>%
  group_by(DoYh)%>%
  summarise(precip_mean = mean (precip.tot, na.rm=TRUE),
            precip_sd = sd (precip.tot, na.rm=TRUE),
            temp_mean = mean (Tair_mean, na.rm=TRUE),
            temp_sd = sd (Tair_mean, na.rm=TRUE),
            season=unique(season))%>%
  ggplot(., aes(x=DoYh, color=season, fill=season))+
  geom_col(aes(y=precip_mean, fill=season),color="white",linewidth=0.1) +
  geom_errorbar(aes(ymin=precip_mean, ymax=precip_mean+precip_sd), linewidth=0.2) +
  geom_line(aes(y=temp_mean/2),linewidth=1) +
  geom_ribbon(aes(ymin=(temp_mean-temp_sd)/2, ymax=(temp_mean+temp_sd)/2), alpha=0.18, color=NA) +
  scale_color_manual(values = season_cols, name="Season") +
  scale_fill_manual(values = season_cols,  name="Season") +
  scale_x_continuous(
    breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
    labels = month.abb[c(11,12,1:10)],
    expand = c(0,0)
  )+
  labs(x="Month")+
  scale_y_continuous(name="Mean Daily Rainfall (mm)",
                     sec.axis=sec_axis(~.*2, name=expression("Mean Daily Air Temperature ("~degree~"C)")))+
  theme_bw(base_size=16)


# VPD time series
p2 <- daily_sum %>%
  group_by(DoYh)%>%
  summarise(VPD_mean = mean (VPD_mean_calcs, na.rm=TRUE),
            VPD_sd = sd (VPD_mean_calcs, na.rm=TRUE),
            season=unique(season))%>%
  ggplot(., aes(x=DoYh, color=season, fill=season))+
  geom_line(aes(y=VPD_mean),linewidth=1) +
  geom_ribbon(aes(ymin=(VPD_mean-VPD_sd), ymax=(VPD_mean+VPD_sd)), alpha=0.18, color=NA) +
  scale_color_manual(values = season_cols, name="Season") +
  scale_fill_manual(values = season_cols,  name="Season") +
  scale_x_continuous(
    breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
    labels = month.abb[c(11,12,1:10)],
    expand = c(0,0)
  )+
  labs(y="Mean Daily VPD", x="Month")+
  theme_bw(base_size=16)


# graph rain/temp and VPD time series together
grid.arrange(p1 + theme(legend.position = "none"),
             p2 + theme(legend.position = "none"),
             nrow=2)

# save figures
ggsave(plot=p1+ theme(legend.position = "none"), filename="Fig_Rain_Temp_seasonal.pdf", path=figpath, width=20, height=5, units="in", scale=0.78)
ggsave(plot=p2+ theme(legend.position = "none"), filename="Fig_VPD_seasonal.pdf", path=figpath, width=20, height=5, units="in", scale=0.78)


# daily uWUE NEE*sprtVPD/ET
ggplot(daily_sum, aes(DoYh, (NEE_daily_calcs*sqrt(VPD_mean_calcs))/ET_daily_calcs,color=factor(season)))+
  geom_line(linewidth = 0.4)+
  #facet_grid(.~Hydroyear)+
  ylim(-10,10)


# graph seasonal cumualtives

# NEE stack seasonal bars - drop years without annual budget
# if selecting 75% full seasons: na_nee_prop<0.25 | is.na(na_nee_prop)
p9 <- seasonal_ann_cum %>%
  #mutate(season_stack = fct_reorder(season, desc(NEE_cum)))%>%
  filter((NA.count.nee<36 | is.na(NA.count.nee)) & Hydroyear<2026) %>%
ggplot(., aes(Hydroyear,NEE_cum,fill=season))+
  geom_col(stat="identity")+
  geom_hline(yintercept = 0,
             linewidth = 0.5,
             color = "grey40") +
  xlim(c(2009,2026))+
  labs(x="Hydrological Year", y=expression("NEE (gC" *m^-2*")"))+
  scale_fill_manual(values = season_cols, name="Season") +
  coord_flip()+
  theme_bw(base_size = 20)+
  theme(
    # Direction: Negative length forces ticks inside the plot area
    axis.ticks.length = unit(-0.15, "cm"),
    
    # Padding: Push text away from the new inward ticks to avoid overlap
    axis.text.x = element_text(margin = margin(t = 0.3, unit = "cm")),
    axis.text.y = element_text(margin = margin(r = 0.3, unit = "cm"))
  )

# NEE seasonal boxplot
p4 <- seasonal_ann_cum %>%
  #mutate(season_stack = fct_reorder(season, desc(NEE_cum)))%>%
  filter(na_nee_prop<0.25 | is.na(na_nee_prop)) %>%
  ggplot(., aes(season,NEE_cum,fill=season))+
  geom_boxplot()+
  geom_hline(yintercept=0)+
  labs(y=expression("Seasonal NEE (gC" *m^-2*")"), x="Season")+
  scale_fill_manual(values = season_cols) +
  coord_flip()+
  theme_bw(base_size = 16)

# save
ggsave(plot=p4+ theme(legend.position = "none",
                      axis.title.y = element_blank()), filename="Fig_NEE_seasonal_box.pdf", path=figpath, width=5, height=3, units="in", scale=0.78)


# NEE/day seasonal boxplot
seasonal_ann_cum %>%
  #mutate(season_stack = fct_reorder(season, desc(NEE_cum)))%>%
  filter(na_nee_prop<0.25 | is.na(na_nee_prop)) %>%
  ggplot(., aes(season,NEE_cum/seasonlength,fill=season))+
  geom_boxplot()+
  geom_hline(yintercept=0)


# ET stack seasonal bars
seasonal_ann_cum %>%
  mutate(season_stack = fct_reorder(season, desc(ET_cum)))%>%
  filter(na_et_prop<0.25 | is.na(na_et_prop)) %>%
  ggplot(., aes(factor(Hydroyear),ET_cum,fill=season))+
  geom_col(stat="identity")+
  coord_flip()

# ET seasonal boxplot
seasonal_ann_cum %>%
  #mutate(season_stack = fct_reorder(season, desc(NEE_cum)))%>%
  #filter((NA.count.nee<36 | is.na(NA.count.nee)) & Hydroyear<2026) %>%
  filter(na_et_prop<0.25 | is.na(na_et_prop)& Hydroyear<2026) %>%
  ggplot(., aes(season,ET_cum,fill=season))+
  geom_boxplot()+
  coord_flip()

# precip stack seasonal bars
p8 <- seasonal_ann_cum %>%
  #mutate(season_stack = fct_reorder(season, desc(NEE_cum)))%>%
  #filter((NA.count.nee<36 | is.na(NA.count.nee)) & Hydroyear<2026) %>%
  #filter((na_precip_prop<0.25 | is.na(na_precip_prop)) & Hydroyear<2026) %>%
  filter((Hydroyear>2011 & Hydroyear<2026)) %>%
  ggplot(., aes(Hydroyear,precip_cum,fill=season))+
  geom_col(stat="identity")+
  geom_hline(yintercept=240, linetype="dashed", color="grey40", linewidth=0.6)+
  xlim(c(2009,2026))+
  labs(x="Hydrological Year", y="Total Rainfall (mm)")+
  scale_fill_manual(values = season_cols, name="Season") +
  coord_flip()+
  theme_bw(base_size = 20)+
  theme(
    # Direction: Negative length forces ticks inside the plot area
    axis.ticks.length = unit(-0.15, "cm"),
    
    # Padding: Push text away from the new inward ticks to avoid overlap
    axis.text.x = element_text(margin = margin(t = 0.3, unit = "cm")),
    axis.text.y = element_text(margin = margin(r = 0.3, unit = "cm"))
  )

# plot NEE vs precip and ET side-by-side
p8_9 <- plot_grid(p8 + theme(legend.position="none"),
                     p9+ theme(legend.position="none", axis.title.y=element_blank(), axis.text.y=element_blank()),
                     ncol=2,
                     align="h")

# save
ggsave(plot=p8_9, filename="Fig_rain_nee_season_stack.pdf", path=figpath, width=14, height=7, units="in", scale=0.78)


# add annual NEE as third panel
p8_9_10 <- plot_grid(p8 + theme(legend.position="none"),
                  p9+ theme(legend.position="none", axis.title.y=element_blank(), axis.text.y=element_blank()),
                  p10+ coord_flip() +theme(legend.position="none", axis.title.y=element_blank(), axis.text.y=element_blank()),
                  ncol=3,
                  align="h")

# save
ggsave(plot=p8_9_10, filename="Fig_rain_nee_season_stack_annual.pdf", path=figpath, width=28, height=14, units="in", scale=0.5)

# precip seasonal boxplot
p5 <- seasonal_ann_cum %>%
  #mutate(season_stack = fct_reorder(season, desc(NEE_cum)))%>%
  #filter((NA.count.nee<36 | is.na(NA.count.nee)) & Hydroyear<2026) %>%
  filter(na_precip_prop<0.25 | is.na(na_precip_prop)& Hydroyear<2026) %>%
  ggplot(., aes(season,precip_cum,fill=season))+
  geom_boxplot()+
  labs(y="Seasonal Rainfall (mm)", x="Season")+
  scale_fill_manual(values = season_cols) +
  coord_flip()+
  theme_bw(base_size = 16)


# save
ggsave(plot=p5+ theme(legend.position = "none",
                      axis.title.y = element_blank()), filename="Fig_precip_seasonal_box.pdf", path=figpath, width=5, height=3, units="in", scale=0.78)



# look at relationship between annual and seasonal cumulative
ggplot(seasonal_ann_cum[(NA.count.nee<36 | is.na(NA.count.nee)) & Hydroyear<2026,],aes(ET_cum/seasonlength, NEE_cum.ann,color=season))+
  geom_point()+
  geom_hline(yintercept=0)+
  geom_smooth(method="lm")

ggplot(seasonal_ann_cum[(NA.count.nee<36 | is.na(NA.count.nee)) & Hydroyear<2026,],aes(precip_cum, NEE_cum.ann,color=season))+
  geom_point()+
  geom_hline(yintercept=0)+
  geom_smooth(method="lm")

# seasonal precip and annual ET
ggplot(seasonal_ann_cum[(NA.count.et<36 | is.na(NA.count.et)) & Hydroyear<2026,],aes(precip_cum, ET_cum.ann,,color=season))+
  geom_point()+
  geom_hline(yintercept=0)+
  geom_smooth(method="lm")

# look at relationship within seasonal cumulative
# ET, NEE
p7 <- ggplot(seasonal_ann_cum[na_nee_prop<0.25 | is.na(na_nee_prop),],aes(ET_cum, NEE_cum))+
  geom_abline(intercept = mod.seas.et.intercept, slope = mod.seas.et.slope, color="#C8A165", 
              size=0.7)+
  geom_point(aes(color=season, fill=season), size=4, shape=21)+
  geom_hline(yintercept=0)+
  scale_fill_manual(values=season_cols, name="Season")+
  scale_color_manual(values=season_outline, name="Season")+
  labs(y=expression("Seasonal NEE (gC" *m^-2*")"), x="Seasonal ET (mm)")+
  theme_bw(base_size=22)+
  theme(
    # Direction: Negative length forces ticks inside the plot area
    axis.ticks.length = unit(-0.15, "cm"),
    
    # Padding: Push text away from the new inward ticks to avoid overlap
    axis.text.x = element_text(margin = margin(t = 0.3, unit = "cm")),
    axis.text.y = element_text(margin = margin(r = 0.3, unit = "cm"))
  )

# precip, NEE
p6 <- ggplot(seasonal_ann_cum[na_nee_prop<0.25 | is.na(na_nee_prop),],aes(precip_cum, NEE_cum))+
  geom_point(aes(color=season, fill=season), size=4, shape=21)+
  geom_hline(yintercept=0)+
  scale_fill_manual(values=season_cols, name="Season")+
  scale_color_manual(values=season_outline, name="Season")+
  labs(y=expression("Seasonal NEE (gC" *m^-2*")"), x="Seasonal Rainfall (mm)")+
  theme_bw(base_size=22)+
  theme(
    # Direction: Negative length forces ticks inside the plot area
    axis.ticks.length = unit(-0.15, "cm"),
    
    # Padding: Push text away from the new inward ticks to avoid overlap
    axis.text.x = element_text(margin = margin(t = 0.3, unit = "cm")),
    axis.text.y = element_text(margin = margin(r = 0.3, unit = "cm"))
  )

# plot NEE vs precip and ET side-by-side
p6_7 <- grid.arrange(p6 + theme(legend.position="none"),
                     p7+ theme(legend.position="none", axis.title.y=element_blank(), axis.text.y=element_blank()),
                     ncol=2)

ggsave(plot=p6_7, filename="Fig_NEE_water_regressions.pdf", path=figpath, width=14, height=7, units="in", scale=0.78)


# number of raining days, NEE
ggplot(seasonal_ann_cum[na_nee_prop<0.25 | is.na(na_nee_prop),],aes(day.count.precip.s, NEE_cum,color=season))+
  geom_point()+
  geom_hline(yintercept=0)+
  geom_smooth(method="lm")


# precip, ET
ggplot(seasonal_ann_cum[na_et_prop<0.25 | is.na(na_et_prop) & (na_precip_prop<0.25 | is.na(na_precip_prop)),],
       aes(precip_cum, ET_cum))+
  geom_point(aes(color=season))+
  geom_abline(intercept=0,slope=1)+
  lims(x=c(0,225), y=c(0,160))+
  scale_color_manual(values = season_cols) +
  theme_bw(base_size = 16)
  

# precip, ET ...  filter by (precip_cum-ET_cum)/precip_cum) >.-5
ggplot(seasonal_ann_cum[na_et_prop<0.25 | is.na(na_et_prop) & (na_precip_prop<0.25 | is.na(na_precip_prop)) &
                          ((precip_cum-ET_cum)/precip_cum)>(-5),],aes(precip_cum, ET_cum))+
  geom_point(aes(color=season))+
  geom_abline(intercept=0,slope=1)+
  lims(x=c(0,400),y=c(0,400))

# annual precip and NEE
p11 <- ggplot(annual_cum[(NA.count.nee<36 | is.na(NA.count.nee)) & Hydroyear<2026,],
              aes(precip_cum.ann, NEE_cum.ann, color=source_sink))+
  geom_point()+
  geom_hline(yintercept = 0,
             linewidth = 0.5,
             color = "grey40") +
  geom_vline(xintercept=240,, linetype="dashed", color="grey40", linewidth=0.6)+
  geom_abline(intercept = mod.ann.precip.intercept, slope = mod.ann.precip.slope, 
              size=0.7)+
  scale_color_manual(values = c("Sink" = "green4", "Source" = "salmon4"), name="") +
  labs(y=expression("Annual NEE (gC" *m^-2*")"), x="Total Rainfall (mm)")+
  theme_bw(base_size=20)+
  theme(
    # Direction: Negative length forces ticks inside the plot area
    axis.ticks.length = unit(-0.15, "cm"),
    
    # Padding: Push text away from the new inward ticks to avoid overlap
    axis.text.x = element_text(margin = margin(t = 0.3, unit = "cm")),
    axis.text.y = element_text(margin = margin(r = 0.3, unit = "cm"))
  )


# annual ET and NEE
p12 <- ggplot(annual_cum[(NA.count.nee<36 | is.na(NA.count.nee)) & Hydroyear<2026,],
              aes(ET_cum.ann, NEE_cum.ann, color=source_sink))+
  geom_point()+
  geom_hline(yintercept = 0,
             linewidth = 0.5,
             color = "grey40") +
  labs(y=expression("Annual NEE (gC" *m^-2*")"), x="Total ET (mm)")+
  scale_color_manual(values = c("Sink" = "green4", "Source" = "salmon4"), name="") +
  theme_bw(base_size=20)+
  theme(
    # Direction: Negative length forces ticks inside the plot area
    axis.ticks.length = unit(-0.15, "cm"),
    
    # Padding: Push text away from the new inward ticks to avoid overlap
    axis.text.x = element_text(margin = margin(t = 0.3, unit = "cm")),
    axis.text.y = element_text(margin = margin(r = 0.3, unit = "cm"))
  )

p11_12 <- plot_grid(p11 + theme(legend.position="none"),
                    p12+ theme(legend.position="none", axis.title.y=element_blank(), axis.text.y=element_blank()),
                    ncol=2,
                    align="h")

# save
ggsave(plot=p11_12, filename="Fig_water_nee_annual.pdf", path=figpath, width=14, height=7, units="in", scale=0.78)


# save seasonal NEE/rain and annual NEE/rain
p6_11 <- plot_grid(p6 + theme(legend.position="none"),
                    p11+ theme(legend.position="none"),
                    ncol=2,
                    align="h")

# save
ggsave(plot=p6_11, filename="Fig_rain_nee_regress.pdf", path=figpath, width=14, height=7, units="in", scale=0.78)


# STATS! 

# Compare NEE variance between seasons
# NEE seasonal boxplot
seasonal_ann_cum %>%
  #mutate(season_stack = fct_reorder(season, desc(NEE_cum)))%>%
  filter(na_nee_prop<0.25 | is.na(na_nee_prop)) %>%
  ggplot(., aes(season,NEE_cum,fill=season))+
  geom_boxplot()+
  geom_hline(yintercept=0)+
  coord_flip()


# Levene's test for variance,  does not assume homoscedascity
seasonal_ann_cum %>% 
  filter(na_nee_prop<0.25 | is.na(na_nee_prop)) %>%
rstatix::levene_test(NEE_cum ~ season)


# Welch one-way ANOVA + Games-Howell post-hoc (no equal-variance assumption)
# https://www.datanovia.com/learn/biostatistics/anova/anova-in-r
res.aov.seas.nee <- seasonal_ann_cum %>% 
  filter(na_nee_prop<0.25 | is.na(na_nee_prop)) %>%
  welch_anova_test(NEE_cum ~ season)
pwc.seas.nee <- seasonal_ann_cum %>% 
  filter(na_nee_prop<0.25 | is.na(na_nee_prop)) %>%
  games_howell_test(NEE_cum ~ season) %>%
  add_xy_position(x = "group", step.increase = 1)

# Compare precip variance between seasons
# NEE seasonal boxplot
seasonal_ann_cum %>%
  #mutate(season_stack = fct_reorder(season, desc(NEE_cum)))%>%
  filter(na_precip_prop<0.25 | is.na(na_precip_prop)) %>%
  ggplot(., aes(season,precip_cum,fill=season))+
  geom_boxplot()+
  geom_hline(yintercept=0)+
  coord_flip()


# Levene's test for variance,  does not assume homoscedascity
seasonal_ann_cum %>% 
  filter(na_precip_prop<0.25 | is.na(na_precip_prop)) %>%
  rstatix::levene_test(precip_cum ~ season)

res.aov.seas.precip <- seasonal_ann_cum %>% 
  filter(na_precip_prop<0.25 | is.na(na_precip_prop)) %>%
  welch_anova_test(precip_cum ~ season)
pwc.seas.precip <- seasonal_ann_cum %>% 
  filter(na_precip_prop<0.25 | is.na(na_precip_prop)) %>%
  games_howell_test(precip_cum ~ season) %>%
  add_xy_position(x = "group", step.increase = 1)


# Stats for seasonal water relatiionship: precip and ET
mod.seas.precip <- lm(NEE_cum ~ precip_cum*season, data=seasonal_ann_cum[na_nee_prop<0.25 | is.na(na_nee_prop),])

# extract residuals
mod.seas.precip.res <- resid(mod.seas.precip)
# produce residual vs. fitted plot
plot(fitted(mod.seas.precip), mod.seas.precip.res)
# add a horizontal line at 0 
abline(0,0)

# create Q-Q plot for residuals
qqnorm(mod.seas.precip.res)
# add a straight diagonal line 
# to the plot
qqline(mod.seas.precip.res) 

# density
plot(density(mod.seas.precip.res))

# model output
summary(mod.seas.precip)


# ET
# Stats for seasonal water relatiionship: precip and ET
mod.seas.et <- lm(NEE_cum ~ ET_cum*season, data=seasonal_ann_cum[na_et_prop<0.25 | is.na(na_et_prop),])

# extract residuals
mod.seas.et.res <- resid(mod.seas.et)
# produce residual vs. fitted plot
plot(fitted(mod.seas.et), mod.seas.et.res)
# add a horizontal line at 0 
abline(0,0)

# create Q-Q plot for residuals
qqnorm(mod.seas.et.res)
# add a straight diagonal line 
# to the plot
qqline(mod.seas.et.res) 

# density
plot(density(mod.seas.et.res))

# model output
summary(mod.seas.et)

# get regression line for ET/NEE in Pre-monsoon
#get intercept and slope value
mod.seas.et.coeff<-coefficients(mod.seas.et)          
mod.seas.et.intercept<-mod.seas.et.coeff[1]+mod.seas.et.coeff[3]
mod.seas.et.slope<- mod.seas.et.coeff[2]+mod.seas.et.coeff[6]


# compare ET and precip models
AICc(mod.seas.precip, mod.seas.et)

# Stats for annual water relatiionship: NEE and precip
mod.ann.precip <- lm(NEE_cum.ann ~ precip_cum.ann, data=annual_cum[(NA.count.nee<36 | is.na(NA.count.nee)) &  Hydroyear<2026,])

# extract residuals
mod.ann.precip.res <- resid(mod.ann.precip)
# produce residual vs. fitted plot
plot(fitted(mod.ann.precip), mod.ann.precip.res)
# add a horizontal line at 0 
abline(0,0)

# create Q-Q plot for residuals
qqnorm(mod.ann.precip.res)
# add a straight diagonal line 
# to the plot
qqline(mod.ann.precip.res) 

# density
plot(density(mod.ann.precip.res))

# model output
summary(mod.ann.precip)

# get regression line for annual precip/NEE
#get intercept and slope value
mod.ann.precip.coeff<-coefficients(mod.ann.precip)          
mod.ann.precip.intercept<-mod.ann.precip.coeff[1]
mod.ann.precip.slope<- mod.ann.precip.coeff[2]

# Stats for annual water relatiionship: NEE and ET
mod.ann.et <- lm(NEE_cum.ann ~ ET_cum.ann, data=annual_cum[(NA.count.nee<36 | is.na(NA.count.nee)) &  Hydroyear<2026,])

# extract residuals
mod.ann.et.res <- resid(mod.ann.et)
# produce residual vs. fitted plot
plot(fitted(mod.ann.et), mod.ann.et.res)
# add a horizontal line at 0 
abline(0,0)

# create Q-Q plot for residuals
qqnorm(mod.ann.et.res)
# add a straight diagonal line 
# to the plot
qqline(mod.ann.et.res) 

# density
plot(density(mod.ann.et.res))

# model output
summary(mod.ann.et)

# compare ET and precip models
AICc(mod.ann.precip, mod.ann.et)


