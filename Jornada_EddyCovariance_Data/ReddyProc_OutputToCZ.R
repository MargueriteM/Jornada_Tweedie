
# Make figures with webtool ReddyProc gap-filled, partitioned data
# save output files for CZ

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
library(viridis)

# find files listed in reddyproc output
year_file <- 2022
list.files(paste("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Bahada/Tower/EddyCovariance_ts/ReddyProc_Gapfill_Partition/",year_file,"/", sep=""))


# import ReddyProc webtool output files
# rp.folder <- "REddyResults_US-Jo1_20221209_435805042" #(2020)
# rp.folder <- "REddyResults_US-Jo1_20221209_41329446" #(2021)
rp.folder <- "REddyResults_US-Jo1_20221209_759647150" #(2022)

flux.rp.units <- (fread(paste("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Bahada/Tower/EddyCovariance_ts/ReddyProc_Gapfill_Partition/",year_file,"/",rp.folder,"/output.txt",sep=""),
                   header=TRUE))[1,]

flux.rp <- fread(paste("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Bahada/Tower/EddyCovariance_ts/ReddyProc_Gapfill_Partition/",year_file,"/",rp.folder,"/output.txt",sep=""),
                 header=FALSE, skip=2,na.strings=c("-9999", "NA","-"),
                 col.names = colnames(flux.rp.units))

# for some reason na.strings won't recognize the -9999
flux.rp[flux.rp == -9999] <- NA



# substract 0.5 from the hour again to match TIMESTAMP_END
flux.rp[,Hour := Hour-0.5]

# plot with no U* filter or gapfill
ggplot(flux.rp, aes(DoY,NEE))+
  geom_line()+
  facet_grid(Year~.)

fig_nee <- ggplot(subset(flux.rp), aes(DoY,NEE))+
  geom_line()+
  geom_point(aes(y=NEE_U50_f),data=subset(flux.rp, is.na(NEE)),colour="red",size=0.25)+
  facet_grid(.~Year)+
ylim(c(-10,10))

fig_nee_fill <- ggplot((flux.rp))+
  geom_line(aes(DoY,NEE_U50_f))+
  geom_line(aes(DoY,NEE), data = flux.rp)+
  facet_grid(.~Year)+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())+
ylim(c(-10,10))

fig_reco <- ggplot(subset(flux.rp), aes(DoY,Reco_U50))+geom_line()+
  geom_line(aes(y=Reco_DT_U50),colour="blue")+
  facet_grid(.~Year)+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())#+
ylim(c(-10,10))

# plot daytime Reco with qc code
ggplot(subset(flux.rp), aes(DoY,Reco_DT_U50, colour=factor(FP_qc)))+
  geom_point(size=1)+
  facet_grid(.~Year)

fig_gpp <- ggplot(subset(flux.rp), aes(DoY,GPP_U50_f))+geom_line()+
  geom_line(aes(y=GPP_DT_U50),colour="blue")+
  facet_grid(.~Year)+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())#+
ylim(c(-10,10))

fig_vpd <- ggplot(subset(flux.rp), aes(DoY,VPD))+geom_line()+
  geom_hline(yintercept=10, colour="red")+
  facet_grid(.~Year)

grid.arrange(fig_nee, fig_reco, fig_gpp, fig_vpd, nrow=4)

grid.arrange(fig_reco, fig_gpp, fig_nee, nrow=3)

grid.arrange(fig_nee_fill, fig_reco, fig_gpp, nrow=3)

grid.arrange(fig_nee, fig_gpp, fig_vpd, nrow=3)
grid.arrange(fig_nee, fig_reco, fig_vpd, nrow=3)

# plot measured vs modeled NEE at U50 (NEE_orig is unfiltered, I can't get unfilled U* filtered from ReddyProc online tool)
ggplot(subset(flux.rp), aes(NEE, NEE_U50_f))+geom_point()+
  geom_abline(intercept=0, slope=1)+
  facet_grid(.~Year)

# plot NEE with different U* quantiles
ggplot(subset(flux.rp), aes(x=DoY))+
  geom_line(aes(y=NEE_U05_f, colour="NEE_U05_f"))+
  geom_line(aes(y=NEE_U50_f, colour="NEE_U50_f"))+
  geom_line(aes(y=NEE_U95_f, colour="NEE_U95_f"))+
  facet_grid(Year~.)


# heat map of gap-filled NEE data
ggplot(flux.rp,
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



# save the ReddyProc output data by year


setwd("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Tower Data/JER_Bajada/EddyCovarianceTower/ReddyProc_Partition_Gapfill")

# save by years
for (i in year_file:year_file){
  # subset each year
  dat.save <- flux.rp[Year==i,]
  dat.save[,date_time := ymd_hm(paste(as.Date(DoY-1, origin = ymd(paste(i,"-01-01")),tz="UTC"),
                                trunc(Hour), (Hour-trunc(Hour))*60), tz = "UTC")]
   write.table (dat.save,
                file= paste("USJo1_partitioned",min(as.character(dat.save$date_time,format= "%Y%m%d%H%M")),
                            max(as.character(dat.save$date_time,format= "%Y%m%d%H%M")),
                            "20200427.csv",sep="_"),
                sep =',', dec='.', row.names=FALSE, na="-9999", quote=FALSE)
}
