# check biomet data. 2021 precip looks really strange


biomet2021.names <- colnames(fread("/Volumes/SEL_Data_Archive/Research Data/Desert/Jornada/Bahada/Tower/EddyCovariance_ts/EddyPro_Biomet/Biomet_EddyPro_2021.csv",
                             header=TRUE))
                    

biomet2021 <- fread("/Volumes/SEL_Data_Archive/Research Data/Desert/Jornada/Bahada/Tower/EddyCovariance_ts/EddyPro_Biomet/Biomet_EddyPro_2021.csv",
                    skip=2, header=FALSE, col.names=biomet2021.names, na.strings=c("-9999"))


biomet2021 <- biomet2021[,':='
                         (date_time=ymd_hm(paste(timestamp_1,timestamp_2,timestamp_3,timestamp_4,timestamp_5, sep=" ")))]

ggplot(biomet2021, aes(date_time,P_rain_1_1_1))+
  geom_line()

ggplot(biomet2021, aes(date_time,Ta_1_1_1))+
  geom_line()


#2020
biomet2020.names <- colnames(fread("/Volumes/SEL_Data_Archive/Research Data/Desert/Jornada/Bahada/Tower/EddyCovariance_ts/EddyPro_Biomet/Biomet_EddyPro_2020.csv",
                                   header=TRUE))


biomet2020 <- fread("/Volumes/SEL_Data_Archive/Research Data/Desert/Jornada/Bahada/Tower/EddyCovariance_ts/EddyPro_Biomet/Biomet_EddyPro_2020.csv",
                    skip=2, header=FALSE, col.names=biomet2020.names, na.strings=c("-9999"))


biomet2020 <- biomet2020[,':='
                         (date_time=ymd_hm(paste(timestamp_1,timestamp_2,timestamp_3,timestamp_4,timestamp_5, sep=" ")))]

ggplot(biomet2020, aes(date_time,P_rain_1_1_1))+
  geom_line()

ggplot(biomet2020, aes(date_time,Ta_1_1_1))+
  geom_line()


# compare biomet with Eddypro flux output
# series 1
p1<-ggplot(biomet2020,aes(date_time,Ta_1_1_1))+geom_line()

p2<-ggplot(flux_add[!(FILENAME_HF %in% ("not_enough_data"))],aes(date_time,TA_1_1_1))+geom_line()

p3<-ggplot(biomet2020,aes(date_time,RH_1_1_1))+geom_line()

p4<-ggplot(flux_add[!(FILENAME_HF %in% ("not_enough_data"))],aes(date_time,RH_1_1_1))+geom_line()

p5<-ggplot(biomet2020,aes(date_time,Pa_1_1_1))+geom_line()

p6<-ggplot(flux_add[!(FILENAME_HF %in% ("not_enough_data"))],aes(date_time,PA_1_1_1))+geom_line()

p7<-ggplot(biomet2020,aes(date_time,WD_1_1_1))+geom_line()

p8<-ggplot(flux_add[!(FILENAME_HF %in% ("not_enough_data"))],aes(date_time, WD_1_1_1))+geom_line()

p9<-ggplot(biomet2020,aes(date_time,MWS_1_1_1))+geom_line()

p10<-ggplot(flux_add[!(FILENAME_HF %in% ("not_enough_data"))],aes(date_time, MWS_1_1_1))+geom_line()
# series 2
p11<-ggplot(biomet2020,aes(date_time,PPFD_1_1_1))+geom_line()

p12<-ggplot(flux_add,aes(date_time, PPFD_IN_1_1_1))+geom_line()

p13<-ggplot(biomet2020,aes(date_time,PPFDr_1_1_1))+geom_line()

p14<-ggplot(flux_add,aes(date_time, PPFD_OUT_1_1_1))+geom_line()

p15<-ggplot(biomet2020,aes(date_time,P_rain_1_1_1))+geom_line()

p16<-ggplot(flux_add,aes(date_time, P_RAIN_1_1_1))+geom_line()

p17<-ggplot(biomet2020,aes(date_time,SWC_1_1_1))+geom_line()

p18<-ggplot(flux_add,aes(date_time, SWC_1_1_1))+geom_line()

p19<-ggplot(biomet2020,aes(date_time,Ts_1_1_1))+geom_line()

p20<-ggplot(flux_add,aes(date_time,TS_1_1_1))+geom_line()
series1 <- grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,ncol=2)
series2 <- grid.arrange(p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,ncol=2)
# series 3
p21<-ggplot(biomet2020,aes(date_time,SHF_1_1_1))+geom_line()

p23<-ggplot(biomet2020,aes(date_time,SHF_1_2_1))+geom_line()

p25<-ggplot(biomet2020,aes(date_time,SHF_2_1_1))+geom_line()

p27<-ggplot(biomet2020,aes(date_time,SHF_2_2_1))+geom_line()

p22<-ggplot(flux_add,aes(date_time,SHF_1_1_1))+geom_line()

p24<-ggplot(flux_add,aes(date_time,SHF_1_2_1))+geom_line()

p26<-ggplot(flux_add,aes(date_time,SHF_2_1_1))+geom_line()

p28<-ggplot(flux_add,aes(date_time,SHF_2_2_1))+geom_line()

p22<-ggplot(flux_add,aes(date_time,G_1_1_1))+geom_line()

p24<-ggplot(flux_add,aes(date_time,G_1_2_1))+geom_line()

p26<-ggplot(flux_add,aes(date_time,G_2_1_1))+geom_line()

p28<-ggplot(flux_add,aes(date_time,G_2_2_1))+geom_line()
#series 4
p31<-ggplot(biomet2020,aes(date_time,LWin_1_1_1))+geom_line()

p33<-ggplot(biomet2020,aes(date_time,LWout_1_1_1))+geom_line()

p35<-ggplot(biomet2020,aes(date_time,SWout_1_1_1))+geom_line()

p37<-ggplot(biomet2020,aes(date_time,Rg_1_1_1))+geom_line()

p39<-ggplot(biomet2020,aes(date_time,Rn_1_1_1))+geom_line()

p32<-ggplot(flux_add,aes(date_time,LW_IN_1_1_1))+geom_line()

p34<-ggplot(flux_add,aes(date_time,LW_OUT_1_1_1))+geom_line()

p36<-ggplot(flux_add,aes(date_time,SW_OUT_1_1_1))+geom_line()

p38<-ggplot(flux_add,aes(date_time,SW_IN_1_1_1))+geom_line()

p40<-ggplot(flux_add,aes(date_time,NETRAD_1_1_1))+geom_line()

series1 <- grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,ncol=2)
series2 <- grid.arrange(p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,ncol=2)
series3 <- grid.arrange(p21,p22,p23,p24,p25,p26,p27,p28,ncol=2)
series4 <- grid.arrange(p31,p32,p33,p34,p35,p36,p37,p38,p39,p40,ncol=2)

# very obvious mismatch variables:
# RH, PA, WD, MWS
# PPFD_OUT, P_RAIN, SWC, TS
# none (G match)
# none (LW_IN, LW_OUT, SW_OUT, SW_IN, NETRAD match)
