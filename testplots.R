#test plots
library(ggplot2)
library(data.table)

tracks2017<-fread(paste(getwd(),"/data/tracks2017.csv",sep=""))
tracks2017[Length == 0, "Length"] <-NA 
tracks2017<-tracks2017[,`:=`(month=format(as.Date(TrckStr, '%m/%d/%Y'),'%m'))]

#vessel_types<-c("Not Available","Other","Cargo","Tanker","Tug Tow","Passenger","Fishing","Pleasure Craft/Sailing")

tracks2017.summary<-tracks2017[,.(count = .N, avg.length = mean(Length, na.rm=T), unique.vessels = length(unique(MMSI)), avg.track.length = mean(Shp_Lng)), by="vssl_gr"]

tracks2017.summary.month<-tracks2017[,.(count = .N, avg.length = mean(Length, na.rm=T), unique.vessels = length(unique(MMSI)), avg.track.length = mean(Shp_Lng)), by=c("vssl_gr","month")]


#unique vessel counts by vessel type
tracks2017.pcSail<-tracks2017[vssl_gr == "Pleasure Craft/Sailing", .(count = .N, avg.length = mean(Length, na.rm=T), draft = mean(Draft), speed = mean(SOG), avg.track.length = mean(Shp_Lng)), by= c("VesslNm","shp_cr_","MMSI")]

tracks2017.fishing<-tracks2017[vssl_gr == "Fishing", .(count = .N, avg.length = mean(Length, na.rm=T), draft = mean(Draft), speed = mean(SOG), avg.track.length = mean(Shp_Lng)), by= c("VesslNm","shp_cr_","MMSI")]

tracks2017.passenger<-tracks2017[vssl_gr == "Passenger", .(count = .N, avg.length = mean(Length, na.rm=T), draft = mean(Draft), speed = mean(SOG), avg.track.length = mean(Shp_Lng)), by= c("VesslNm","shp_cr_","MMSI")]

tracks2017.other<-tracks2017[vssl_gr == "Other", .(count = .N, avg.length = mean(Length, na.rm=T), draft = mean(Draft), speed = mean(SOG), avg.track.length = mean(Shp_Lng)), by= c("VesslNm","shp_cr_","MMSI")] [order(shp_cr_, -count)]

tracks2017.cargo<-tracks2017[vssl_gr == "Cargo", .(count = .N, avg.length = mean(Length, na.rm=T), draft = mean(Draft), speed = mean(SOG),avg.track.length = mean(Shp_Lng)), by= c("VesslNm","shp_cr_","MMSI")]

tracks2017.tanker<-tracks2017[vssl_gr == "Tanker", .(count = .N, avg.length = mean(Length, na.rm=T), draft = mean(Draft), speed = mean(SOG), avg.track.length = mean(Shp_Lng)), by= c("VesslNm","shp_cr_","MMSI")]

tracks2017.tug<-tracks2017[vssl_gr == "Tug Tow", .(count = .N, avg.length = mean(Length, na.rm=T), draft = mean(Draft), speed = mean(SOG), avg.track.length = mean(Shp_Lng)), by= c("VesslNm","shp_cr_","MMSI")]



ggplot()+
  geom_bar(data=tracks_2017,aes(x=vssl_gr))+
  facet_wrap(.~month_,ncol=3)+
  coord_flip()

ggplot(tracks_2017)+
  geom_bar(aes(x=month_,fill=vssl_gr))+
  scale_fill_brewer(palette="Dark2")+
  theme_classic()

tracks_2017_summary<-tracks_2017[,.(trcount=.N),by=c("vssl_gr","month_")]

ggplot(tracks_2017_summary,aes(y=vssl_gr))+
  geom_point(aes(x=month_,size=trcount),alpha=0.7)+
  scale_size_area(max_size = 10)


ggplot(tracks_2017) + 
  stat_count(mapping = aes(x=vssl_gr, y=..prop.., group=1))+
  facet_wrap(.~month_f,ncol=3)


ggplot()+
  geom_bar(data=tracks_2017,aes(x=month_,fill=vssl_gr),position = "fill")+
  scale_fill_brewer(palette="Dark2")+
  theme_classic()
