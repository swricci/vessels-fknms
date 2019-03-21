#working code to extract vessel track lines to use in dBSea

library(data.table)
library(sf)
library(ggplot2)

ais_zone17_062017<-fread("/Users/sbrown/Documents/research/vesseltraffic/AIS_2017_06_Zone17.csv")
ais_fknms_062017<-ais_zone17_062017[LAT %between% c(24.25,24.75) & LON %between% c(-82.75, -80.75),]
write.csv(ais_fknms_062017,"ais_fknms_062017.csv")

tug_offshore<-ais_fknms_062017[MMSI == 367006630,]
write.csv(tug_offshore,"tug_path_offshore.csv")

tug_offshore<-tug_offshore[,.(BaseDateTime,LAT,LON,SOG,Draft)]
#change datetime character string to date and time 
tug_offshore$BaseDateTime<-as.POSIXct(tug_offshore$BaseDateTime,format="%Y-%m-%dT%H:%M:%S")
#order table by time
tug_offshore<-tug_offshore[order(BaseDateTime),]

#calculate time difference between each point
etime<-rep(NA,length(tug_offshore$BaseDateTime))
for (i in seq(1, length(tug_offshore$BaseDateTime)-1,by=1)){
  etime[i+1]<-tug_offshore$BaseDateTime[i+1]-tug_offshore$BaseDateTime[i]
}
etime[1]<-0

tug_offshore_dbsea<-data.table(LON=tug_offshore$LON,LAT=tug_offshore$LAT,Draft=tug_offshore$Draft,etime)
tug_offshore_dbsea<-st_as_sf(tug_offshore_dbsea,coords=c('LON','LAT'), crs= "+init=epsg:4326")

tug_offshore_dbsea<-st_transform(tug_offshore_dbsea,26917)
