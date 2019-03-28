#take two points find the distance  then divide by time difference
# plot this
# get representative velocity (median/mean if relatively constant, if there is a drop then determine what the speed would be if it was moving at constant rate)

library(data.table)
library(sf)
library(ggplot2)

#get directory for track shapefiles
tracks.dir<-"C:/Users/sbrown/Documents/ArcGIS/Projects/vesselNoise/selected_tracks_bytype"
tracks.filelist<-list.files(tracks.dir, pattern=glob2rx("*.shp$"), full.names = T)

cargo.caribenavigator<-st_read(tracks.filelist[1])
plot(cargo.caribenavigator[3])
plot(cargo.caribenavigator$BaseDateTi,cargo.caribenavigator$SOG)
cargo.caribenavigator$BaseDateTi<-as.POSIXct(cargo.caribenavigator$BaseDateTi,format="%Y-%m-%dT%H:%M:%S")

cargo.maerskmemphis<-st_read(tracks.filelist[2])
plot(cargo.maerskmemphis[3])

cargo.tropicmist<-st_read(tracks.filelist[3])
plot(cargo.tropicmist[3])


#get directory of track csv files. The csv files have correct time stamps and point location data (XY)
#tracks.dir<-"C:/Users/sbrown/Documents/research/track_info"

vessel.tracks<-fread("C:/Users/sbrown/Documents/research/track_info/selected_vessel_tracks.csv")
vessel.tracks$BaseDateTime<-as.POSIXct(vessel.tracks$BaseDateTime,format="%m/%d/%Y %H:%M:%S")

tug_genesispatriot<-vessel.tracks[VesselName=="GENESIS PATRIOT",][order(BaseDateTime),]

#calculate time difference between each point
tug_etime<-rep(NA,length(tug_genesispatriot$BaseDateTime))
for (i in seq(1, length(tug_genesispatriot$BaseDateTime)-1,by=1)){
  tug_etime[i+1]<-tug_genesispatriot$BaseDateTime[i+1]-tug_genesispatriot$BaseDateTime[i]
}
tug_etime[1]<-0

#calculate distance between each point
#convert to sf
tug_genesispatriot_sf<-st_as_sf(tug_genesispatriot,coords=c('POINT_X','POINT_Y'), crs= "+init=epsg:4326")
plot(tug_genesispatriot_sf[4])

tug_dist<-rep(NA,length(tug_etime))
for (i in seq(from=1, to=length(tug_dist)-1,by= 1)){
  tug_dist[i+1]<-st_distance(tug_genesispatriot_sf[i+1,],tug_genesispatriot_sf[i,])
}
tug_dist[1]<-0

tug_info<-cbind(tug_etime,tug_dist)
tug_info<-as.data.table(tug_info)

tug_info$velocity<-tug_info$tug_dist/(tug_info$tug_etime*60)
plot(tug_info$velocity)
plot(tug_genesispatriot$SOG)


st_distance(tug_genesispatriot_sf[1,],tug_genesispatriot_sf[2,])