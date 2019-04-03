library(data.table)
library(sf)
library(ggplot2)

#track points from ArcPro - equal distance along original track line, set to 1km with starting point.
#find the time it takes to go that distance if vessel was traveling at average speed in m/s, time is in sec

satori_track2_pts_dt<-fread("C:/Users/sbrown/Documents/dBSea_fknms/satori_track2_eqpts.csv")
satori_track2_pts<-st_as_sf(satori_track2_pts,coords=c('POINT_X','POINT_Y'), crs= "+init=epsg:4269")
satori_track2_utm<-st_transform(satori_track2_pts, 26917) #convert to UTM for dBSea
#plot(satori_track2_pts)

satori_speed<-5.35 #m/s, originally 10.4 knots

satori_dist<-rep(NA,42)
for (i in seq(from=1, to=42-1,by= 1)){
  satori_dist[i+1]<-st_distance(satori_track2_pts[i+1,],satori_track2_pts[i,])
}
satori_dist[1]<-0

satori_time<-rep(NA,42)
for (i in seq(from=1, to=42-1,by= 1)){
  satori_time[i+1]<-satori_dist[i+1]/satori_speed
}
satori_time[1]<-0

satori_track2<-data.table(st_coordinates(satori_track2_utm),satori_time)
colnames(satori_track2)<-c("Easting","Northing", "Time")
write.csv(satori_track2,"C:/Users/sbrown/Documents/dBSea_fknms/satori_track2.csv",row.names = F, quote = F)





###OLD CODE BELOW###
#take two points find the distance  then divide by time difference
# plot this
# get representative velocity (median/mean if relatively constant, if there is a drop then determine what the speed would be if it was moving at constant rate)



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