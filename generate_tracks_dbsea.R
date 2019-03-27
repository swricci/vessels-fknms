#take two points find the distance  then divide by time difference
# plot this
# get representative velocity (median/mean if relatively constant, if there is a drop then determine what the speed would be if it was moving at constant rate)

library(data.table)
library(sf)
library(ggplot2)

#get directory for track shapefiles
tracks.dir<-"C:/Users/sbrown/Documents/ArcGIS/Projects/vesselNoise/selected_tracks_bytype"
tracks.filelist<-list.files(tracks.dir, pattern=glob2rx("*.shp$"), full.names = T)

tug.track<-st_read(tracks.filelist[21])
plot(tug.track[3])

cargo.track<-st_read(tracks.filelist[1])
plot(cargo.track[3])
