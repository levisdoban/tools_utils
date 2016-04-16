library(raster)

rad2deg = function(rad) {(rad * 180) / (pi)}
deg2rad = function(deg) {(deg * pi) / (180)}

#calculates the destination lat/long coord given a starting lat/lon and the distance to the next point, and the bearing 

nextgps = function(lat, long, bearing, dist){
  d = dist
  R = 6378
  long = deg2rad(long)
  lat = deg2rad(lat)
  brng = deg2rad(bearing) 
  
  final_lat = asin(sin(lat)*cos(d/R) + cos(lat) * sin(d/R) * cos(brng) );
  final_long = long + atan2(sin(brng)*sin(d/R)*cos(lat),cos(d/R)-sin(lat)*sin(final_lat));
  
  lat1 = rad2deg(final_lat)
  long1 = rad2deg(final_long)
  ss = list(lat = lat1, long = long1)
  return(ss)
  
}

# calculates the bounding box, min/max lat and long for a given point.

bound_box = function(lat, long, dist){
  
startpoint = nextgps(lat, long, 90, dist/2)
firstcorner = nextgps(startpoint$lat, startpoint$long, 0, dist/2)
secondcorner = nextgps(firstcorner$lat, firstcorner$long, 270, dist) 
thirdcorner = nextgps(secondcorner$lat, secondcorner$long, 180, dist) 
#fourthcorner = nextgps(thirdcorner$lat, thirdcorner$long, 90, dist) 

longs = c(thirdcorner[[2]], firstcorner[[2]])
lats = c(thirdcorner[[1]], firstcorner[[1]])
data = c(min(lats), max(lats),min(longs), max(longs) )
ssd = extent(data) 
return(ssd)
} 

############# EXAMPLE ############3
# lat = 0.1567
# long = 34.4567
# dist = 40 #in KM
#> bound_box(lat, long, dist)
#class       : Extent 
#xmin        : -0.0229743 
#xmax        : 0.3363661 
#ymin        : 34.27703 
#ymax        : 34.63637 

################################

