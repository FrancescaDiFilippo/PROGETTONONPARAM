setwd('C:/Users/franc/Desktop/NONPA/PROGETTO')
dataset<- read.csv("dataset.csv")
comuni_italia <- st_read("Limiti01012020/Com01012020/Com01012020_WGS84.shp")

colnames(dataset)
# "timestamp"     "cuebiq_id"     "device"        "latitude"     
# "longitude"     "datetime"      "timedelta"     "spacedelta"   
# "stopping"      "journey_id"    "stationary_id"
library(gstat)
library(sf)
st_sf(dataset)
st_as_sfc(d)


library(maps)
library(maptools)
library(rgdal)
library(sp)
library(shp2graph)
library(lubridate)

lat_long_point_start <- cbind(dataset$latitude , dataset$longitude )
head(lat_long_point_start)
x_y= lat_long_point_start
x_y = as.data.frame(x_y)
colnames(x_y)<- c('lat','long')
head(x_y)
coordinates(x_y)<- ~lat+long

x_y@proj4string <- Lombardia_comuni@proj4string
x_y_new <- st_crs(x_y)
st_crs(x_y) <-st_crs(Lombardia_comuni)
head(x_y)

x_y_drop=x_y
dati <- st_as_sf(dataset, coords= c('latitude','longitude'),crs = 4326 )
dev.off()
#quartz()
x11()
plot(st_geometry(Lombardia_comuni))
plot(st_geometry(dati), col='darkorange1', cex=0.1, pch=20)
points(x_y_pick, col='cornflowerblue', cex=0.1, pch=20)
 
dati <- dati[1:1000,]


library(ggplot2)

ggplot() + geom_sf(data = Lombardia_comuni) + geom_sf(data = dati) 
st_contains(dati,Milano)


library(sf)
# since you mentioned in the comment you used dplyr I thought you wouldn't mind this dependency, used for filter
library(dplyr)

nhood <- st_read('input/HVERFAHEITI.shp')
names(schools) = c("school","lat","long")
schools <-  st_as_sf(schools, coords = c("long", "lat"), crs = 4326)

# this shows which neighbourhoods (HEITI) intersects each school
st_join(schools, nhood['HEITI'], join = st_intersects)
class(Lombardia_comuni)
class(dati)
dati <- st_as_sf(dataset, coords= c('latitude','longitude'),crs = 4326 )
st_join(dati, Lombardia_comuni, join = st_intersects)
st_intersects(dati,comuni_italia)
