library(sf)
library(dplyr)
library(gstat)
library(maps)
library(maptools)
library(rgdal)
library(sp)
library(shp2graph)
library(lubridate)


# controllo ingressi in area c
setwd('C:/Users/Elena/Desktop/Elena/Polimi/MAGISTRALE/Nonparametric statistics/Progetto/github repository/PROGETTONONPARAM/DATASET')
ingressi_areac_varchi <- st_read("ingressi_areac_varchi.csv")

library(sp)
xym <- cbind(as.double(ingressi_areac_varchi$LONG_X_4326), as.double(ingressi_areac_varchi$LAT_Y_4326))
p <- Polygon(xym)
ps <- Polygons(list(p),1)
sps <- SpatialPolygons(list(ps))

x11()
plot(sps)

# Oggetto di tipo sf:
polygon_c <- st_as_sf(sps, coords= c(ingressi_areac_varchi$LONG_X_4326, ingressi_areac_varchi$LAT_Y_4326),crs = 4326)
polygon_c <- st_transform(polygon_c, crs=4326)


library(rgeos)

#intersects <- rgeos::gIntersects(sps, as(dati, Class = "Spatial"))
#intersects
#x11()
#plot(intersects)



comuni_italia <- st_read("Limiti01012020/Com01012020/Com01012020_WGS84.shp")
comuni_italia <-st_transform(comuni_italia, crs=4326)
Lombardia_comuni <- comuni_italia %>% filter(COD_REG == 3)

Milano = Lombardia_comuni %>% filter(COMUNE == "Milano")

x11()
plot(st_geometry(Milano), col = 3, border = 'grey',  axes = TRUE)
plot(st_geometry(st_centroid(Milano)), pch = 3, col = 'red', add = TRUE)
plot(polygon_c, col='blue', add=TRUE)




# controllare le persone che entrano in area c


setwd('C:/Users/Elena/Desktop/Elena/Polimi/MAGISTRALE/Nonparametric statistics/Progetto')

dataset <- st_read("dataset.csv")




dati <- st_as_sf(dataset, coords= c('latitude','longitude'),crs = 4326 )
contains <- st_contains(dati,polygon_c )
contains

within__ <- st_within(dati, polygon_c)
within__

df <- within__ %>% lengths > 0
dati_milan <- dati[df==1,]

x11()
ggplot() + geom_sf(data= Milano)+geom_sf(data=dati_milan, col='Red')








