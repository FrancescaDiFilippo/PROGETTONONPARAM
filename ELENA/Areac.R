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
polygon_c <- st_as_sf(sps)



comuni_italia <- st_read("Limiti01012020/Com01012020/Com01012020_WGS84.shp")
comuni_italia <-st_transform(comuni_italia, crs=4326)
Lombardia_comuni <- comuni_italia %>% filter(COD_REG == 3)

Milano = Lombardia_comuni %>% filter(COMUNE == "Milano")

x11()
plot(st_geometry(Milano), col = 3, border = 'grey',  axes = TRUE)
plot(st_geometry(st_centroid(Milano)), pch = 3, col = 'red', add = TRUE)
plot(polygon_c, col='blue', add=TRUE)



















