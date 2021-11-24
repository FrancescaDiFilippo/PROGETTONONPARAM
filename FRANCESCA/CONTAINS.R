library(sf)
library(dplyr)
library(gstat)
library(ggplot2)
library(maps)
library(maptools)
library(rgdal)
library(sp)
library(shp2graph)
library(lubridate)

#COMUNI------
setwd('C:/Users/franc/Desktop/NONPA/PROGETTO')
comuni_italia <- st_read("Limiti01012020/Com01012020/Com01012020_WGS84.shp")
comuni_italia <-st_transform(comuni_italia, crs=4326)
Lombardia_comuni <- comuni_italia %>% filter(COD_REG == 3)

Milano = Lombardia_comuni %>% filter(COMUNE == "Milano")

x11()
plot(st_geometry(Milano), col = 3, border = 'grey',  axes = TRUE)
plot(st_geometry(st_centroid(Milano)), pch = 3, col = 'red', add = TRUE)
x11()
plot(st_geometry(Lombardia), col = sf.colors(12, categorical = TRUE), border = 'grey', 
     axes = TRUE)
plot(st_geometry(st_centroid(lombardia)), pch = 3, col = 'red', add = TRUE)


#PROVINCE:-----
province_italia <- st_read("Limiti01012020/ProvCM01012020/ProvCM01012020_WGS84.shp")
st_transform(province_italia, crs=4326)

Lombardia_province <- province_italia %>% filter(COD_REG == 3)
Milano_prov <- Lombardia_province %>% filter(DEN_UTS =='Milano')
x11()
plot(st_geometry(Lombardia_province), col = sf.colors(12, categorical = TRUE), border = 'grey',  axes = TRUE)
plot(st_geometry(st_centroid(Lombardia_province)), pch = 3, col = 'red', add = TRUE)
x11()
plot(st_geometry(Lombardia), col = sf.colors(12, categorical = TRUE), border = 'grey', 
     axes = TRUE)
plot(st_geometry(st_centroid(lombardia)), pch = 3, col = 'red', add = TRUE)


## DATASET----
setwd('C:/Users/franc/Desktop/NONPA/PROGETTO')
dataset<- read.csv("dataset.csv")

#within Milano:
dataset_milan <- dataset[which(dataset$latitude<45.531 & dataset$latitude>45.433 & dataset$longitude<9.245 & dataset$longitude>9.098),]
dati <- st_as_sf(dataset, coords= c('longitude','latitude'),crs = 4326 )
dati <- dati[1:1000,]

class(dati)
class(Lombardia_comuni)

contains <- st_contains(dati,Milano )
contains

within__ <- st_within(dati,Milano)
within__

df <- within__ %>% lengths > 0
dati_milan <- dati[df==1,]

x11()
ggplot() + geom_sf(data= Milano)+geom_sf(data=dati_milan, col='Red')
