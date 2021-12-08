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






library(ptinpoly)
# define a square 
poligono <- cbind(
  ingressi_areac_varchi$LONG_X_4326, ingressi_areac_varchi$LAT_Y_4326
)

pinside <- rbind(c(9.18173638705536,45.4776150167559))
poutside <- rbind(c(2,1))
pin <- rbind(c(9.18174838705542,45.4776150167564))

pip2d(poligono, pinside)
pip2d(poligono, poutside)
pip2d(poligono, pin)




# comuni_italia <- st_read("Limiti01012020/Com01012020/Com01012020_WGS84.shp")
# comuni_italia <-st_transform(comuni_italia, crs=4326)
# Lombardia_comuni <- comuni_italia %>% filter(COD_REG == 3)
# 
# Milano = Lombardia_comuni %>% filter(COMUNE == "Milano")
# 
# x11()
# plot(st_geometry(Milano), col = 3, border = 'grey',  axes = TRUE)
# plot(st_geometry(st_centroid(Milano)), pch = 3, col = 'red', add = TRUE)
# plot(polygon_c, col='blue', add=TRUE)
# 



# controllare le persone che entrano in area c


setwd('C:/Users/Elena/Desktop/Elena/Polimi/MAGISTRALE/Nonparametric statistics/Progetto')

dataset <- st_read("dataset.csv")

dati_latlon <- cbind(dataset$longitude, dataset$latitude)

presenza <- pip2d(poligono, dati_latlon)

for(i in 1:length(presenza)){
  print(i)
  print(presenza[i])
  if (presenza[i] >= 0){
    dataset$ingresso_areac[i] <- "area c"
  }
  else{
    dataset$ingresso_areac[i] <- "outside"
  }
}

n_persone_in_areac <- sum(dataset$ingresso_areac == "area c")  # 638

write.csv(dataset,"C:\\Users\\Elena\\Desktop\\Elena\\Polimi\\MAGISTRALE\\Nonparametric statistics\\Progetto\\dataset_areac.csv", row.names = FALSE)












