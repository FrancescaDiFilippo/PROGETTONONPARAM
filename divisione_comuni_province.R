library(sf)
library(dplyr)
setwd('C:/Users/franc/Desktop/NONPA/PROGETTO')
comuni_italia <- st_read("Limiti01012020/Com01012020/Com01012020_WGS84.shp")

CODICI <- read.csv("CODICI COMUNI ITALIA.csv",header=T, sep=';')
colnames(CODICI)
CODICI_REG <- CODICI %>% select(ï..Codice.Regione,Denominazione.Regione) %>%group_by(Denominazione.Regione)
ii <- CODICI_REG[,"Denominazione.Regione"== 'Lombaridia']
Lombardia_comuni <- comuni_italia %>% filter(COD_REG == 3)

Milano = Lombardia_comuni %>% filter(COMUNE == "Milano")
II <- which(comuni$COMUNE=='Milano')

x11()
plot(st_geometry(Milano), col = 3, border = 'grey',  axes = TRUE)
plot(st_geometry(st_centroid(Milano)), pch = 3, col = 'red', add = TRUE)
x11()
plot(st_geometry(Lombardia), col = sf.colors(12, categorical = TRUE), border = 'grey', 
     axes = TRUE)
plot(st_geometry(st_centroid(lombardia)), pch = 3, col = 'red', add = TRUE)



#PROVINCE:-----
province_italia <- st_read("Limiti01012020/ProvCM01012020/ProvCM01012020_WGS84.shp")

Lombardia_province <- province_italia %>% filter(COD_REG == 3)
Milano_prov <- Lombardia_province %>% filter(DEN_UTS =='Milano')
x11()
plot(st_geometry(Lombardia_province), col = sf.colors(12, categorical = TRUE), border = 'grey',  axes = TRUE)
plot(st_geometry(st_centroid(Lombardia_province)), pch = 3, col = 'red', add = TRUE)
x11()
plot(st_geometry(Lombardia), col = sf.colors(12, categorical = TRUE), border = 'grey', 
     axes = TRUE)
plot(st_geometry(st_centroid(lombardia)), pch = 3, col = 'red', add = TRUE)

# PROVE CON LE COORDINATE----
st_intersects(Milano, Milano_prov)
st_contains(Milano, Milano_prov)


x11()
plot(st_geometry(Lombardia_province), col = sf.colors(12, categorical = TRUE), border = 'grey',  axes = TRUE)
plot(st_geometry(Lombardia), col = sf.colors(12, categorical = TRUE), border = 'grey', 
     axes = TRUE)


ggplot() + 
  geom_sf(data = comuni, aes(fill = BIR74)) + 
  scale_y_continuous(breaks = 34:36)