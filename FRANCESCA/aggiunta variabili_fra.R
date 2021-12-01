#### AGGIUNTA VARIABILI ####

setwd('C:/Users/franc/Desktop/NONPA/PROGETTO')
dataset<- read.csv("dataset.csv")

library(dplyr)
dati <- dataset[1:1000,]
# write.csv2( dati,'dataset_100.csv')
# colnames(dati)
# dati<- read.csv("dataset_100.csv", header=T)

 attach(dati)
dati <- dati[,-11]

#dati <- dataset[,-dim(dataset)[2]]
dati[which(is.na(dati[,7])==TRUE),7] <- 0
dati[which(is.na(dati[,8])==TRUE),8] <- 0

dati[which(stopping==TRUE),7] <- 0
dati[which(stopping==TRUE),8] <- 0


# 
# #bisogna mettere a 0 i timedelta e spacedelta di tutte le prime osservazioni di ciascun journey_id
# j.id <- unique(dati$journey_id) #dataset ordinato per journey ID
# first<- NULL #first è il vettore degli indici delle prime osservazioni per ciascun journey_id
# for (i in 1:length(j.id)){
#   temp <- subset(dati, journey_id==j.id[i])
#   temp2 <- cbind(temp[1,1], temp[1,10])
#   first<-rbind(first, which(dati$timestamp==temp2[1] & dati$journey_id==temp2[2]))
# }
# dati[first,7:8] <- 0

dati_totaltime <- dati %>%  group_by(cuebiq_id,journey_id) %>% summarise(tot_time = sum(timedelta))
dati_totalspace <- dati %>%  group_by(cuebiq_id,journey_id) %>% summarise(tot_space = sum(spacedelta))
dati_tot  <- dati %>%  group_by(cuebiq_id,journey_id) %>% summarise(tot_time = sum(timedelta),tot_space = sum(spacedelta))
dati_tot$mean_speed <- (dati_tot$tot_space/dati_tot$tot_time)/1000*3600
dati_meanvalues <- dati_tot %>%  group_by(cuebiq_id) %>% summarise(mean_time =mean(tot_time), mean_space=mean(tot_space))
dati_complete <- left_join(dati_tot,dati_meanvalues, by= 'cuebiq_id')
ii <- which(dati_complete$mean_speed > 30)

sum(dati$timedelta[126:223])
# prove------
# #"timestamp"  "cuebiq_id" "device"  "latitude" "longitude" "datetime"  "stopping"  "journey_id" "stationary_id"
# dati_totaltime <- dati %>% select(time=first(timestamp ), tot_time= sum(timedelta),journey_id, cuebiq_id ,device , lat_init=first( latitude),long_init= first(longitude),time_init=first(datetime)) 
#                     ... %>% group_by(cuebiq_id,journey_id)
# timedelta <- as.integer(timedelta)
# dati_totaltime <- dati %>% select( cuebiq_id,journey_id,tot_time=sum(as.numeric(timedelta)))%>% group_by(cuebiq_id,journey_id)
# 
# dati_totaltime <- dati %>%  group_by(cuebiq_id,journey_id) %>% summarise(tot_time = sum(timedelta))
# which(is.na(timedelta)==TRUE)
# 
# dati <- dati[,-11]
# 
# dati <- dataset[,-dim(dataset)[2]]
# dati[which(is.na(dati[,7])==TRUE),7] <- 0
# dati[which(is.na(dati[,8])==TRUE),8] <- 0
#####################################################################################
