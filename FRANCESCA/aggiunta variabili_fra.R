#### AGGIUNTA VARIABILI ####

setwd('C:/Users/franc/Desktop/NONPA/PROGETTO')
dataset<- read.csv("dataset.csv")


dati <- dataset[1:1000,]
colnames(dati)
dati$timedel
attach(data)
#"timestamp"  "cuebiq_id" "device"  "latitude" "longitude" "datetime"  "stopping"  "journey_id" "stationary_id"
dati_totaltime <- dati %>% select(time=first(timestamp ), tot_time= sum(timedelta), cuebiq_id ,device , lat_init=first( latitude),long_init= first(longitude),time_init=first(datetime)) 
                     %>% group_by(cuebiq_id,journey_id)
timedelta <- as.integer(timedelta)
dati_totaltime <- dati %>% select( cuebiq_id,journey_id,tot_time=sum(as.numeric(timedelta)))%>% group_by(cuebiq_id,journey_id)

which(is.na(timedelta)==TRUE)

dati <- dati[,-11]

dati <- dataset[,-dim(dataset)[2]]
dati[which(is.na(dati[,7])==TRUE),7] <- 0
dati[which(is.na(dati[,8])==TRUE),8] <- 0

#bisogna mettere a 0 i timedelta e spacedelta di tutte le prime osservazioni di ciascun journey_id
j.id <- unique(data$journey_id) #dataset ordinato per journey ID
first<- NULL #first è il vettore degli indici delle prime osservazioni per ciascun journey_id
for (i in 1:length(j.id)){
  temp <- subset(data, journey_id==j.id[i])
  temp2 <- cbind(temp[1,1], temp[1,10])
  first<-rbind(first, which(data$timestamp==temp2[1] & data$journey_id==temp2[2]))
}
data[first,7:8] <- 0

data <- data[1:4000,] #dataset completo troppo pesante

#calcolo quanti viaggi ci sono stati al giorno 
range.time <- cbind(1581894000,1582498799)
j.per.day <- NULL
for (i in 1:7){
  j.per.day [i] <- length(unique(data[which(data$timestamp>=range.time[1]+(i-1)*24*60*60 & data$timestamp<range.time[1]+i*24*60*60),dim(data)[2]]))
}

#quanti utenti si sono spostati al giorno
people.per.day <- NULL
for (i in 1:7){
  people.per.day[i] <- length(unique(data[which(data$timestamp>=range.time[1]+(i-1)*24*60*60 & data$timestamp<range.time[1]+i*24*60*60),2]))
}

#distanza percorsa al giorno
dist.per.day <- rep(0,7)
for (i in 1:7){
  journeys <- unique(data[which(data$timestamp>=range.time[1]+(i-1)*24*60*60 & data$timestamp<range.time[1]+i*24*60*60),dim(data)[2]])
  for(j in 1:length(journeys)){
    dist.per.day[i] <- dist.per.day[i] + sum(data[which(data$journey_id == journeys[j]),8])
  }
}

#distanza percorsa al giorno per ogni persona
dist.per.day / people.per.day #troppo alta 

#velocità dei viaggi (eventualmente si può fare anche caso functional: selezioniamo dei viaggi a caso (oppure gli scegliamo secondo certi criteri tipo giorno/notte ...) e calcoliamo la velocità istanzanea nei vari istanti di tempo)
journeys <- unique(data.red$journey_id)
space <-  NULL
time <- NULL
for(i in 1:length(journeys)){
  space[i] <- sum(data[which(data$journey_id == journeys[i]),8])
  time[i] <- sum(data[which(data$journey_id == journeys[i]),7])
}
vel <- space/time #velocità media di ogni viaggio

#tempo medio al giorno dei viaggi
time.per.day <- rep(0,7)
for (i in 1:7){
  jour <- unique(data[which(data$timestamp>=range.time[1]+(i-1)*24*60*60 & data$timestamp<range.time[1]+i*24*60*60),dim(data)[2]])
  for(j in 1:length(jour)){
    time.per.day[i] <- time.per.day[i] + sum(data[which(data$journey_id == jour[j]),7])
    if(j==length(jour))
      time.per.day[i] <- time.per.day[i]/length(jour)
  }
}
#anche la durata media dei viaggi sembra un po' alta
