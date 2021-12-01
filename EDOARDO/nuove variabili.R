setwd('C:/Users/E5440/Desktop/esami/Nonparametric statistics/Progetto')
data <- read.table('dataset.csv', header=TRUE, sep=',')
data <- data[,-dim(data)[2]]
data[which(is.na(data[,7])==TRUE),7] <- 0
data[which(is.na(data[,8])==TRUE),8] <- 0

#bisogna mettere a 0 i timedelta e spacedelta di tutte le prime osservazioni di ciascun journey_id
j.id <- unique(data$journey_id) #dataset ordinato per journey ID
j.start <- 1
j.fin <-1
first <- NULL
for(i in 1:length(j.id)){
  while(data$journey_id[j.fin] == j.id[i] & j.fin < dim(data)[1]){
    j.fin<-j.fin+1
  }
  first <- c(first, j.start)
  j.start <- j.fin
  }
data[first,7] <- 0.00001
data[first,8] <- 0
vel <- data[,8]/data[,7] 
length(which(vel>200/3.6))/length(vel)
#tolgo viaggi con velocità istantanea superiore a 200 km/h
j.da.scartare <- unique(data[which(vel>200/3.6),10])
ii <- NULL
for (i in 1:length(j.da.scartare)) {
  ii <- c(ii, which(data$journey_id == j.da.scartare[i]))
}
length(ii)/length(vel) #circa il 17% delle osservazioni sono relative a viaggi in cui si osserva almeno una volta vel > 200 km/h
data <- data[-ii,]
vel <- data[,8]/data[,7]
library(stringr)
dataset <- data
dataset$datetime_correct <-str_replace_all(dataset$datetime, "[TZ]", " ")
dataset$datetime_correct <-strptime(dataset$datetime_correct, format="%Y-%m-%d %H:%M:%S")

write.csv(dataset, 'dataset.definitivo.csv')












#data <- data[1:4000,] #dataset completo troppo pesante

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
dist.per.day / people.per.day 

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





