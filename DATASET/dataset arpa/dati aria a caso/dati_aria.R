library(lubridate)
library(dplyr)
library(stringr)
data<- read.table('dati.aria.csv', header = TRUE, sep = ';')
data$data.ok <-str_replace_all(data$data, "[TZ]", " ")
#data$data.ok <-strptime(data$data.ok, format="%d-%m-%Y %H:%M:%S")
#data$data.ok <-as.POSIXct(as.character(data$data, format="%d-%m-%Y %H:%M:%S"))
#data <- data %>% filter(month(data[,5])==2 & year(data[,5])<=23 & year(data[,5])>=17)%>%group_by(data$data.ok)
#data <- filter(data, month(data[,5])==2 & year(data[,5])<=23 & year(data[,5])>=17)
data <- data[which(month(data[,5])==2),]
data <- data[which(year(data[,5])<=23 & year(data[,5])>=17),]
data <- data[is.na(data$valore)==FALSE,]
data$Municipio <- rep(0,dim(data)[1])
data$Municipio[which(data$stazione_id==2 | data$stazione_id==5|data$stazione_id==9)] <- 3
data$Municipio[which(data$stazione_id==3)] <- 6
data$Municipio[which(data$stazione_id==4)] <- 9
data$Municipio[which(data$stazione_id==6 | data$stazione_id==7)] <- 1
data$Municipio[which(data$stazione_id==8)] <- 7
NO2 <- data[which(data$inquinante=='NO2'),-2]
PM10 <- data[which(data$inquinante=='PM10'),-2]
SO2 <- data[which(data$inquinante=='SO2'),-2] #unico elemento di cui abbiamo dati tutti i giorni
write.csv(SO2, 'SO2_cittàstudi.csv')
