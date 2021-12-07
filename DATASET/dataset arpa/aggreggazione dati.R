benzene.senato <- read.table("C:/Users/E5440/Desktop/esami/Nonparametric statistics/Progetto/dataset arpa/milano senato/benzene.csv", header=TRUE, sep=',')
benzene.sen <- NULL
for (i in 0:6) {
  benzene.sen[i+1] <- mean(benzene.senato[((i*24)+1):((i+1)*24),2])
}
biossido.senato <- read.table("C:/Users/E5440/Desktop/esami/Nonparametric statistics/Progetto/dataset arpa/milano senato/biossido di azoto.csv", header=TRUE, sep=',')
biossido.sen <- NULL
for (i in 0:6) {
  biossido.sen[i+1] <- mean(biossido.senato[((i*24)+1):((i+1)*24),2])
}
pm10.sen <- read.table("C:/Users/E5440/Desktop/esami/Nonparametric statistics/Progetto/dataset arpa/milano senato/PM10.csv", header=TRUE, sep=',')


benzene.studi <- read.table("C:/Users/E5440/Desktop/esami/Nonparametric statistics/Progetto/dataset arpa/milano città studi/benzene.csv", header=TRUE, sep=',')
benzene.st <- NULL
for (i in 0:6) {
  benzene.st[i+1] <- mean(benzene.studi[((i*24)+1):((i+1)*24),2])
}
biossido.studi <- read.table("C:/Users/E5440/Desktop/esami/Nonparametric statistics/Progetto/dataset arpa/milano città studi/biossido di azoto.csv", header=TRUE, sep=',')
biossido.st <- NULL
for (i in 0:6) {
  biossido.st[i+1] <- mean(biossido.studi[((i*24)+1):((i+1)*24),2])
}
pm10.st <- read.table("C:/Users/E5440/Desktop/esami/Nonparametric statistics/Progetto/dataset arpa/milano città studi/PM10.csv", header=TRUE, sep=',')


data <- cbind(benzene.sen,biossido.sen, pm10.sen[,2], benzene.st, biossido.st, pm10.st[2])
colnames(data) <- c('Benzene Senato', 'Biossido Senato', 'PM10 Senato', 'Benzene città studi', 'Biossido città studi', 'PM10 città studi')
