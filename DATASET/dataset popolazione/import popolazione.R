data <- read.table("C:/Users/E5440/Desktop/esami/Nonparametric statistics/Progetto/dataset popolazione/dati_popolazione_lombardia_2020.csv", header = TRUE, sep=',')
data <- data[,-c(1,3,4,5,6,7,9,11,12,14,15)]
data<- data[,-2]
