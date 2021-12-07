brera <- read.table('C:/Users/E5440/Desktop/esami/Nonparametric statistics/Progetto/dataset meteo/Brera.csv', header=TRUE, sep=',')
brera <- brera[49:216,]
juvara <- read.table('C:/Users/E5440/Desktop/esami/Nonparametric statistics/Progetto/dataset meteo/Juvara.csv', header=TRUE, sep=',')
juvara <- juvara[49:216,]
lambrate <- read.table('C:/Users/E5440/Desktop/esami/Nonparametric statistics/Progetto/dataset meteo/Lambrate.csv', header=TRUE, sep=',')
lambrate <- lambrate[49:216,]
rosellini <- read.table('C:/Users/E5440/Desktop/esami/Nonparametric statistics/Progetto/dataset meteo/Rosellini.csv', header=TRUE, sep=',')
rosellini <- rosellini[49:216,]
zavattari <- read.table('C:/Users/E5440/Desktop/esami/Nonparametric statistics/Progetto/dataset meteo/Zavattari.csv', header=TRUE, sep=',')
zavattari <- zavattari[49:216,]

data <- cbind(brera[,3], juvara[,3], lambrate[,3], rosellini[,3], zavattari[,3])
colnames(data) <- c('Brera', 'Juvara', 'Lambrate', 'Rosellini','Zavattari' )
