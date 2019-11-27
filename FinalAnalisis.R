library(readxl)

data <- read_excel("world_cup_results.xlsx")

colnames(data)


dataCCA <- data[,c(1,7,8,9,10,11)]



dataPrueba2 <- dataCCA  %>% group_by(Year)


length(which(dataPrueba2$Year==2014))
  
dataPrueba25 <- summarize(dataPrueba, numPartidos = count())


dataPrueba3 <- filter(dataCCA, HomeTeam =="Brazil" | AwayTeam=="Brazil" )
dataPrueba3


dataTest <- summarise(dataPrueba2, HomeGoals = sum(HomeGoals),  AwayGoals = sum(AwayGoals))
dataTest


JuegosMundial <- matrix(c(1930,1934,1938,1950,1954,1958,1962,1966,1970,1974,1978,1982,1986,1990,1994,1998,2002,2004,2006,2010,2014,18,17,18,22,26,35,32,32,32,38,38,52,52,52,52,64,64,64,64),20,2,byrow = F)

JuegosMundial
