#LOADING LIBRARIES----------------------------------------------------------------------------------------------
library(readxl)
library(dplyr)

#LOADING DATA--------------------------------------------------------------------------------------------------
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

#GOALS BY COUNTRIES---------------------------------------------------------------------------------
#HOME GOALS 
Ecuador_H <- as.data.frame( dataCCA[ dataCCA$HomeTeam == "Ecuador", ] )
Brazil_H <- as.data.frame( dataCCA[ dataCCA$HomeTeam == "Brazil", ] )
Uruguay_H <- as.data.frame( dataCCA[ dataCCA$HomeTeam == "Uruguay", ] )
Germany_H <- as.data.frame( dataCCA[ dataCCA$HomeTeam == "Germany", ] )
Italy_H <- as.data.frame( dataCCA[ dataCCA$HomeTeam == "Italy", ] )
France_H <- as.data.frame( dataCCA[ dataCCA$HomeTeam == "France", ] )
#AWAY GOALS 
Ecuador_A <- as.data.frame( dataCCA[ dataCCA$AwayTeam == "Ecuador", ] )
Brazil_A <- as.data.frame( dataCCA[ dataCCA$AwayTeam == "Brazil", ] )
Uruguay_A <- as.data.frame( dataCCA[ dataCCA$AwayTeam == "Uruguay", ] )
Germany_A <- as.data.frame( dataCCA[ dataCCA$AwayTeam == "Germany", ] )
Italy_A <- as.data.frame( dataCCA[ dataCCA$AwayTeam == "Italy", ] )
France_A <- as.data.frame( dataCCA[ dataCCA$HomeTeam == "France", ] )
names( Italy_A ) <- c("Year", "Country",   "HomeTeam",  "HomeGoals", "AwayGoals", "AwayTeam")
Italy_A
Ecuador_H <- select( Ecuador_H, Year, Country, HomeGoals, AwayGoals )
names( Ecuador_H ) <- c( "Year", "Country", "GoalsFavor", "GoalsContra" )
Ecuador_H
Ecuador_A <-Ecuador_A[ , c( 1, 2, 5, 4 ) ]
names( Ecuador_A ) <- c( "Year", "Country", "GoalsFavor", "GoalsContra" )
Ecuador_A

#ECUADOR GOALS--------------------------------------------------------------------------------------
Ec_Goals <- rbind( Ecuador_H, Ecuador_A )
Ec_Goals

#---------------------------------------------------------------------------------------------------
Brazil_H <- select( Brazil_H, Year, Country, HomeGoals, AwayGoals )
names( Brazil_H ) <- c( "Year", "Country", "GoalsFavor", "GoalsContra" )
Brazil_H
Brazil_A <- Brazil_A[ , c( 1, 2, 5, 4 ) ]
names( Brazil_A ) <- c( "Year", "Country", "GoalsFavor", "GoalsContra" )
Brazil_A
#BRAZIL GOALS---------------------------------------------------------------------------------------
Br_Goals <- rbind( Brazil_H, Brazil_A )
Br_Goals
#---------------------------------------------------------------------------------------------------
Germany_H <- select( Germany_H, Year, Country, HomeGoals, AwayGoals )
names( Germany_H ) <- c( "Year", "Country", "GoalsFavor", "GoalsContra" )
Germany_H
Germany_A <- select( Germany_A, Year, Country, AwayGoals, HomeGoals )
names( Germany_A ) <- c( "Year", "Country", "GoalsFavor", "GoalsContra" )
Brazil_A
#GERMANY GOALS--------------------------------------------------------------------------------------
Ger_Goals <- rbind( Germany_H, Germany_A )
Ger_Goals
#---------------------------------------------------------------------------------------------------
Italy_H <- select( Italy_H, Year, Country, HomeGoals, AwayGoals )
names( Italy_H ) <- c( "Year", "Country", "GoalsFavor", "GoalsContra" )
Italy_H
Italy_A <- Italy_A[, c(1, 2 , 5, 4 ) ]
names( Italy_A ) <- c( "Year", "Country", "GoalsFavor", "GoalsContra" )
Italy_A
#ITALY GOALS----------------------------------------------------------------------------------------
It_Goals <- rbind( Italy_H, Italy_A )
It_Goals
#---------------------------------------------------------------------------------------------------
Uruguay_H <- select( Uruguay_H, Year, Country, HomeGoals, AwayGoals )
names( Uruguay_H ) <- c( "Year", "Country", "GoalsFavor", "GoalsContra" )
Uruguay_H
Uruguay_A <- select( Uruguay_A, Year, Country, AwayGoals, HomeGoals )
names( Uruguay_A ) <- c( "Year", "Country", "GoalsFavor", "GoalsContra" )
Uruguay_A
#URUGUAY GOALS--------------------------------------------------------------------------------------
Ur_Goals <- rbind( Uruguay_H, Uruguay_A )
Ur_Goals

#---------------------------------------------------------------------------------------------------
France_H <- select( France_H, Year, Country, HomeGoals, AwayGoals )
names( France_H ) <- c( "Year", "Country", "GoalsFavor", "GoalsContra" )
France_H
France_A <- select( France_A, Year, Country, AwayGoals, HomeGoals )
names( France_A ) <- c( "Year", "Country", "GoalsFavor", "GoalsContra" )
France_A
#GERMANY GOALS--------------------------------------------------------------------------------------
Fra_Goals <- rbind( France_H, France_A )
Fra_Goals
#FINAL GOALS----------------------------------------------------------------------------------------
Goals <- matrix( 0, 2, 6 )
Goals[ 1, 1 ] <- sum( Ec_Goals[ 1:4, 3 ] )
Goals[ 2, 1 ] <- sum( Ec_Goals[ 5:10, 3 ] )
Goals[ 1, 2 ] <- sum( Br_Goals[ 1:82, 3 ] )
Goals[ 2, 2 ] <- sum( Br_Goals[ 83:108, 3 ] )
Goals[ 1, 3 ] <- sum( Ger_Goals[ 1:32, 3 ] )
Goals[ 2, 3 ] <- sum( Ger_Goals[ 33:48, 3 ] )
Goals[ 1, 4 ] <- sum( It_Goals[ 1:57, 3] )
Goals[ 2, 4 ] <- sum( It_Goals[ 58:83, 3] )
Goals[ 1, 5 ] <- sum( Ur_Goals[ 1:28, 3 ] )
Goals[ 2, 5 ] <- sum( Ur_Goals[ 29:52, 3 ] )
Goals[ 1, 6 ] <- sum( Fra_Goals[ 1:31, 3 ] )
Goals[ 2, 6 ] <- sum( Fra_Goals[ 32:62, 3 ] )

colnames( Goals ) <- c( "Ecuador", "Brazil", "Germany", "Italy", "Uruguay", "France" )
rownames( Goals ) <- c( "Home Favor Goals", "Away Favor Goals" )
Goals

GoalsContra <- matrix( 0, 2, 6 )
GoalsContra[ 1, 1 ] <- sum( Ec_Goals[ 1:4, 4 ] )
GoalsContra[ 2, 1 ] <- sum( Ec_Goals[ 5:10, 4 ] )
GoalsContra[ 1, 2 ] <- sum( Br_Goals[ 1:82, 4 ] )
GoalsContra[ 2, 2 ] <- sum( Br_Goals[ 83:108, 4 ] )
GoalsContra[ 1, 3 ] <- sum( Ger_Goals[ 1:32, 4 ] )
GoalsContra[ 2, 3 ] <- sum( Ger_Goals[ 33:48, 4 ] )
GoalsContra[ 1, 4 ] <- sum( It_Goals[ 1:57, 4 ] )
GoalsContra[ 2, 4 ] <- sum( It_Goals[ 58:83, 4 ] )
GoalsContra[ 1, 5 ] <- sum( Ur_Goals[ 1:28, 4 ] )
GoalsContra[ 2, 5 ] <- sum( Ur_Goals[ 29:52, 4 ] )
GoalsContra[ 1, 6 ] <- sum( Fra_Goals[ 1:31, 4 ] )
GoalsContra[ 2, 6 ] <- sum( Fra_Goals[ 32:62, 4 ] )
colnames( GoalsContra ) <- c( "Ecuador", "Brazil", "Germany", "Italy", "Uruguay", "France" )
rownames( GoalsContra ) <- c( "Home Against Goals", "Away Against Goals" )
GoalsContra 
#SEDE MUNDIAL---------------------------------------------------------------------------------------

Sede <- matrix( c( 0, 2, 2, 2, 1, 2 ), 1, 6 )
colnames( Sede ) <- c( "Ecuador", "Brazil", "Germany", "Italy", "Uruguay", "France" )
rownames( Sede ) <- "Sede"
Sede

#PARTICIPATION--------------------------------------------------------------------------------------
participation <- matrix( c( 3, 20, 18, 18, 12, 14 ), 1, 6 )
colnames( participation ) <- c( "Ecuador", "Brazil", "Germany", "Italy", "Uruguay",  "France" )
rownames( participation ) <- "Participation"
participation

Goals <- as.data.frame( Goals )
GoalsContra <- as.data.frame( GoalsContra )
Sede <- as.data.frame( Sede )
participation <- as.data.frame( participation )

Table <- rbind( Goals, GoalsContra )
Table <- rbind( Table, Sede )
Table1 <- rbind( Table, participation )
Table1
