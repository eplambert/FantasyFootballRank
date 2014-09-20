## Eric Lambert
## September 19, 2014

## This code is written to attempt to predict the best available running back in fantasy football

setwd("/home/eric/Fantasy Football Project")
file <- read.csv("fantasyData.csv", sep=";", header=TRUE)

file$age<- c(29,27,28,27,25,27,27,23,25,26,31,28,25,31,28,29,26,24,26,31,29,23,23,26,26,31,22,25,24,22,25,33,27,28,23,27,29,25,24,29,29,24,23,27,27,28,22,28,25,29)
file$TeamRushRank13 <- c(8,20,4,30,22,10,2,20,5,1,32,16,9,3,6,12,12,7,24,1,17,26,15,6,13,11,18,9,25,27,27,2,26,17,19,31,29,23,25,25,13,30,4,27,13,14,10,7,5,18)
file$TD13<- c(10,1,12,4,1,12,2,3,7,9,6,9,7,9,6,5,5,11,9,2,4,2,4,3,6,3,5,1,1,8,4,9,10,8,7,2,6,3,1,2,2,2,0,7,6,4,4,3,4,7)

data<- cbind(file, Rating=0)
numPlayers<-nrow(data)

##Re-order the rows and apply ranks
##Attributes are:
##      1) Number of offensive plays in 2013            --> numTeamPlays13
##      2) Number of games the player participated in   --> gamesplayed13
##      3) Number of attempted rushes in 2013           --> numRush13
##      4) Number of receptions in 2013                 --> numCatch13
##      5) Number of targets in 2013                    --> target13
##      6) Amount of rushing yards in 2013              --> rushyds13
##      7) Rushing yards/Games played in 2013           --> rushydspergame13
##      8) Fraction of rushes versus total plays        --> rushperplay13
##      9) Fraction of catches versus total plays       --> catchperplay13
##      10) Fraction of catches versus number of targets--> catchpertarget13
##      11) Players between the age of 22-26 are likely to have a good season   = +2
##              Players between 27-29 are less likely                           = 0
##              Players over 30 are not likely to have a good season            = -2
##      12) Players playing for a team who have a good rushing attack get a boost
##              Ranked: 1-5     = +2
##              Ranked: 6-10    = +1
##              Ranked: 11-15   = 0
##              Ranked: 16-20   = -1
##              Ranked: >21     = -2
##      13) Ranking based on the normalized value of touchdowns scored

## Ranking the number of plays a team makes
meanplays <- mean(data$numTeamPlays13)
for (i in 1:numPlayers) {
        data$Rating[i] <- data$Rating[i] + data$numTeamPlays13[i]/meanplayss 
}


## If number of games played divided by the mean total of games played
meangames <- mean(data$gamesplayed13)
for (i in 1:numPlayers) {
        data$Rating[i] <- data$Rating[i] + data$gamesplayed13[i]/meangames 
}

## Ranking the number of rushes a player made
meanrush <- mean(data$numRush13)
for (i in 1:numPlayers) {
        data$Rating[i] <- data$Rating[i] + data$numRush13[i]/meanrush 
}

## Ranking the number of catches a player made
meancatch <- mean(data$numCatch13)
for (i in 1:numPlayers) {
        data$Rating[i] <- data$Rating[i] + data$numCatch1313[i]/meancatch 
}

## Ranking the number of targets a player gets
meantarget <- mean(data$target13)
for (i in 1:numPlayers) {
        data$Rating[i] <- data$Rating[i] + data$target13[i]/meantarget 
}

## Ranking the rushing yards a player made
meanrushyds <- mean(data$rushyds13)
for (i in 1:numPlayers) {
        data$Rating[i] <- data$Rating[i] + data$rushyds13[i]/meanrushyds 
}

## Ranking the rushing yards per game for a player
meanrypg <- mean(data$rushydspergame13)
for (i in 1:numPlayers) {
        data$Rating[i] <- data$Rating[i] + data$rushydspergame13[i]/meanrypg 
}

## Ranking a players rushes per game
meanrpp <- mean(data$rushperplay13)
for (i in 1:numPlayers) {
        data$Rating[i] <- data$Rating[i] + data$rushperplay13[i]/meanrpp 
}

## Ranking the catch per play for a player
meancpp <- mean(data$catchperplay13)
for (i in 1:numPlayers) {
        data$Rating[i] <- data$Rating[i] + data$catchperplay13[i]/meancpp
}

## Ranking catches per target
meancpt <- mean(data$catchpertarget13)
for (i in 1:numPlayers) {
        data$Rating[i] <- data$Rating[i] + data$catchpertarget13[i]/meancpt 
}

## Handles ranking for player ages
for (i in 1:numPlayers) {
        if (data$age[i]<26) {
                data$Rating[i]<-data$Rating[i]+2
        } else if (file$age[i]>29) {
                data$Rating[i]<-data$Rating[i]-2
        } else {
                data$Rating[i] <- data$Rating[i]
        }
}

## Handles ranking for players on good rushing teams
for (i in 1:numPlayers) {
        if (data$TeamRushRank13[i] < 6) {
                data$Rating[i]<-data$Rating[i]+2
        } else if (data$TeamRushRank13[i]>=6 & file$TeamRushRank13[i]<11) {
                data$Rating[i]<-data$Rating[i]+1
        } else if (data$TeamRushRank13[i]>=11 & file$TeamRushRank13[i]<16) {
                data$Rating[i]<-data$Rating[i]
        } else if (data$TeamRushRank13[i]>=16 & file$TeamRushRank13[i]<21) {
                data$Rating[i]<-data$Rating[i]-1
        } else {
                data$Rating[i]<-data$Rating[i]-2
        }
}

## Ranking the number of touchdowns scored in 2013
meantd <- mean(data$TD13)
for (i in 1:numPlayers) {
        data$Rating[i] <- data$Rating[i] + data$TD13[i]/meantd
}

## Ray Rice was suspended for two games
games <- 2
data$Rating[4] <- data$Rating[4]-games

data<-data[order(data$Rating, na.last=TRUE, decreasing=TRUE),]          ## Data is now ordered with highest rated

## Print Results into a table
library(gridExtra)
picks <- data.frame(Player=data$Player, Score=data$Rating, Rank=1:numPlayers, ESPN.Rank=data$X2014.rank)
pdf("data_output.pdf", height=15, width=6)
grid.table(picks)
dev.off()

espn.ranking <- data.frame(Player=file$Player, ESPN.Rank.2013=file$X2013.rank, ESPN.Rank.2014=file$X2014.rank)
pdf("espnranks.pdf", height=15, width=6)
grid.table(espn.ranking)
dev.off()
