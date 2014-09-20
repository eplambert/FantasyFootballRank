## Eric Lambert
## September 19, 2014

## This code analyzes quarterbacks

setwd("/home/eric/Fantasy Football Project")
library(xlsx)
file <- read.xlsx("qbdata.xlsx", sheetIndex=1)



data<- cbind(file, Rating=0)
num <- nrow(data)

## Re-order players and apply rankings
##      1) games played
##      2) completetion percentage
##      3) passing yards
##      4) Touchdowns
##      5) rushing yards
##      6) tdpergame
##      7) yards per game
##      8) completetions per game
##      9) interceptions per game
##      10) attempts per game

## 1) Ranking games played
meangames <- mean(data$gp)
for (i in 1:num) {
        data$Rating[i] <- data$Rating[i] + data$gp[i]/meangames 
}

## 2) completetion percentage
meancmpp <- mean(data$cmppercent)
for (i in 1:num) {
        data$Rating[i] <- data$Rating[i] + data$comppercent[i]/meancmpp 
}

## 3) Passing yards
meanpyds <- mean(data$yds)
for (i in 1:num) {
        data$Rating[i] <- data$Rating[i] + data$yds[i]/meanpyds 
}

## 4) Touchdowns
meantd <- mean(data$td)
for (i in 1:num) {
        data$Rating[i] <- data$Rating[i] + data$td[i]/meantd 
}

## 5) Rushing yards
meanryds <- mean(data$rushyds)
for (i in 1:num) {
        data$Rating[i] <- data$Rating[i] + data$rushyds[i]/meanryds 
}

## 6) Touchdown per game
meantdpg <- mean(data$tdpergame)
for (i in 1:num) {
        data$Rating[i] <- data$Rating[i] + data$tdpergame[i]/meantdpg 
}

## 7) Yards per game
meanydpg <- mean(data$ydspergame)
for (i in 1:num) {
        data$Rating[i] <- data$Rating[i] + data$ydspergame[i]/meanydpg 
}

## 8) Completetions per game
meancpg <- mean(data$cmppergame)
for (i in 1:num) {
        data$Rating[i] <- data$Rating[i] + data$cmppergame[i]/meancpg 
}

## 9) Interceptions per game
meanint <- mean(data$intpergame)
for (i in 1:num) {
        data$Rating[i] <- data$Rating[i] - data$intpergame[i]/meanint
}

## 10) Attempts per game
meanatt <- mean(data$attpergame)
for (i in 1:num) {
        data$Rating[i] <- data$Rating[i] + data$attpergame[i]/meanatt 
}

data<-data[order(data$Rating, na.last=TRUE, decreasing=TRUE),]

## Print Results into a table
library(gridExtra)
picks <- data.frame(Player=data$Player, Score=data$Rating, Rank=1:num, ESPN.Rank=data$espn.rank)
pdf("qb_output.pdf", height=10, width=6)
grid.table(picks)
dev.off()
