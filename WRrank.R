## Eric Lambert
## September 19, 2014

## This code is to analyze and rank widereceivers and tightends
## Tight ends are treated as widereceivers

setwd("/home/eric/Fantasy Football Project")
library(xlsx)
filewr <- read.xlsx("wrdata.xlsx", sheetIndex=1)

rate<- cbind(filewr, Rating=0)
num <- nrow(rate)

## Re-order rows and apply ranks
## 13 attributes
##      1) Number of games player participated in
##      2) FDperGame
##      3) tdpergame
##      4) targetpergame
##      5) catchpergame
##      6) ydspergame
##      7) catchpertarget
##      8) offense rank
##      9) choice
##      10) longest reception
##      11) TDs
##      12) Age
##      13) Targets

## 1) rates players based on the number of games played
meangp <- mean(rate$gp)
for (i in 1:num) {
        rate$Rating[i] <- rate$Rating[i] + rate$gp[i]/meangp 
}
## 2) fdpergame
meanfd <- mean(rate$fdpergame)
for (i in 1:num) {
        rate$Rating[i] <- rate$Rating[i] + rate$fdpergame[i]/meanfd 
}
## 3) tdpergame
meantd <- mean(rate$tdpergame)
for (i in 1:num) {
        rate$Rating[i] <- rate$Rating[i] + rate$tdpergame[i]/meantd 
}
## 4) targetpergame
meantrgt <- mean(rate$targetpergame)
for (i in 1:num) {
        rate$Rating[i] <- rate$Rating[i] + rate$targetpergame[i]/meantrgt 
}
## 5) catchpergame
meancpg <- mean(rate$catchpergame)
for (i in 1:num) {
        rate$Rating[i] <- rate$Rating[i] + rate$catchpergame[i]/meancpg 
}
## 6) ydspergame
meanypg <- mean(rate$ydspergame)
for (i in 1:num) {
        rate$Rating[i] <- rate$Rating[i] + rate$ydspergame[i]/meanypg 
}
## 7) catchpertarget
meancpt <- mean(rate$catchpertarget)
for (i in 1:num) {
        rate$Rating[i] <- rate$Rating[i] + rate$catchpertarget[i]/meancpt 
}
## 8) teamoffenserank
##              Ranked: 1-5     = +2
##              Ranked: 6-10    = +1
##              Ranked: 11-15   = 0
##              Ranked: 16-20   = -1
##              Ranked: >21     = -2
for (i in 1:num) {
        if (rate$teamoffenserank[i]<6) {
                rate$Rating[i] <- rate$Rating[i] + 2  
        } else if (rate$teamoffenserank[i]>=6 & rate$teamoffenserank[i]<11) {
                rate$Rating[i] <- rate$Rating[i] + 1
        } else if (rate$teamoffenserank[i]>=11 & rate$teamoffenserank[i]<16) {
                rate$Rating[i] <- rate$Rating[i]
        } else if (rate$teamoffenserank[i]>=16 & rate$teamoffenserank[i]<21) {
                rate$Rating[i] <- rate$Rating[i] - 1
        } else {
                rate$Rating[i] <- rate$Rating[i]-2
        }
}
## 9) choice
##      If a WR is ranked as a
##              1st choice = +2
##              2nd choice = +1
##              3rd choice = 0
for (i in 1:num) {
        if (rate$choice[i]==1) {
                rate$Rating[i] <- rate$Rating[i]+2
        } else if (rate$choice[i]==2) {
                rate$Rating[i] <- rate$Rating[i]+1
        } else {
                rate$Rating[i] <- rate$Rating[i]
        }
}
## 10) Longest Reception
meanlr <- mean(rate$long)
for (i in 1:num) {
        rate$Rating[i] <- rate$Rating[i] + rate$long[i]/meanlr 
}
## 11) number of touchdowns
meanntd <- mean(rate$td)
for (i in 1:num) {
        rate$Rating[i] <- rate$Rating[i] + rate$td[i]/meanntd 
}
## 12) Handles Age of players:
##      Age: 22-26 = +2
##      Age: <22 OR >26 AND <28 = +1
##      Age: >29 = 0
for (i in 1:num) {
        if (rate$age[i]>21 & rate$age[i]<27) {
                rate$Rating[i] <- rate$Rating[i]+2
        } else if (rate$age[i]<22 | (rate$age[i]>26 & rate$age[i]<29)) {
                rate$Rating[i] <- rate$Rating[i]+1
        } else {
                rate$Rating[i] <- rate$Rating[i]
        }
}
## 13) Targets
meantar <- mean(rate$target)
for (i in 1:num) {
        rate$Rating[i] <- rate$Rating[i] + rate$target[i]/meantar 
}

data<-rate[order(rate$Rating, na.last=TRUE, decreasing=TRUE),]

## Print Results into a table
library(gridExtra)
picks <- data.frame(Player=data$Player, Score=data$Rating, Rank=1:num, ESPN.Rank=data$espn.rank)
pdf("wr_output.pdf", height=25, width=6)
grid.table(picks)
dev.off()
