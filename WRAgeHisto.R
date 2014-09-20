## Eric Lambert
## September 19, 2014

## WR histogram

setwd("/home/eric/Fantasy Football Project")
library(xlsx)
data<- read.xlsx("wrdata.xlsx", sheetIndex=1)

png("AgeHistogramWR.png", width=480, height=480)
par(mfrow=c(2,1), mar=c(4,4,2,1))
hist(subset(data, exp>2)$ageofbestyear, col="steelblue", xlab="Age", main="WR 3+ years Exp")
hist(subset(data, exp>3)$ageofbestyear, col="blue", xlab="Age", main="WR 4+ years Exp")

dev.off()
