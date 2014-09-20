## Eric Lambert
## September 19, 2014

## Plotting RB players with atleast 3 & 4 years experience

setwd("/home/eric/Fantasy Football Project")
library(xlsx)
data<- read.xlsx("AgeData.xlsx", sheetIndex=1)

png("AgeHistogram.png", width=480, height=480)
par(mfrow=c(2,1), mar=c(4,4,2,1))
hist(subset(data, Years.in.NFL>2)$Age.Best.Year, col="red", xlab="Age", main="3+ years Exp")
hist(subset(data, Years.in.NFL>3)$Age.Best.Year, col="red", xlab="Age", main="4+ years Exp")

dev.off()
