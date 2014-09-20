## Eric Lambert
## September 19, 2014


## QB histogram

setwd("/home/eric/Fantasy Football Project")
library(xlsx)
data<- read.xlsx("qbdata.xlsx", sheetIndex=1)

png("AgeHistogramQB.png", width=480, height=480)
hist(subset(data, age>25)$agebestyear, col="grey", xlab="Age", main="QB Best Years", breaks = 22:37)


dev.off()
