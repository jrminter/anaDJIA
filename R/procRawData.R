# procRawData.R
rm(list=ls())
library(quantmod)
library(xts)
library(rFinFuncs)
setClass('myDate')
setwd("~/git/anaDJIA/R")

makeCorrectDate <- function(str){
  b <- unlist(strsplit(str, "/"))
  c <- paste0(b[3],"/",b[1],"/",b[2])
  c
}


strCsv <- "../dat/csv/DJA.csv"
dat <- read.csv(strCsv, skip=4, header=TRUE,  quote = "\"", colClasses=c("character", "character"))
print(head(dat))

cDate <- sapply(dat$Date, makeCorrectDate)

djia <- as.numeric(dat$DJIA)
n <- length(djia)
print(n)
djia.ret <- djia[2:n]/djia[1:n-1]-1
print(head(djia.ret))
print(tail(djia.ret))

print(head(cDate))
print(tail(cDate))

df <- data.frame(djia.dr=as.numeric(djia.ret))
rownames(df) <- cDate[2:n]
print(head(df))
print(tail(df))

dji.dr <- as.xts(df)
print(class(dji.dr))
print(head(dji.dr))
print(tail(dji.dr))

plot(dji.dr,
     main="^DJI",
     xlab="Date",
     ylab="Daily Return")

dev.copy(png,'../knitr/inc/png/dji-daily-ret.png',
         width=1024, height=512)
dev.off()

# save the dataframe, but we don't track it
save(dji.dr, file='../dat/dji.dr.RData')

