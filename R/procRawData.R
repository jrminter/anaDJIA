# procRawData.R
rm(list=ls())
# library(quantmod)
library(xts)
# library(rFinFuncs)
# setClass('myDate')
setwd("~/git/anaDJIA/R")

makeCorrectDate <- function(str){
  b <- unlist(strsplit(str, "/"))
  c <- paste0(b[3],"/",b[1],"/",b[2])
  c
}

bSavePlots = FALSE
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


do.plot <- function(x,
                    bSave,
                    nBrks=250,
                    brMult=1.0,
                    legend.loc='topright',
                    scale.mult=1.1,
                    plt.median=TRUE){
  
  plot(x,
     main="^DJI",
     xlab="Date",
     ylab="Daily Return")
  if(bSave){
    pdf("../knitr/inc/pdf/dji-daily-ret.pdf",width=7,height=5)
    plot(x, main="^DJI", xlab="Date", ylab="Daily Return")
    dev.off()
  }

  h <- hist(x, breaks=nBrks, plot=FALSE)
  v.med <- median(x)
  v.mu  <- mean(x)
  v.sd  <- sd(x)
  d.max <- max(h$density)
  hist(x, breaks=nBrks, main=NULL, probability=TRUE,
       ylim=c(0, scale.mult*d.max), xlab="DJIA daily returns")
  if(plt.median){
    x.t <- c(v.med, v.med)
    y.t <- c(0, d.max)
    lines(x.t, y.t, col='blue', lw=2)
  }
  kern.bw <- brMult * bw.nrd0(djia.ret)
  lines(density(djia.ret, kern.bw), col='red')
  legend(x=legend.loc, c('data','kernel density'),
         lty=c(1,1),col=c('black', 'red'))
  if(bSave){
    pdf("../knitr/inc/pdf/dji-daily-ret-histo.pdf",width=7,height=5)
    h <- hist(x, breaks=nBrks, plot=FALSE)
    v.med <- median(x)
    v.mu  <- mean(x)
    v.sd  <- sd(x)
    d.max <- max(h$density)
    hist(x, breaks=nBrks, main=NULL, probability=TRUE,
         ylim=c(0, scale.mult*d.max), xlab="DJIA daily returns")
    if(plt.median){
      x.t <- c(v.med, v.med)
      y.t <- c(0, d.max)
      lines(x.t, y.t, col='blue', lw=2)
    }
    kern.bw <- brMult * bw.nrd0(djia.ret)
    lines(density(djia.ret, kern.bw), col='red')
    legend(x=legend.loc, c('data','kernel density'),
           lty=c(1,1),col=c('black', 'red'))
    dev.off()
  }

  hist(x, breaks=nBrks, main=NULL, probability=TRUE,
       ylim=c(0, 0.02*scale.mult*d.max), xlab="DJIA daily returns")
  if(plt.median){
    x.t <- c(v.med, v.med)
    y.t <- c(0, d.max)
    lines(x.t, y.t, col='blue', lw=2)
  }
  lines(density(x, kern.bw), col='red')
  legend(x=legend.loc, c('data','kernel density'),
         lty=c(1,1),col=c('black', 'red'))
  
  if(bSave){
    pdf("../knitr/inc/pdf/dji-daily-ret-histo-lo.pdf",width=7,height=5)
    hist(x, breaks=nBrks, main=NULL, probability=TRUE,
         ylim=c(0, 0.02*scale.mult*d.max), xlab="DJIA daily returns")
    if(plt.median){
      x.t <- c(v.med, v.med)
      y.t <- c(0, d.max)
      lines(x.t, y.t, col='blue', lw=2)
    }
    lines(density(x, kern.bw), col='red')
    legend(x=legend.loc, c('data','kernel density'),
           lty=c(1,1),col=c('black', 'red'))
    dev.off()
  }
  
  y <- x$djia.dr + v.mu
  y <- y/v.sd
  
  qqnorm(y, col='black', main='DJIA daily returns',
         xlab='Theoretical Quantiles',
         ylab='Sample Quantiles',
          pch=19, cex=.5, #) , xlim=c(-25,25),
         ylim=c(-20,20))
  qqline(y, col='red', lw=2)
  if(bSave){
    pdf("../knitr/inc/pdf/dji-daily-ret-qq.pdf",width=7,height=5)
    qqnorm(y, col='black', main='DJIA daily returns',
           xlab='Theoretical Quantiles',
           ylab='Sample Quantiles',
           pch=19, cex=.5, #) , xlim=c(-25,25),
           ylim=c(-20,20))
    qqline(y, col='red', lw=2)
    dev.off()
  }
}

do.plot(dji.dr, bSavePlots)
# save the dataframe, but we don't track it
save(dji.dr, file='../dat/dji.dr.RData')