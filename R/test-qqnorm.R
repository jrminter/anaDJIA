set.seed(42)
setwd("~/git/anaDJIA/R")

bSave <- TRUE

n <- 35835
# let's use an equivalent normal distribution for quantiles.
# n <- 1000
mu <- 0.000233479
s <- 0.01063935

x <- rnorm(n, mean=mu, sd=s)
# qqnorm(x); qqline(x)

y <- x+mu
y <- y/s

do.plot <- function()
{
  qqnorm(y, pch=19, cex=0.5); qqline(y)
}

do.plot()

if(bSave){
  pdf("../knitr/inc/pdf/normal-qq.pdf", width=7, height=5)
  do.plot()
  dev.off()
}