rm(list = ls(all = T))
graphics.off()
library(SIS)
library(glmnet)
library(zoo)

lg1<-function(dy1,kk){
  dy1=t(dy1)
  rdy1=apply(dy1,2,rev)
  mxt1=NULL
  for (i in 1:(nrow(rdy1)-kk)){
    xt1=NULL
    for (j in 1:kk){
      xt1=c(xt1,rdy1[i+j,])
    }
    mxt1<-cbind(xt1,mxt1)
  }
  return(mxt1)
}

setwd("~/Dropbox/SIS/Codes/201708")

myp = read.csv("electricity_monthly.csv", header = F, sep = ";")
mydate = as.character(myp[, 1])
st = which(mydate == "30.09.2010")
et = which(mydate == "31.07.2015")
myp1 = myp[(st:et), 2:92]

myp1 = as.matrix(myp1[, -15], nr = 59, nc = 90)
myp1 = matrix(as.numeric(myp1),59, 90)

for (i in 1:nrow(myp1)) {
  for (j in 1:ncol(myp1)) {
    if (myp1[i, j] < 0.01) {
      myp1[i, j] = 0.01
    }
  }
}
rm(myp, st, et, i, j)
logr <- t(diff(log(myp1), lag=1))
sum(is.na(logr))

#VAR estimation
lagn = 10
lr1 = lg1(logr, lagn)
n1=ncol(lr1)
d1=nrow(logr)
y1=logr[,(ncol(logr)-n1+1):ncol(logr)]
cy1=c(y1)##y in lasso
kx1=t(lr1)%x%diag(d1)

fit1= SIS(kx1,cy1,family = "gaussian", penalty = "lasso", tune="bic")
save(fit1, file="lassolag10.RData")

