rm(list = ls(all = T))
graphics.off()
library(SIS)
library(glmnet)
library(zoo)
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
logr <- diff(log(myp1), lag=1)
sum(is.na(logr))




#VAR estimation
lagn = 5
# construct VAR model initial lag = 5
y1 = t(logr[(lagn+1):nrow(logr), ])
# z11 = rep(1, 53) # vector of intercept terms
z12 = t(logr[lagn:(nrow(logr)-1), ])
z13 = t(logr[(lagn-1):(nrow(logr)-2), ])
z14 = t(logr[(lagn-2):(nrow(logr)-3), ])
z15 = t(logr[(lagn-3):(nrow(logr)-4), ])
z16 = t(logr[(lagn-4):(nrow(logr)-5), ])#lag is 5
# z1 = rbind(z11, z12, z13, z14, z15, z16)
z1 = rbind(z12, z13, z14, z15, z16)
# vectorization
vecy = c(y1)
vecz = t(z1) %x% diag(90)  # kronecker product
fit1 = SIS(vecz, vecy, family = "gaussian", penalty = "lasso", tune="bic")
save(fit1, file="lassolag5.RData")


#####other choice of lags
#VAR estimation
lagn = 4
# construct VAR model initial lag = 5
y1 = t(logr[(lagn+1):nrow(logr), ])
# z11 = rep(1, 53) # vector of intercept terms
z12 = t(logr[lagn:(nrow(logr)-1), ])
z13 = t(logr[(lagn-1):(nrow(logr)-2), ])
z14 = t(logr[(lagn-2):(nrow(logr)-3), ])
z15 = t(logr[(lagn-3):(nrow(logr)-4), ])
# z1 = rbind(z11, z12, z13, z14, z15, z16)
z1 = rbind(z12, z13, z14, z15)
# vectorization
vecy = c(y1)
vecz = t(z1) %x% diag(90)  # kronecker product
fit1 = SIS(vecz, vecy, family = "gaussian", penalty = "lasso", tune="bic")
save(fit1, file="lassolag4.RData")

#VAR estimation
lagn = 3
# construct VAR model initial lag = 5
y1 = t(logr[(lagn+1):nrow(logr), ])
# z11 = rep(1, 53) # vector of intercept terms
z12 = t(logr[lagn:(nrow(logr)-1), ])
z13 = t(logr[(lagn-1):(nrow(logr)-2), ])
z14 = t(logr[(lagn-2):(nrow(logr)-3), ])
# z1 = rbind(z11, z12, z13, z14, z15, z16)
z1 = rbind(z12, z13, z14)
# vectorization
vecy = c(y1)
vecz = t(z1) %x% diag(90)  # kronecker product
fit1 = SIS(vecz, vecy, family = "gaussian", penalty = "lasso", tune="bic")
save(fit1, file="lassolag3.RData")

#VAR estimation
lagn = 2
# construct VAR model initial lag = 5
y1 = t(logr[(lagn+1):nrow(logr), ])
# z11 = rep(1, 53) # vector of intercept terms
z12 = t(logr[lagn:(nrow(logr)-1), ])
z13 = t(logr[(lagn-1):(nrow(logr)-2), ])
# z1 = rbind(z11, z12, z13, z14, z15, z16)
z1 = rbind(z12, z13)
# vectorization
vecy = c(y1)
vecz = t(z1) %x% diag(90)  # kronecker product
fit1 = SIS(vecz, vecy, family = "gaussian", penalty = "lasso", tune="bic")
save(fit1, file="lassolag2.RData")

#VAR estimation
lagn = 1
# construct VAR model initial lag = 5
y1 = t(logr[(lagn+1):nrow(logr), ])
# z11 = rep(1, 53) # vector of intercept terms
z12 = t(logr[lagn:(nrow(logr)-1), ])
z1 = rbind(z12)
# vectorization
vecy = c(y1)
vecz = t(z1) %x% diag(90)  # kronecker product
fit1 = SIS(vecz, vecy, family = "gaussian", penalty = "lasso", tune="bic")
save(fit1, file="lassolag1.RData")


