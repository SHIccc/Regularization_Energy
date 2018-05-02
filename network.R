rm(list=ls(all=T))
graphics.off()

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
#load dataset
lagn = 10
lr1 = lg1(logr, lagn)
n1=ncol(lr1)
d1=nrow(logr)
y1=logr[,(ncol(logr)-n1+1):ncol(logr)]
cy1=c(y1)##y in lasso
kx1=t(lr1)%x%diag(d1)
#load fit1
#load("mylassolag2.RData")
load("lassolag10.RData")
str(fit1)

capA = rep(0, 90*90*10)
capA[fit1$ix] = fit1$coef.est[-1]
hatu = matrix(cy1 - kx1%*%capA, nr=90)

myA = matrix(capA, nr=90)

A1 = myA[1:90,1:90]
A2 = myA[1:90,91:180]
A3 = myA[1:90,181:270]
A4 = myA[1:90,271:360]
A5 = myA[1:90,361:450]
A6 = myA[1:90,451:540]
A7 = myA[1:90,541:630]
A8 = myA[1:90,631:720]
A9 = myA[1:90,721:810]
A10 = myA[1:90,811:900]

B1 = A1
B2 = A1%*%B1 + A2
B3 = A1%*%B2 + A2%*%B1 + A3
B4 = A1%*%B3 + A2%*%B2 + A3%*%B1 + A4
B5 = A1%*%B4 + A2%*%B3 + A3%*%B2 + A4%*%B1 + A5 
B6 = A1%*%B5 + A2%*%B4 + A3%*%B3 + A4%*%B2 + A5%*%B1 + A6
B7 = A1%*%B6 + A2%*%B5 + A3%*%B4 + A4%*%B3 + A5%*%B2 + A6%*%B1 + A7
B8 = A1%*%B7 + A2%*%B6 + A3%*%B5 + A4%*%B4 + A5%*%B3 + A6%*%B2 + A7%*%B1 + A8
B9 = A1%*%B8 + A2%*%B7 + A3%*%B6 + A4%*%B5 + A5%*%B4 + A6%*%B3 + A7%*%B2 + A8%*%B1 + A9
B10 = A1%*%B9 + A2%*%B8 + A3%*%B7 + A4%*%B6 + A5%*%B5 + A6%*%B4 + A7%*%B3 + A8%*%B2+ A9%*%B1 +A10
capB=cbind(B1,B2,B3,B4,B5,B6,B7,B8,B9,B10)


#GFEV calculation

dd=matrix(0,nrow(y1),nrow(y1))
Sigma = hatu %*% t(hatu)/ncol(hatu)

#ss1=sqrt(diag(Sigma))
#s01=diag(1/ss1)%*%Sigma%*%diag(1/ss1)
#th1=sqrt(log(nrow(hatu))/ncol(hatu))
#cs1=c(s01)
#is1=which(abs(cs1)<cs1)
#cs1[is1]=0
#s01=matrix(cs1,nr=nrow(hatu),byrow=F)
#Sigma=diag(ss1)%*%s01%*%diag(ss1)
sigma = 1/sqrt(diag(Sigma))

dd=matrix(0,90,90)
for(l in 1:nrow(y1)){
  
  el = numeric(nrow(y1))
  el[l] = 1
  
  for(j in 1:nrow(y1)){
    #j=2
    eh = numeric(nrow(y1))
    eh[j] = 1
    d1 = d2=mB=NULL
    mB[[1]] = diag(nrow(y1))
    for (k in 1:lagn){
      mB[[k+1]] = matrix(capB[,(nrow(y1)*(k-1)+1):((nrow(y1))*k)], nr=nrow(y1))
      d1[k] = (el) %*% mB[[k]] %*% Sigma %*% matrix(eh) * sigma[j]
      d2[k] = (el) %*% mB[[k]] %*% Sigma %*% t(mB[[k]]) %*% matrix(el) 
      num = sum(d1^2)
      deno = sum(d2)
      dd[l,j]= num/deno
    }
    }}

rowSums(dd)
colSums(dd)
sum(rowSums(dd))
myn = read.csv("electricity_monthly.csv", header = T, sep = ";")
myn = names(myn)[2:92]
rownames(dd)=colnames(dd)=myn[-15]

save(dd, file = "energyfullconn.RData")
