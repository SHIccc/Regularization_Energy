##SCAD 
#lag=1
rm(list = ls(all = TRUE))
setwd("~/Dropbox/SIS/Codes/ToRun")

load(file = "fit2forep1.RData")
SISoutput = fit2.fore.fore
SISoutput$ix%/%(90^2)+1
par(mgp=c(2, 0.5, 0))
plot(SISoutput$fit, main="(I)SIS-SCAD, lag=1")

load(file = "cvfitscad1.RData")
plot(cvfit.scad1, ylim=c(0,1))

#title("(I)SIS-LASSO, lag=1", line = -2)
print(SISoutput$fit)
#model
ix.SIS = SISoutput$ix
coef.SIS = SISoutput$coef.est
# coefficient matrix 
# For A1
SISoutput$ix[1:11]
SISoutput$ix[1:11] - (SISoutput$ix[1:11]%/%90)*90   # the row
SISoutput$ix[1:11]%/%90 + 1                    # the column

A1 = matrix(ncol=90, nrow=90, sample(0, 90*90, replace=TRUE))
A1[67,32] = SISoutput$coef.est[2]
A1[67,60] = SISoutput$coef.est[3]
A1[68,60] = SISoutput$coef.est[4]
A1[69,60] = SISoutput$coef.est[5]
A1[76,60] = SISoutput$coef.est[6]
A1[67,66] = SISoutput$coef.est[7]
A1[67,67] = SISoutput$coef.est[8]
A1[68,68] = SISoutput$coef.est[9]
A1[69,69] = SISoutput$coef.est[10]
A1[75,74] = SISoutput$coef.est[11]
A1[76,76] = SISoutput$coef.est[12]
A1
#A2 = matrix(ncol=90, nrow=90, sample(0, 90*90, replace=TRUE))
# compare with y1
#y1mod = cbind(A1, A2, A3, A4, A5) %*% z1
y1mod = A1 %*% z1

#y1mod.ts = ts(t(y1mod), frequency = 12, start = c(2010, 9)) # mts [1:45, 1:90]
#y1.ts = ts(t(y1), frequency = 12, start = c(2010, 9)) # mts [1:45, 1:90]
y1fore = y1mod[,45]
for ( i in 1:9)
{
  B = (A1^(i)) %*% y1mod
  y1fore = cbind(y1fore, B[,45])
}
rm(B)
#calculate and plot mse
y1org = y1[, 36:45]
y1fore.ts = ts(t(y1fore), start = c(2014,11), frequency = 12)
y1org.ts  = ts(t(y1org),  start = c(2014,11), frequency = 12)

#plot(y1org.ts[70:77], lwd = 2, col = "red ", ylim =c(-2,2))
#lines(y1fore.ts, lwd = 2, col = "blue")

plot(y1org.ts[,1], lwd = 1.2, col = "grey", ylim = c(-2,2))
lines(y1fore.ts[,1], lwd = 2, col = "blue")
for (i in 2:90)
{
  lines(y1org.ts[,i], lwd = 1.2, col = "grey")
  lines(y1fore.ts[,i], lwd = 2, col = "blue")
}

plot(y1org[,1], col="grey", lwd =1.2, type="l", ylim = c(-2, 2))
lines(y1fore[,1], col = "blue", lwd = 2)
for (i in 2:10)
{
  lines(y1org[,i], col = "grey", lwd = 2)
  lines(y1fore[,i], col = "blue", lwd = 2)
}

#y1mse = 0
#y1diff = 0
#for (i in 1:90)
#{
#  for (j in 1:10)
#  {
#    y1diff[i*j]= y1org[i,j] - y1fore[i,j]
#    y1mse = y1mse + (y1diff[i*j])^2
#  }
#}
#y1mse = y1mse/900

y1diff = y1org - y1fore
y1sum=0
for (i in 1:90)
{
  for (j in 1:10)
  {
    y1sum= y1sum+y1diff[i,j]^2
  }
}
y1mse = y1sum/900

y1diff.ts = ts(y1diff, start = c(2014,11), frequency = 12)
plot(y1diff[,1], type = "l", col="black", ylim =c(-2,2))
for (i in 2:10){
  lines(y1diff[,i], col=rainbow(11)[i])
}

#information criteria
y1est = cbind(y1mod, y1fore[,-1])
load(file = "y1inf.RData")

y1res = y1est - t(y1inf[5:58,])
y1sig=0
for (i in 1:54){
  y1sig = y1sig + t(y1res[,i]) %*% y1res[,i]
}
y1sig.avg = y1sig/54

sc1.aic = log(abs(y1sig.avg)) + 2*11/54
sc1.hq  = log(abs(y1sig.avg)) + 2*11*log(log(54))/54
sc1.bic = log(abs(y1sig.avg)) + 2*11*log(54)/54

