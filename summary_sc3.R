
#lag=3
rm(list = ls(all = TRUE))
load(file = "fit2forep3.RData")
SISoutput = fit2.fore.fore
SISoutput$ix%/%(90^2)+1 # 1:15, 2:3, 3:12
par(mgp=c(2, 0.5, 0))
plot(SISoutput$fit, main="(I)SIS-SCAD, lag=3")
print(SISoutput$fit)

load(file = "cvfitscad3.RData")
plot(cvfit.scad3, ylim =c(0,1) )

#model
ix.SIS = SISoutput$ix
coef.SIS = SISoutput$coef.est
# coefficient matrix 
# For A1
SISoutput$ix[1:15]
SISoutput$ix[1:15] - (SISoutput$ix[1:15]%/%90)*90   # the row
SISoutput$ix[1:15]%/%90 + 1                    # the column

A1 = matrix(ncol=90, nrow=90, sample(0, 90*90, replace=TRUE))
A1[75, 1] = SISoutput$coef.est[2]
A1[67,60] = SISoutput$coef.est[3]
A1[68,60] = SISoutput$coef.est[4]
A1[69,60] = SISoutput$coef.est[5]
A1[71,61] = SISoutput$coef.est[6]
A1[64,63] = SISoutput$coef.est[7]
A1[68,63] = SISoutput$coef.est[8]
A1[64,65] = SISoutput$coef.est[9]
A1[68,68] = SISoutput$coef.est[10]
A1[69,68] = SISoutput$coef.est[11]
A1[74,74] = SISoutput$coef.est[12]
A1[75,74] = SISoutput$coef.est[13]
A1[76,76] = SISoutput$coef.est[14]
A1[74,84] = SISoutput$coef.est[15]
A1[67,86] = SISoutput$coef.est[16]
#A1

A2 = matrix(ncol=90, nrow=90, sample(0, 90*90, replace=TRUE))
SISoutput$ix[16:18]
(SISoutput$ix[16:18] - 8100) - ((SISoutput$ix[16:18] - 8100)%/%90)*90   # the row
(SISoutput$ix[16:18] - 8100)%/%90 + 1                                   # the column
A2[67, 5] = SISoutput$coef.est[16]
A2[67,60] = SISoutput$coef.est[17]
A2[76,67] = SISoutput$coef.est[18]

A3 = matrix(ncol=90, nrow=90, sample(0, 90*90, replace=TRUE))
SISoutput$ix[19:30]
(SISoutput$ix[19:30] - 8100*2) - ((SISoutput$ix[19:30] - 8100*2)%/%90)*90   # the row
(SISoutput$ix[19:30] - 8100*2)%/%90 + 1                                # the column
A3[68, 9] = SISoutput$coef.est[20]
A3[69, 9] = SISoutput$coef.est[21]
A3[76,11] = SISoutput$coef.est[22]
A3[76,20] = SISoutput$coef.est[23]
A3[67,33] = SISoutput$coef.est[24]
A3[74,36] = SISoutput$coef.est[25]
A3[75,36] = SISoutput$coef.est[26]
A3[64,53] = SISoutput$coef.est[27]
A3[75,53] = SISoutput$coef.est[28]
A3[76,60] = SISoutput$coef.est[29]
A3[74,61] = SISoutput$coef.est[30]
A3[75,61] = SISoutput$coef.est[31]
#A4 = matrix(ncol=90, nrow=90, sample(0, 90*90, replace=TRUE))
#A5 = matrix(ncol=90, nrow=90, sample(0, 90*90, replace=TRUE))

# compare with y1
#y1mod = cbind(A1, A2, A3, A4, A5) %*% z1
y1mod = cbind(A1, A2, A3) %*% z1

#y1mod.ts = ts(t(y1mod), frequency = 12, start = c(2010, 9)) # mts [1:45, 1:90]
#y1.ts = ts(t(y1), frequency = 12, start = c(2010, 9)) # mts [1:45, 1:90]
y1fore = y1mod[,45]
#for ( i in 1:9)
#{
#  B = (cbind(A1, A2))^(i) %*% y1mod
#  y1fore = cbind(y1fore, B[,45])
#}
#rm(B)

#or ust this
y1mod = cbind(A1, A2, A3) %*% z1
y1fore = y1mod[,45]
B1 = cbind(A1, A2, A3) %*% rbind(y1mod, z1[1:180,])
y1fore = cbind(y1fore, B1[,45]) # 12.14
B2 = cbind(A1, A2, A3) %*% rbind(B1, y1mod, z1[1:90,])
y1fore = cbind(y1fore, B2[,45]) # 1.15
B3 = cbind(A1, A2, A3) %*% rbind(B2, B1, y1mod)
y1fore = cbind(y1fore, B3[,45]) # 2.15
B4 = cbind(A1, A2, A3) %*% rbind(B3, B2, B1)
y1fore = cbind(y1fore, B4[,45]) # 3.15
B5 = cbind(A1, A2, A3) %*% rbind(B4, B3, B2)
y1fore = cbind(y1fore, B5[,45]) # 4.15
B6 = cbind(A1, A2, A3) %*% rbind(B5, B4, B3)
y1fore = cbind(y1fore, B6[,45]) # 5.15
B7 = cbind(A1, A2, A3) %*% rbind(B6, B5, B4)
y1fore = cbind(y1fore, B7[,45]) # 6.15
B8 = cbind(A1, A2, A3) %*% rbind(B7, B6, B5)
y1fore = cbind(y1fore, B8[,45]) # 7.15
B9 = cbind(A1, A2, A3) %*% rbind(B8, B7, B6)
y1fore = cbind(y1fore, B9[,45]) # 8.15

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

sc3.aic = log(abs(y1sig.avg)) + 2*30/54
sc3.hq  = log(abs(y1sig.avg)) + 2*30*log(log(54))/54
sc3.bic = log(abs(y1sig.avg)) + 2*30*log(54)/54


