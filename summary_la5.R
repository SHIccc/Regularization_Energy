## clear history
rm(list = ls(all = TRUE))
graphics.off()

## install and load packages
#libraries = c("SIS", "forecast", "glmnet", "lars", "ncvreg", "vars", "mice", "VIM")
#lapply(libraries, function(x) if (!(x %in% installed.packages())) {
#  install.packages(x)
#})
libraries = c("SIS", "vars")
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

rm(list = ls(all = TRUE))
#setwd("C:\\Users\\chenshic.hub\\Documents\\Codes")
setwd("~/Dropbox/SIS/Codes/ToRun")

# the coefficient matrix
load(file = "fit1.RData") # the output of fit1

plot.glmnet(fit1$fit)
plot.glmnet(fit1$fit, label = "TRUE")
load(file = "cvfit5.RData")
plot(cvfit5, ylim=c(0,1))

ix.SIS = fit1$ix
coef.SIS = fit1$coef.est

# check the coefficient matrix
print(fit1)
fit1$ix%/%(90^2)+1 # 1st-30, 2nd-12, 3rd-13, 4th-15, 5th-3

# coefficient matrix 
# For A1
fit1$ix[1:30]
# fit1$ix[1:30]/90
fit1$ix[1:30]%/%90 + 1                    # the column
fit1$ix[1:30] - (fit1$ix[1:30]%/%90)*90   # the row

A1 = matrix(ncol=90, nrow=90, sample(0, 90*90, replace=TRUE))
A1[64, 2] = fit1$coef.est[2]
A1[67,22] = fit1$coef.est[3]
A1[68,22] = fit1$coef.est[4]
A1[84,32] = fit1$coef.est[5]
A1[75,52] = fit1$coef.est[6]
A1[67,60] = fit1$coef.est[7]
A1[68,60] = fit1$coef.est[8]
A1[69,60] = fit1$coef.est[9]
A1[76,60] = fit1$coef.est[10]
A1[65,61] = fit1$coef.est[11]
A1[67,61] = fit1$coef.est[12]
A1[68,61] = fit1$coef.est[13]
A1[74,61] = fit1$coef.est[14]
A1[75,61] = fit1$coef.est[15]
A1[64,62] = fit1$coef.est[16]
A1[67,67] = fit1$coef.est[17]
A1[68,68] = fit1$coef.est[18]
A1[69,68] = fit1$coef.est[19]
A1[68,69] = fit1$coef.est[20]
A1[69,69] = fit1$coef.est[21]
A1[70,70] = fit1$coef.est[22]
A1[71,70] = fit1$coef.est[23]
A1[74,74] = fit1$coef.est[24]
A1[75,74] = fit1$coef.est[25]
A1[76,76] = fit1$coef.est[26]
A1[84,81] = fit1$coef.est[27]
A1[63,84] = fit1$coef.est[28]
A1[64,84] = fit1$coef.est[29]
A1[84,84] = fit1$coef.est[30]
A1[64,88] = fit1$coef.est[31]

A1
# For A2
fit1$ix[31:42]
(fit1$ix[31:42] - 8100)%/%90 + 1                              # the column
(fit1$ix[31:42] - 8100) - ((fit1$ix[31:42] - 8100)%/%90)*90   # the row

A2 = matrix(ncol=90, nrow=90, sample(0, 90*90, replace=TRUE))
A2[76,27] = fit1$coef.est[32]
A2[64,46] = fit1$coef.est[33]
A2[63,65] = fit1$coef.est[34]
A2[64,65] = fit1$coef.est[35]
A2[76,79] = fit1$coef.est[36]
A2[63,84] = fit1$coef.est[37]
A2[64,84] = fit1$coef.est[38]
A2[65,84] = fit1$coef.est[39]
A2[66,84] = fit1$coef.est[40]
A2[67,84] = fit1$coef.est[41]
A2[68,84] = fit1$coef.est[42]
A2[86,84] = fit1$coef.est[43]

A2

# For A3
fit1$ix[43:55]
(fit1$ix[43:55] - 8100*2)%/%90 + 1                                # the column
(fit1$ix[43:55] - 8100*2) - ((fit1$ix[43:55] - 8100*2)%/%90)*90   # the row

A3 = matrix(ncol=90, nrow=90, sample(0, 90*90, replace=TRUE))
A3[75, 7] = fit1$coef.est[44]
A3[68, 9] = fit1$coef.est[45]
A3[69, 9] = fit1$coef.est[46]
A3[67,11] = fit1$coef.est[47]
A3[84,13] = fit1$coef.est[48]
A3[67,49] = fit1$coef.est[49]
A3[76,60] = fit1$coef.est[50]
A3[75,61] = fit1$coef.est[51]
A3[76,69] = fit1$coef.est[52]
A3[84,80] = fit1$coef.est[53]
A3[62,84] = fit1$coef.est[54]
A3[63,84] = fit1$coef.est[55]
A3[64,84] = fit1$coef.est[56]
A3

# For A4
fit1$ix[56:70]
(fit1$ix[56:70] - 8100*3)%/%90 + 1                                # the column
(fit1$ix[56:70] - 8100*3) - ((fit1$ix[56:70] - 8100*3)%/%90)*90   # the row

A4 = matrix(ncol=90, nrow=90, sample(0, 90*90, replace=TRUE))
A4[67, 2] = fit1$coef.est[57]
A4[68, 2] = fit1$coef.est[58]
A4[69, 2] = fit1$coef.est[59]
A4[75, 2] = fit1$coef.est[60]
A4[67, 3] = fit1$coef.est[61]
A4[68, 3] = fit1$coef.est[62]
A4[69, 3] = fit1$coef.est[63]
A4[68, 9] = fit1$coef.est[64]
A4[69, 9] = fit1$coef.est[65]
A4[74,22] = fit1$coef.est[66]
A4[75,22] = fit1$coef.est[67]
A4[69,42] = fit1$coef.est[68]
A4[67,49] = fit1$coef.est[69]
A4[74,64] = fit1$coef.est[70]
A4[75,64] = fit1$coef.est[71]

A4

# For A5 
fit1$ix[71:73]
(fit1$ix[71:73] - 8100*4)%/%90 + 1                                # the column
(fit1$ix[71:73] - 8100*4) - ((fit1$ix[71:73] - 8100*4)%/%90)*90   # the row

A5 = matrix(ncol=90, nrow=90, sample(0, 90*90, replace=TRUE))
A5[68, 3] = fit1$coef.est[72]
A5[69, 3] = fit1$coef.est[73]
A5[67,48] = fit1$coef.est[74]

A5

## estimated model derived from SIS-SCAD-VAR model
# compare with y1
y1mod = cbind(A1, A2, A3, A4, A5) %*% z1

y1mod.ts = ts(t(y1mod), frequency = 12, start = c(2010, 9)) # mts [1:53, 1:90]
y1.ts = ts(t(y1), frequency = 12, start = c(2010, 9)) # mts [1:53, 1:90]

# the corresponding coefficient for different futures
fit1$ix[1:30] - (fit1$ix[1:30]%/%90)*90
fit1$coef.est[1:30]
(fit1$ix[31:42] - 8100) - ((fit1$ix[31:42] - 8100)%/%90)*90
fit1$coef.est[31:42]
(fit1$ix[43:55] - 8100*2) - ((fit1$ix[43:55] - 8100*2)%/%90)*90
fit1$coef.est[43:55]
(fit1$ix[56:70] - 8100*3) - ((fit1$ix[56:70] - 8100*3)%/%90)*90
fit1$coef.est[56:70]
(fit1$ix[71:73] - 8100*4) - ((fit1$ix[71:73] - 8100*4)%/%90)*90  
fit1$coef.est[71:73]

# the variable selected
v62 = abs(fit1$coef.est[54])

v63 = abs(fit1$coef.est[28]) + abs(fit1$coef.est[34]) + abs(fit1$coef.est[37]) + abs(fit1$coef.est[55])

v64 = abs(fit1$coef.est[2])  + abs(fit1$coef.est[16]) + abs(fit1$coef.est[29]) + abs(fit1$coef.est[31]) + 
  abs(fit1$coef.est[33]) + abs(fit1$coef.est[36]) + abs(fit1$coef.est[38]) + abs(fit1$coef.est[56])

v65 = abs(fit1$coef.est[11]) + abs(fit1$coef.est[39])

v66 = abs(fit1$coef.est[40])

v67 = abs(fit1$coef.est[3])  + abs(fit1$coef.est[7])  + abs(fit1$coef.est[12]) + abs(fit1$coef.est[17]) +
  abs(fit1$coef.est[41]) + abs(fit1$coef.est[47]) + abs(fit1$coef.est[49]) + abs(fit1$coef.est[57]) +
  abs(fit1$coef.est[61]) + abs(fit1$coef.est[69]) + abs(fit1$coef.est[74])

v68 = abs(fit1$coef.est[4])  + abs(fit1$coef.est[8])  + abs(fit1$coef.est[13]) + abs(fit1$coef.est[18]) +
  abs(fit1$coef.est[20]) + abs(fit1$coef.est[42]) + abs(fit1$coef.est[45]) + abs(fit1$coef.est[58]) +
  abs(fit1$coef.est[62]) + abs(fit1$coef.est[64]) + abs(fit1$coef.est[72])

v69 = abs(fit1$coef.est[9])  + abs(fit1$coef.est[19]) + abs(fit1$coef.est[21]) + abs(fit1$coef.est[46]) +
  abs(fit1$coef.est[59]) + abs(fit1$coef.est[63]) + abs(fit1$coef.est[65]) + abs(fit1$coef.est[68]) +
  abs(fit1$coef.est[73])

v70 = abs(fit1$coef.est[22])

v71 = abs(fit1$coef.est[23])

v74 = abs(fit1$coef.est[14]) + abs(fit1$coef.est[24]) + abs(fit1$coef.est[66]) + abs(fit1$coef.est[70])

v75 = abs(fit1$coef.est[6])  + abs(fit1$coef.est[15]) + abs(fit1$coef.est[25]) + abs(fit1$coef.est[44]) +
  abs(fit1$coef.est[51]) + abs(fit1$coef.est[60]) + abs(fit1$coef.est[67]) + abs(fit1$coef.est[71])

v76 = abs(fit1$coef.est[10]) + abs(fit1$coef.est[26]) + abs(fit1$coef.est[32]) + abs(fit1$coef.est[37]) + 
  abs(fit1$coef.est[50]) + abs(fit1$coef.est[52])

v84 = abs(fit1$coef.est[5])  + abs(fit1$coef.est[27]) + abs(fit1$coef.est[30]) + abs(fit1$coef.est[48]) +
  abs(fit1$coef.est[53])

v86 = abs(fit1$coef.est[43])

## output result
ranking1 = data.frame(v67, v68, v69, v76, v75, v64, v84, v74, v63, v65, v66, v62, v71, v70, v86, row.names = NULL)
ranking1/sum(ranking1)
# save.image(file = "ranking1.RData")
load(file = "ranking1.RData")

###mse
#or ust this
y1mod = cbind(A1, A2, A3, A4, A5) %*% z1
y1fore = y1mod[,45]
B1 = cbind(A1, A2, A3, A4, A5) %*% rbind(y1mod, z1[1:360,])
y1fore = cbind(y1fore, B1[,45]) # 12.14
B2 = cbind(A1, A2, A3, A4, A5) %*% rbind(B1, y1mod, z1[1:270,])
y1fore = cbind(y1fore, B2[,45]) # 1.15
B3 = cbind(A1, A2, A3, A4, A5) %*% rbind(B2, B1, y1mod, z1[1:180,])
y1fore = cbind(y1fore, B3[,45]) # 2.15
B4 = cbind(A1, A2, A3, A4, A5) %*% rbind(B3, B2, B1, y1mod, z1[1:90,])
y1fore = cbind(y1fore, B4[,45]) # 3.15
B5 = cbind(A1, A2, A3, A4, A5) %*% rbind(B4, B3, B2, B1, y1mod)
y1fore = cbind(y1fore, B5[,45]) # 4.15
B6 = cbind(A1, A2, A3, A4, A5) %*% rbind(B5, B4, B3, B2, B1)
y1fore = cbind(y1fore, B6[,45]) # 5.15
B7 = cbind(A1, A2, A3, A4, A5) %*% rbind(B6, B5, B4, B3, B2)
y1fore = cbind(y1fore, B7[,45]) # 6.15
B8 = cbind(A1, A2, A3, A4, A5) %*% rbind(B7, B6, B5, B4, B3)
y1fore = cbind(y1fore, B8[,45]) # 7.15
B9 = cbind(A1, A2, A3, A4, A5) %*% rbind(B8, B7, B6, B5, B4)
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
y1est = cbind(y1mod[,1:44], y1fore)
setwd("~/Dropbox/SIS/Codes/ToRun")
load(file = "y1inf.RData")

y1res = y1est - t(y1inf[5:58,])
y1sig=0
for (i in 1:54){
  y1sig = y1sig + t(y1res[,i]) %*% y1res[,i]
}
y1sig.avg = y1sig/54

la5.aic = log(abs(y1sig.avg)) + 2*73/54
la5.hq  = log(abs(y1sig.avg)) + 2*73*log(log(54))/54
la5.bic = log(abs(y1sig.avg)) + 2*73*log(54)/54
