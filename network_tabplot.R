rm(list=ls(all=T))
graphics.off()
library(xtable)

setwd("~/Dropbox/SIS/Codes/201708")
load("energyfullconn.RData")
head(dd)
#qgraph(dd,overlay = TRUE)

rownames(dd)

#GI contract
GIconn = dd[1:7,1:7]
GIconn

#GT contract
GTconn = dd[8:14,8:14]

#HP contract
HPconn = dd[15:20,15:20]

#GJ contract
GJconn = dd[21:27,21:27]

#HI
HIconn = dd[28:34,28:34]

#NE
NEconn = dd[35:40, 35:40]

#POA
POAconn =dd[41:47,41:47]

#PDA
PDAconn = dd[48:54,48:54]

#PBA
PBAconn = dd[55:60,55:60]

#LPXBHR
LPXBHRconn = dd[61:84,61:84]
#xtable(LPXBHRconn[,1:8])
#xtable(LPXBHRconn[,9:16])
xtable(LPXBHRconn[,17:24])

#LPXBHxx
LPXBHxxconn = dd[85:90, 85:90]

#Table 4.1
##For 11 different types, in total 90 contracts
#7,7,6,7,7,6,7,7,6,24,6
sectorname = c("GI","GT","HP","GJ","HI","NE","POA","PDA","PBA","LPXBHR","LPXBHxx")
#Table 4..
secconn = matrix(0, length(sectorname), length(sectorname))
mr= c(7,14,20,27,34,40,47,54,60,84,90)
mc= c(0,7,14,20,27,34,40,47,54,60,84)
for(i in 1:11){
  for(j in 1:11){
    secconn[i,j] = sum(dd[(mc[i]+1):mr[i],(mc[j]+1):mr[j] ])
  }
}
rownames(secconn)=colnames(secconn)=sectorname
secconn1=secconn%*%solve(diag(c(7,7,6,7,7,6,7,7,6,24,6)))
colnames(secconn1)=sectorname
from = rowSums(secconn1)
to = colSums(secconn1)
secconn2 = rbind(secconn1, to)
secconn2 = cbind(secconn2, c(from, sum(to)))
xtable(secconn2)
#table dataset
tab41 = matrix(0,13,9)
tab41[,1]= c("GI","GT","HP","GJ","HI","NE","POA","PDA","PBA","LPXBHR","LPXBHR","LPXBHR","LPXBHxx")
tab41[1,2:8]=rownames(GIconn)
tab41[2,2:8]=rownames(GTconn)
tab41[3,2:7]=rownames(HPconn)
tab41[4,2:8]=rownames(GJconn)
tab41[5,2:8]=rownames(HIconn)
tab41[6,2:7]=rownames(NEconn)
tab41[7,2:8]=rownames(POAconn)
tab41[8,2:8]=rownames(PDAconn)
tab41[9,2:7]=rownames(PBAconn)
tab41[10,2:9]=rownames(LPXBHRconn)[1:8]
tab41[11,2:9]=rownames(LPXBHRconn)[9:16]
tab41[12,2:9]=rownames(LPXBHRconn)[17:24]
tab41[13,2:7]=rownames(LPXBHxxconn)

#tab41 = rbind(tab41[13,],tab41[1:12,])

xtable(tab41)

#network plot
myd=secconn1
diag(myd)=0
qgraph(myd, palette="pastel", layout="circle", groups=sectorname, legend =F)
mylp = LPXBHRconn
diag(mylp)=0
qgraph(mylp, palette="gray", layout="spring", legend =F, minimum=0.1)
qgraph(mylp, palette="gray", layout="spring", legend =F, minimum=0.1)

par(mfrow = c(1,2))
myhp=HPconn
diag(myhp)=0
qgraph(myhp, palette="gray", layout="spring", legend =F, minimum=0.78, arrow =T)

myne = NEconn
diag(myne)=0
qgraph(myne, palette="gray", layout="spring", legend =F, minimum=0.78, arrow =T)


#joint plot of HP,NE
par(mfrow=c(1,1))
myhn = dd[c(15:20,35:40),c(15:20,35:40)]
diag(myhn)=0
HP=c(1:6)
NE=c(7:12)
hng=list(HP, NE)
names(hng)=c("HP-type","NE-type")
qgraph(myhn, palette="colorblind", layout="circle",legend =T, minimum=0.8, arrow =T, groups = hng, line=2.5)
#table
LPXBHRconn
#colSums(LPXBHRconn)[order(colSums(LPXBHRconn), decreasing = T)]
#rowSums(LPXBHRconn)[order(rowSums(LPXBHRconn), decreasing = T)]

lptab = cbind(rowSums(LPXBHRconn)[order(rowSums(LPXBHRconn), decreasing = T)],
              colSums(LPXBHRconn)[order(colSums(LPXBHRconn), decreasing = T)],
              (colSums(LPXBHRconn)[order(colSums(LPXBHRconn), decreasing = T)]-
                 rowSums(LPXBHRconn)[order(rowSums(LPXBHRconn), decreasing = T)] ))
xtable(lptab)

