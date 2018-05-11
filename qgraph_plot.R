rm(list=ls(all=T))
graphics.off()
library(qgraph)

setwd("~/Dropbox/SIS/Codes/201708")
load("energyfullconn.RData")
head(dd)
#qgraph(dd,overlay = TRUE)

GI= c(1:7)
GT= c(8:14)
HP= c(15:20)
GJ= c(21:27)
HI= c(28:34)
NE= c(35:40)
POA=c(41:47)
PDA=c(48:54)
PBA = c(55:60)
LPXBHR= c(61:84)
LPXBHxx=c(85:90)
mygroup = list(GI, GT, HP, GJ, HI, NE, POA, PDA,PBA, LPXBHR,LPXBHxx)
names(mygroup) = c("GI","GT","HP","GJ","HI","NE","POA","PDA","PBA",
                   "LPXBHR","LPXBHxx")
dd1=dd
diag(dd1)=0

#Q=qgraph(dd1, groups=mygroup, vsize=2.5, minimum = 0.003, palette = "colorblind")
#Q=qgraph(dd1, groups=mygroup, vsize=2.5, minimum = 0.003, palette = "pastel")
Q=qgraph(dd1, groups=mygroup, vsize=2.5, minimum = 0.003, palette = "pastel", 
         shape="circle",label.cex=2.5, label.scale.equal=T)

Q=qgraph(dd1, groups=mygroup, vsize=2.5, minimum = 0.003, palette = "pastel", 
         shape="diamond",label.cex=2.5, label.scale.equal=T, layout="spring")
         
#qgraph(Q, vsize=1, label.cex = 4)

qgraph(cor(dd), minimum = 0.25)
#Q=qgraph(dd1[1:20,1:20], posCol = "blue", legend=T)
#Q=qgraph(dd1[1:20,1:20], edge.color = rainbow(4))
Q <- qgraph(Q, layout = "spring")
qgraph(Q, overlay = TRUE)

Q=qgraph(dd1, groups=mygroup, vsize=2.5, minimum = 0.003, palette = "pastel", 
         shape="circle",label.cex=2.5, label.scale.equal=T, legend=T)


#cluster of two LPXB types
mlp = dd1[61:90,61:90]
mgro = list(1:24,25:30)
names(mgro)=c("LPXBHR","LPXBHxx")
Q=qgraph(mlp, groups=mgro, vsize=2.5, minimum = 0.3, palette = "pastel", 
         shape="circle",label.cex=3, label.scale.equal=T, layout="circle")

