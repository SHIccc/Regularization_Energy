rm(list=ls(all=TRUE))
graphics.off()

## install and load packages
libraries = c("SIS", "forecast", "glmnet", "ncvreg", "vars", "mice", "VIM", "lattice")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

setwd("~/")

## read data
ele_spot = read.csv("electricity_monthly.csv", header = F, sep = ";", na.strings=c(""))
eledate = as.character(ele_spot[, 1])
st = which(eledate == "30.09.2010")
et = which(eledate == "31.07.2015")
eledata11 = ele_spot[(st:et), 2:92]

xx = t(eledata11[, -15])
xx1 = matrix(as.numeric(xx), nr = 90, nc = 59)
xx1 = t(xx1)

yy = log(xx1[2, ]) - log(xx1[1, ])
for ( i in 3: 59)
{
  xx2 = log(xx1[i, ]) - log(xx1[i-1, ])
  yy = rbind(yy, xx2)
}
rm(xx2)
yy[is.na(yy)] = log(0.01) -log(0.01)
sum(is.na(yy))
completedData = yy

str(completedData)
L = completedData

wireframe(L, drape=T, xlab=list("Type", rot=30, cex=1.2), zlim=c(-7,7),
          ylab=list("Time", cex=1.2),
          zlab=list("Price", cex=1.2),
          scales=list(arrows=FALSE, cex=0.8, 
                      y=list(at=seq(1,150,5), labels=round(seq(2010, 2015,length=18),1)))
)

dev.new()
contour(L, col=rainbow(9), xlab="Time", ylab="Type")
