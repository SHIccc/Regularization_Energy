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


##plot
library(plot3D)
#persp3D(z=logr, theta = 120)
persp3D(z=myp1,zlab="Price", ylab="Contracts", xlab="06.2000-07.2015")
z=myp1


ribbon3D (x = seq(0, 1, length.out = nrow(z)),
          y = seq(0, 1, length.out = ncol(z)), z, 
          zlab="Price", ylab="Contracts", xlab="06.2000-07.2015", theta=30)

ribbon3D (x = seq(0, 1, length.out = nrow(z)),
          y = seq(0, 1, length.out = ncol(z)), z,
          colvar = z, phi = 40, theta = 40,
          col = NULL,  NAcol = "white", breaks = NULL,
          border = NA, facets = TRUE, colkey = NULL, resfac = 1,
          image = FALSE, contour = FALSE, panel.first = NULL,
          clim = NULL, clab = NULL, bty = "b",
          lighting = FALSE, shade = NA, ltheta = -135, lphi = 0,
          space = 0.4, along = "x",
          curtain = FALSE, add = FALSE, plot = TRUE)


#hist3D (x = seq(0, 1, length.out = nrow(z)),
#        y = seq(0, 1, length.out = ncol(z)), z)

