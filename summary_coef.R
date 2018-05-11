####Coefficient
## clear history
rm(list = ls(all = TRUE))
graphics.off()

## install and load packages
#libraries = c("SIS", "forecast", "glmnet", "lars", "ncvreg", "vars", "mice", "VIM")
#lapply(libraries, function(x) if (!(x %in% installed.packages())) {
#  install.packages(x)
#})
libraries = c("SIS")
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

#setwd("C:\\Users\\chenshic.hub\\Documents\\Codes")
setwd("~/Dropbox/SIS/Codes/ToRun")

##LASSO 
#lag=1
load(file = "fit1forep1.RData")
SISoutput = fit1.fore
SISoutput$ix%/%(90^2)+1
par(mgp=c(2, 0.5, 0))
plot(SISoutput$fit, main="(I)SIS-LASSO, lag=1")
#title("(I)SIS-LASSO, lag=1", line = -2)
print(SISoutput$fit)

#lag=2
rm(list = ls(all = TRUE))
load(file = "fit1forep2.RData")
SISoutput = fit1.fore
SISoutput$ix%/%(90^2)+1
par(mgp=c(2, 0.5, 0))
plot(SISoutput$fit, main="(I)SIS-LASSO, lag=2")
print(SISoutput$fit)

#lag=3
rm(list = ls(all = TRUE))
load(file = "fit1forep3.RData")
SISoutput = fit1.fore
SISoutput$ix%/%(90^2)+1
par(mgp=c(2, 0.5, 0))
plot(SISoutput$fit, main="(I)SIS-LASSO, lag=3")
print(SISoutput$fit)

#lag=5
rm(list = ls(all = TRUE))
load(file = "fit1.RData")
SISoutput = fit1
SISoutput$ix%/%(90^2)+1
par(mgp=c(2, 0.5, 0))
plot(SISoutput$fit, main="(I)SIS-LASSO, lag=5")
print(SISoutput$fit)

##SCAD 
#lag=1
rm(list = ls(all = TRUE))
load(file = "fit2forep1.RData")
SISoutput = fit2.fore.fore
SISoutput$ix%/%(90^2)+1
par(mgp=c(2, 0.5, 0))
plot(SISoutput$fit, main="(I)SIS-SCAD, lag=1")
#title("(I)SIS-LASSO, lag=1", line = -2)
print(SISoutput$fit)

#lag=2
rm(list = ls(all = TRUE))
load(file = "fit2forep2.RData")
SISoutput = fit2.fore.fore
SISoutput$ix%/%(90^2)+1
par(mgp=c(2, 0.5, 0))
plot(SISoutput$fit, main="(I)SIS-SCAD, lag=2")
print(SISoutput$fit)

#lag=3
rm(list = ls(all = TRUE))
load(file = "fit2forep3.RData")
SISoutput = fit2.fore.fore
SISoutput$ix%/%(90^2)+1
par(mgp=c(2, 0.5, 0))
plot(SISoutput$fit, main="(I)SIS-SCAD, lag=3")
print(SISoutput$fit)
