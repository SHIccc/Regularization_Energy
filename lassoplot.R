## clear history
rm(list = ls(all = TRUE))
graphics.off()

## install and load packages
libraries = c("SIS", "glmnet")
#lapply(libraries, function(x) if (!(x %in% installed.packages())) {
#  install.packages(x)
#})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

setwd("~/")

# the coefficient matrix
load(file = "lassolag1n.RData")
plot.glmnet(fit1$fit)
plot.glmnet(fit1$fit, label = "TRUE")
plot(fit1$fit, xvar = "dev", label = TRUE)
ix.SIS = fit1$ix
coef.SIS = fit1$coef.est
load(file = "lassolag2n.RData")
plot.glmnet(fit1$fit)
plot(fit1$fit, xvar = "dev", label = TRUE)
load(file = "lassolag3n.RData")
plot.glmnet(fit1$fit)
plot(fit1$fit, xvar = "dev", label = TRUE)
load(file = "lassolag4n.RData")
plot.glmnet(fit1$fit)
plot(fit1$fit, xvar = "dev", label = TRUE)
load(file = "lassolag5n.RData")
plot.glmnet(fit1$fit)
plot(fit1$fit, xvar = "dev", label = TRUE)
load(file = "lassolag10.RData")
plot.glmnet(fit1$fit)
plot(fit1$fit, xvar = "dev", label = TRUE)