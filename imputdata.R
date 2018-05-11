rm(list=ls(all=TRUE))
graphics.off()

## install and load packages
libraries = c("ncvreg", "mice", "VIM", "lattice")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

#setwd("~/Dropbox/SIS/Codes")

## read data
ele_spot = read.csv("electricity_monthly.csv", header = F, sep = ";", na.strings=c(""))
eledate = as.character(ele_spot[, 1])
st = which(eledate == "30.09.2010")
et = which(eledate == "31.07.2015")
eledata11 = ele_spot[(st:et), 2:92]

## transform the nonstationary data to stationary
xx = t(eledata11[, -15])

## eliminate missing data before passing X and y to ncvreg
# The pattern of missing data numerically and grahpically
md.pattern(xx)
aggr_plot = aggr(xx, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), 
                 cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

xx1 = matrix(as.numeric(xx), nr = 90, nc = 59)
xx1 = t(xx1)

yy = log(xx1[2, ]) - log(xx1[1, ])
for ( i in 3: 59)
{
  xx2 = log(xx1[i, ]) - log(xx1[i-1, ])
  yy = rbind(yy, xx2)
}
rm(xx2)

## missing data
# The pattern of missing data numerically and grahpically
sum(is.na(yy))
md.pattern(yy)
aggr_plot = aggr(yy, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), 
                 cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

## 1st method: impute the data by simulated distribution
# impute the missing data
row.names(yy) # yy:58*90
row.names(yy) = NULL

# imputation method
yyfull = mice(yy, m=5, maxit=50, meth="sample", seed = 500) # sampling
yyfull = mice(yy, m=5, maxit=50, meth="pmm", seed = 500) # predictive mean matching
yyfull$meth
yyfull$imp$V63 #check inputed data for column 4
completedData = complete(yyfull, 1)

# imputed data
completedData = complete(yyfull, 1)
densityplot(yyfull)
stripplot(yyfull, pch = 20, cex =1.2)

## 2nd method: replace the negative valus in xx by 0.01
yy[is.na(yy)] = log(0.01) -log(0.01)
sum(is.na(yy))
completedData = yy