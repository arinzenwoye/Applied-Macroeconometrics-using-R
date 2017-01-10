library("arfima", lib.loc="~/R/win-library/3.2")
library("dynlm", lib.loc="~/R/win-library/3.2")
library("fBasics", lib.loc="~/R/win-library/3.2")
library("forecast", lib.loc="~/R/win-library/3.2")
library("fUnitRoots", lib.loc="~/R/win-library/3.2")
library("fracdiff", lib.loc="~/R/win-library/3.2")
library("mFilter", lib.loc="~/R/win-library/3.2")
library("rugarch", lib.loc="~/R/win-library/3.2")
library("quantmod", lib.loc="~/R/win-library/3.2")
library("timsac", lib.loc="~/R/win-library/3.2")
library("TSA", lib.loc="~/R/win-library/3.2")
library("vars", lib.loc="~/R/win-library/3.2")
library("foreign", lib.loc="C:/Program Files/R/R-3.2.2/library")
library(fNonlinear)
library(MSwM)
ps5_uk_gdp1 <- read.csv("C:/Users/user/Desktop/ps5_uk_gdp1.csv")
attach(ps5_uk_gdp1)

#Question 1
y=UK
n=88
dy0=100*log(y/Lag(y,k=1))
dy1=Lag(dy0,k=1)
dy2=Lag(dy0,k=2)
dy3=Lag(dy0,k=3)
dy4=Lag(dy0,k=4)
dy0=dy0[n:240]
dy1=dy1[n:240]
dy2=dy2[n:240]
dy3=dy3[n:240]
dy4=dy4[n:240]
intdate=intdate[n:240]
y1=cbind(dy0)
x1=cbind(dy1,dy2,dy3,dy4)
model=lm(y1~x1)
summary(model)
library(MSwM)
MSmodel=msmFit(model, k=2, sw=rep(TRUE, 6))
summary(MSmodel)
plotProb(MSmodel,which=1)
MSmodel@Fit@CondMean[,1]
MSmodel@Fit@CondMean[,2]
recfilt=MSmodel@Fit@smoProb[,2]
recession=recfilt>=0.50
recession=cbind(intdate,recession)
lowgrowth=dy0<=0
newrec=cbind(intdate,lowgrowth)

#Question 2
dyhat=(MSmodel@Fit@filtProb[,2]*MSmodel@Fit@CondMean[,2])+(MSmodel@Fit@filtProb[,1]*MSmodel@Fit@CondMean[,1])
res=dy0-dyhat
ts.plot(dyhat)
res=dy0-dyhat
bds.test(res,m=4)
dy1y2=dy1*dy2
dy1sq=dy1^2
dy2sq=dy2^2
inv=cbind(dy1,dy2,dy1y2,dy1sq,dy2sq)
dmv=dy0
df=data.frame(dmv,inv)
m2=nls(dmv~inv,data=df,start=c(a=0,b=0,c=0,d=0,e=0,f=0), algorithm = "plinear")
summary(m2)
res2=residuals(m2)
bds.test(res2,m=4)







