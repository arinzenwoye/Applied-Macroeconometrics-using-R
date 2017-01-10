library(fBasics)
library(dynlm)
library(rugarch)
library(quantmod)
library(forecast)
library(mFilter)
library(vars)
library(timsac)
library(TSSS)
library(arfima)
# using the arfima function in forecast
library(tseries)
library(fUnitRoots)


# Path for Germany data file
datapath = "f:/rutgers/econ510/problem/ps4_germany_gdp.csv"

# Load Data
data=read.csv(datapath,header=T)
attach(data)
head(data)
tail(data)
y1=log(Germany)

# Dickey Fuller test with no lags
g_adf<-ur.df(y1, type="none",lags=0)
summary(g_adf)

# Dickey Fuller test with AIC for lags
g_adf<-ur.df(y1, type="drift",selectlags = "AIC")
summary(g_adf)
##############################################################

# Path for UK data file
datapath = "f:/rutgers/econ510/problem/ps4_uk_gdp.csv"

# Load Data
data=read.csv(datapath,header=T)
attach(data)
head(data)
tail(data)
y2=log(UK)

# Dickey Fuller test with no lags
uk_adf<-ur.df(y2, type="drift",lags=0)
summary(uk_adf)

# Dickey Fuller test with AIC for lags
uk_adf<-ur.df(y2, type="drift",selectlags = "AIC")
summary(uk_adf)

##############################################################
# Path for Japan data file
datapath = "f:/rutgers/econ510/problem/ps4_japan_gdp.csv"

# Load Data
data=read.csv(datapath,header=T)
attach(data)
head(data)
tail(data)
y3=log(Japan)

# Dickey Fuller test with no lags
japan_adf<-ur.df(y3, type="drift",lags=0)
summary(japan_adf)

# Dickey Fuller test with AIC for lags
japan_adf<-ur.df(y3, type="drift",selectlags = "AIC")
summary(japan_adf)


# Phillips Perron test on all three GDP series
urppTest(y1,type="Z-alpha",model="constant")
urppTest(y2,type="Z-alpha",model="constant")
urppTest(y3,type="Z-alpha",model="constant")



# HP filter the GDP series and do the ADF test
hpy1<-hpfilter(y1,type="lambda",freq=1600)
g_adf<-ur.df(hpy1$cycle, type="drift",selectlags = "AIC")
summary(g_adf)

hpy2<-hpfilter(y2,type="lambda",freq=1600)
uk_adf<-ur.df(hpy2$cycle, type="drift",selectlags = "AIC")
summary(uk_adf)

hpy3<-hpfilter(y3,type="lambda",freq=1600)
japan_adf<-ur.df(hpy3$cycle, type="drift",selectlags = "AIC")
summary(japan_adf)

###########################################
# Estimate fractional difference models
###############Germany
# Growth rates
dy1=100*log(y1/Lag(y1,k=1))
dy1=dy1[2:length(y1)]

# arfima function in forecast gives me different results
# frac_y1<-arfima(dy1,ic="aic")

# This is the call to the function in the arfima library
frac_y1<-arfima(dy1,order=c(2,0,1),autoweed=TRUE,quiet=TRUE)
frac_y1<-bestModes(frac_y1,1)
print(frac_y1)
AIC(frac_y1,k=2)[1]

###############UK
# Growth rates
dy2=100*log(y2/Lag(y2,k=1))
dy2=dy2[2:length(y2)]

frac_y2<-arfima(dy2,order=c(1,0,1),autoweed=TRUE,quiet=TRUE)
frac_y2<-bestModes(frac_y2,1)
print(frac_y2)
AIC(frac_y2,k=2)[1]


###############Japan
# Growth rates
dy3=100*log(y3/Lag(y3,k=1))
dy3=dy3[2:length(y3)]

#  This one appears to minimize the AIC for Japan
frac_y3<-arfima(dy3,order=c(1,0,1),autoweed=TRUE,quiet=TRUE)
frac_y3<-bestModes(frac_y3,1)
print(frac_y3)
AIC(frac_y3,k=2)[1]



#####Cointegration

# Align the data starting at 199401-201412
z1=y1[13:length(y1)]
z2=y2[156:239]
z3=y3[1:length(y3)]
z=cbind(z1,z2,z3)

coint.GDP<-ca.jo(z,type="trace",ecdet="const",K=2,spec="transitory")
summary(coint.GDP)

# Drop Japan
z=cbind(z1,z2)
coint.GDP<-ca.jo(z,type="trace",ecdet="const",K=2,spec="transitory")
summary(coint.GDP)
VECM=cajorls(coint.GDP)
summary(VECM$rlm)













