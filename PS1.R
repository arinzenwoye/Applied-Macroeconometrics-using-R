#Question 1

real_gdp <- read.csv("real_gdp.csv")
realgdp=matrix(real_gdp[,2])
plot(real_gdp)
log_realgdp <- log(realgdp)
ts.plot(log_realgdp)
difflrealgdp=diff(log_realgdp,lag=1,differences=1)
ts.plot(difflrealgdp)
ac<-acf(difflrealgdp, type=c("correlation"),main="ACF of difflrealgdp")
plot(ac,ci=.95, ylim=c(-1,1))#acf of log differenced realgdp
pac<-pacf(difflrealgdp, main="PACF of diffrealgdp")
Arima(difflrealgdp, order=c(3,0,0))
#loop below provides the model with minimum aic 
final.aic <- Inf
final.order<-c(0,0,0)
for(i in 0:4) for (j in 0:4){
current.aic<-AIC(Arima(difflrealgdp,order=c(i,0,j)))
if(current.aic<final.aic){
final.aic<-current.aic
final.order <-c(i,0,j)
final.arma<-Arima(difflrealgdp,order=final.order)
}
}
#loop provides model with minimum BIC
for(p in 0:4){
for(q in 0:4){
print((Arima(difflrealgdp , order = c(p,0,q)))$aic)
p = p+1
q = q+1
}
}
Arima(difflrealgdp, order=c(1,0,0))


#Question 2

retail_sales <- read.csv("retail_sales.csv")
retailsales=matrix(retail_sales[,2])
log_retailsales <- log(retailsales)
ts.plot(log_retailsales)
d1=diff(log_retailsales,lag=4)
difflretailsales=diff(d1,differences=1)
ts.plot(difflretailsales,main="log differenced seasonal adjusted Retail Sales")
acf(difflretailsales, type=c("correlation"),main="ACF Function")
pacf(difflretailsales)
fit<-Arima(difflretailsales, order=c(0,1,4),seasonal=c(0,1,1))
final.aic <- Inf
final.order<-c(0,0,0)
for(i in 0:4) for (j in 0:4){
current.aic<-AIC(arima(difflretailsales,order=c(i,1,j),seasonal=c(0,1,1),optim.control = list(maxit = 1000)))
if(current.aic<final.aic){
final.aic<-current.aic
final.order <-c(i,1,j)
final.arma<-arima(difflretailsales,order=final.order)
}
}