require(forecast)
require(astsa)
require(TSA)
library(dplyr)
library(readxl)
library(tseries)
library(stats)
library(readxl)
library(lmtest)
wineind <- read_excel("~/Documents/STAT 463/wineind.xls")
wine.ts <- matrix(wineind$value,nrow=176,ncol=1)
wine.ts<- as.vector((wine.ts))
wine <- ts(wine.ts, start=c(1980), end=c(1994), frequency = 12)
plot(wine) #

acf2(wine)#

m1.wine = arima(wine,order=c(0,0,0),seasonal=list(order=c(0,1,0), period=12))
res.m1 = residuals(m1.wine);  
par(mfrow=c(1,1))
plot(res.m1,xlab='Time',ylab='Residuals')

acf2(res.m1)

m2.wine = arima(wine,order=c(0,0,0),seasonal=list(order=c(1,1,1), period=12))
res.m2 = residuals(m2.wine);  
par(mfrow=c(1,1))
plot(res.m2,xlab='Time',ylab='Residuals')

acf2(res.m2)

m3.wine = arima(wine,order=c(0,0,0),seasonal=list(order=c(1,1,2), period=12))#
res.m3 = residuals(m3.wine);  #
par(mfrow=c(1,1))
plot(res.m3,xlab='Time',ylab='Residuals')#

acf2(res.m3)

m4.wine = arima(wine,order=c(0,1,0),seasonal=list(order=c(1,1,2), period=12))
res.m4 = residuals(m4.wine);  
par(mfrow=c(1,1))
plot(res.m4,xlab='Time',ylab='Residuals')

acf2(res.m4) 

adf.test(res.m4)
eacf(res.m4) #

m5.wine = arima(wine.ts,order=c(4,1,0),seasonal=list(order=c(1,1,2), period=12),method = "ML")
coeftest(m5.wine)
res.model2 = residuals(m5.wine) 
par(mfrow=c(2,1))
acf(res.model2, lag.max = 36)
pacf(res.model2, lag.max = 36)


m7.wine = arima(wine.ts,order=c(1,1,2),seasonal=list(order=c(1,1,2), period=12),method = "ML")
coeftest(m7.wine)
res.model4 = residuals(m7.wine) 
par(mfrow=c(2,1))
acf(res.model4, lag.max = 36)
pacf(res.model4, lag.max = 36)

m8.wine = arima(wine.ts,order=c(1,1,3),seasonal=list(order=c(1,1,2), period=12),method = "ML") #choose this one
coeftest(m8.wine) #
res.model5 = residuals(m8.wine) #
par(mfrow=c(2,1))
acf(res.model5, lag.max = 36)#
pacf(res.model5, lag.max = 36)#

m9.wine = arima(wine.ts,order=c(2,1,2),seasonal=list(order=c(1,1,2), period=12),method = "ML")
coeftest(m9.wine)
res.model6 = residuals(m9.wine) 
par(mfrow=c(2,1))
acf(res.model6, lag.max = 36)
pacf(res.model6, lag.max = 36)

sort.score <- function(x, score = c("bic", "aic")){
  if (score == "aic"){
    x[with(x, order(AIC)),]
  } else if (score == "bic") {
    x[with(x, order(BIC)),]
  } else {
    warning('score = "x" only accepts valid arguments ("aic","bic")')
  }
}

sc.AIC=AIC(m5.wine,m7.wine,m8.wine,m9.wine)
sc.BIC=AIC(m5.wine,m7.wine,m8.wine,m9.wine,k=log(length(wine)))
sort.score(sc.AIC, score = "aic")
sort.score(sc.BIC, score = "aic")

residual.analysis <- function(model, std = TRUE){
  library(TSA)
  library(FitAR)
  if (std == TRUE){
    res.model = rstandard(model)
  }else{
    res.model = residuals(model)
  }
  par(mfrow=c(3,2))
  plot(res.model,type='o',ylab='Standardized residuals', main="Time series plot of standardized residuals")
  abline(h=0)
  hist(res.model,main="Histogram of standardized residuals")
  qqnorm(res.model,main="QQ plot of standardized residuals")
  qqline(res.model, col = 2)
  acf(res.model,main="ACF of standardized residuals")
  print(shapiro.test(res.model))
  k=0
  LBQPlot(res.model, lag.max = length(model$residuals)-1 , StartLag = k + 1, k = 0, SquaredQ = FALSE)
}

residual.analysis(model=m8.wine)#

sarima.for(wine,200,1,1,3,1,1,2,12)#

