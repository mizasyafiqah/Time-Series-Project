require(forecast)
require(astsa)
require(TSA)
library(dplyr)
library(readxl)
library(tseries)
Hawaii <- read_excel("~/Documents/STAT 463/CO2Hawaii.xls")

library("xts")
hawaii.ts <- matrix(Hawaii$CO2,nrow=360,ncol=1)
hawaii.ts<- as.vector((hawaii.ts))
data <- ts(hawaii.ts, start=c(1988,1), end=c(2017,12), frequency = 12)

plot(data)
acf2(data)


hawaii.m1 = arima(data,order=c(0,0,0),seasonal=list(order=c(0,2,0), period=12))
res.m1 = residuals(hawaii.m1);  
par(mfrow=c(1,1))
plot(res.m1,xlab='Time',ylab='Residuals')

acf(res.m1, lag.max = 36)
pacf(res.m1, lag.max = 36)

hawaii.m2 = arima(data,order=c(0,0,0),seasonal=list(order=c(1,2,1), period=12))
res.m2 = residuals(hawai.m2);  
par(mfrow=c(1,1))
plot(res.m1,xlab='Time',ylab='Residuals')

acf(res.m2, lag.max = 36)
pacf(res.m2, lag.max = 36)

hawai.m3 = arima(data,order=c(0,0,0),seasonal=list(order=c(1,2,2), period=12))
res.m3 = residuals(hawaii.m3);  
par(mfrow=c(1,1))
plot(res.m3,xlab='Time',ylab='Residuals')

acf2(res.m3)
pacf(res.m3, lag.max = 36)

hawaii.m4 = arima(data,order=c(0,1,0),seasonal=list(order=c(1,2,2), period=12))
res.m4 = residuals(hawaaii.m4);  
par(mfrow=c(1,1))
plot(res.m4,xlab='Time',ylab='Residuals')

acf(res.m4, lag.max = 36)
pacf(res.m4, lag.max = 36)

eacf(res.m4) #1,1 1,2 2,2

hawaii.m5 = arima(data,order=c(1,1,2),seasonal=list(order=c(1,2,2), period=12))
res.m5 = residuals(hawaii.m5);  
par(mfrow=c(1,1))
plot(res.m5,xlab='Time',ylab='Residuals')

acf(res.m5, lag.max = 36)
pacf(res.m5, lag.max = 36)

hawaii.m6 = arima(data,order=c(1,1,1),seasonal=list(order=c(1,2,2), period=12))
res.m6 = residuals(hawaii.m6);  
par(mfrow=c(1,1))
plot(res.m6,xlab='Time',ylab='Residuals')

acf(res.m6, lag.max = 36)
pacf(res.m6, lag.max = 36)

hawaii.m7 = arima(data,order=c(2,1,2),seasonal=list(order=c(1,2,2), period=12))
res.m7 = residuals(hawaii.m7);  
par(mfrow=c(1,1))
plot(res.m7,xlab='Time',ylab='Residuals')

acf(res.m7, lag.max = 36)
pacf(res.m7, lag.max = 36)

sort.score <- function(x, score = c("bic", "aic")){
  if (score == "aic"){
    x[with(x, order(AIC)),]
  } else if (score == "bic") {
    x[with(x, order(BIC)),]
  } else {
    warning('score = "x" only accepts valid arguments ("aic","bic")')
  }
}

sc.AIC=AIC(hawaii.m5,hawaii.m6,hawaii.m7)
sc.BIC=AIC(hawaii.m5,hawaii.m6,hawaii.m7,k=log(length(data)))

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

residual.analysis(model=hawaii.m6)

par(mfrow=c(1,1))
sarima.for(data,120,1,1,2,1,2,2,12)
