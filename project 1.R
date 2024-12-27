require(forecast)
require(astsa)
require(TSA)
library(dplyr)
library(readxl)
library(tseries)
passenger <- read_excel("~/Documents/STAT 463/US monthly air passengers.xls")
passenger <- passenger %>% group_by(MONTH,YEAR) %>% summarise(total = sum(Sum_PASSENGERS))

                              
library("xts")
pass.ts <- matrix(passenger$total,nrow=126,ncol=1)
pass.ts<- as.vector((pass.ts))
data <- ts(pass.ts, start=c(1970), end=c(2016), frequency = 12)


# Plot the data
plot(data, ylab= 'number of passenger', xlab = 'year')
plot(diff(data))
plot(diff(log(data)))

#Plot the acf
acf2(data)
acf(data)
acf2(diff(data))
acf(log(data))
acf2(diff(log(data)))

#fit seasonal arima model
data<- diff(log(data))
pass.m1 = arima(data,order=c(0,0,0),seasonal=list(order=c(0,0,0), period=12))
res.m1 = residuals(pass.m1);  
par(mfrow=c(1,1))
plot(res.m1,xlab='Time',ylab='Residuals')

par(mfrow=c(2,1))
acf2(res.m1)

#model 2
pass.m1 = arima(data,order=c(0,0,0),seasonal=list(order=c(0,6,0), period=12))
res.m1 = residuals(pass.m1);  
par(mfrow=c(1,1))
plot(res.m1,xlab='Time',ylab='Residuals')

par(mfrow=c(2,1))
acf2(res.m1)

#model 3
pass.m2 = arima(data,order=c(0,0,0),seasonal=list(order=c(5,6,5),period=12))
res.m2 = residuals(pass.m2);  
par(mfrow=c(1,1))
plot(res.m2,xlab='Time',ylab='Residuals',main="Time series plot of the residuals")

acf2(res.m2)
 acf(res.m2, lag.max = 36)
pacf(res.m2, lag.max = 36)

# model 4
log.data = log(data)
par(mfrow=c(1,1))
plot(log.data,ylab='log of number of passengers',xlab='Year')

pass.m3 = arima(log.data,order=c(0,0,0),seasonal=list(order=c(1,1,1), period=12))
res.m3 = residuals(pass.m3)
plot(res.m3,xlab='Time',ylab='Residuals',main="Time series plot of the residuals")

acf2(res.m3)

#model 5
diff.data = diff(data)
par(mfrow=c(1,1))
plot(diff.data,ylab='difference of number of passengers',xlab='Year')

pass.m4 = arima(diff.data,order=c(0,0,0),seasonal=list(order=c(0,0,7), period=12))
res.m4 = residuals(pass.m4)
plot(res.m4,xlab='Time',ylab='Residuals',main="Time series plot of the residuals")

acf2(res.m4)

#model 6
pass.m5 = arima(data,order=c(0,1,0),seasonal=list(order=c(5,6,5), period=12))
res.m5 = residuals(pass.m5)
plot(res.m5,xlab='Time',ylab='Residuals',main="Time series plot of the residuals")

acf2(res.m5) #AR3

adf.test(res.m5)

eacf(res.m5) #ARMA(1,1)(1,2)(2,1)

#fit the arma part
model2.pass = arima(data,order=c(1,1,4),seasonal=list(order=c(5,6,5), period=12),method = "ML")
coeftest(model2.pass)

res.model2 = residuals(model2.pass) 
par(mfrow=c(2,1))
acf(res.model2, lag.max = 36)
pacf(res.model2, lag.max = 36)

#model 2
model3.pass = arima(diff.data,order=c(1,1,1),seasonal=list(order=c(0,1,1), period=12),method = "ML")
coeftest(model3.pass)

res.model3 = residuals(model3.pass) 
par(mfrow=c(2,1))
acf(res.model2, lag.max = 36)
pacf(res.model2, lag.max = 36)

#model 3
model2.pass = arima(diff.data,order=c(1,1,2),seasonal=list(order=c(0,1,1), period=12),method = "ML")
coeftest(model2.pass)

res.model2 = residuals(model2.pass) 
par(mfrow=c(2,1))
acf(res.model2, lag.max = 36)
pacf(res.model2, lag.max = 36)

#model 4
model2.pass = arima(diff(data),order=c(2,1,1),seasonal=list(order=c(0,1,1), period=12),method = "ML")
coeftest(model2.pass)

res.model2 = residuals(model2.pass) 
par(mfrow=c(2,1))
acf(res.model2, lag.max = 36)
pacf(res.model2, lag.max = 36)

sarima.for(data,20,1,1,4,5,6,5,12)
plot(model2.pass,n.ahead = 20)
plot(model2.pass,n.ahead = 40,xlim=c(2015,2025),)

