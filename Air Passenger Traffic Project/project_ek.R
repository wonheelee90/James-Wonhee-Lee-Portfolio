library(TSA)
library(data.table)
library(vars)
library(ggfortify)
library(forecast)


setwd("C:/Users/kmink/Dropbox/[Georgia Tech]MS in Analytics/Isye 6402/project/")
passenger = read.csv("passengers.csv",header=T)
total_passenger <- ts(passenger[,4],start=c(2002,10), frequency=12)
plot(total_passenger)

diff_tp=diff(total_passenger)
diff_tp=diff_tp/1000
plot(diff_tp)

har1=harmonic(diff_tp,1)
model1=lm(diff_tp~har1)
summary(model1)
har2=harmonic(diff_tp,2)
model2=lm(diff_tp~har2)
summary(model2)

dd_tp = ts(diff_tp-fitted(model2),start=c(2002,10), frequency=12)
plot(dd_tp)

decomposed_data = decompose(total_passenger,"multiplicative")
autoplot(decomposed_data)

acf(dd_tp,lag.max=12*16)
pacf(dd_tp,lag.max=12*16)

acf(decomposed_data$random[7:176],lag.max=12*16)
pacf(decomposed_data$random[7:176],lag.max=12*16)

arimaPS<-auto.arima(total_passenger)
arimaPS
ggtsdiag(arimaPS)

forecastAP <- forecast(arimaPS, level = c(95), h = 36)
autoplot(forecastAP)