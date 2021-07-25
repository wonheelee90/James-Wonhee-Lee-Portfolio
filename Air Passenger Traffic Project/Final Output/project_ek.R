library(TSA)
library(data.table)
library(vars)
library(ggfortify)
library(forecast)

#setwd("C:/Users/kmink/Dropbox/[Georgia Tech]MS in Analytics/Isye 6402/project/")

setwd('C:/Users/kmink/Desktop')

## Import data
data = read.csv("TSData.csv",  header = TRUE)[2:8]

ts_p1=ts(data[,"Domestic.Travel"],start=c(2002,10), freq=12)
ts_p2=ts(data[,"Int.Travel"],start=c(2002,10), freq=12)
ts_p4=ts(data[,"Unemployment.rate"],start=c(2002,10), freq=12)
ts_p5=ts(data[,"Jet.fuel.price"],start=c(2002,10), freq=12)
ts_p6=ts(data[,"Interest.Rates"],start=c(2002,10), freq=12)
ts_p7=ts(data[,"Opening.XAL"],start=c(2002,10), freq=12)

par(mfrow=c(2,3))
plot(ts_p1,xlab="Year",ylab="Number of Flights",main="Domestic Travel",type="l")
plot(ts_p2,xlab="Year",ylab="Number of Flights",main="International Travel",type="l")
plot(ts_p4,xlab="Year",ylab="Average Monthly Rate",main="Umemployment Rate",type="l")
plot(ts_p5,xlab="Year",ylab="Average Monthly Price",main="Jet Fuel Price",type="l")
plot(ts_p6,xlab="Year",ylab="Average Monthly Rate",main="Interest Rate",type="l")
plot(ts_p7,xlab="Year",ylab="Price",main="Opening XAL",type="l")

decomposed_domestic = decompose(ts_p1,"additive")
autoplot(decomposed_domestic, main="Decomposition of Domestic Travel")

decomposed_intl = decompose(ts_p2,"additive")
autoplot(decomposed_intl,  main="Decomposition of International Travel")

decomposed_unemp = decompose(ts_p4,"additive")
autoplot(decomposed_unemp)

decomposed_fuel = decompose(ts_p5,"additive")
autoplot(decomposed_fuel)

decomposed_ir = decompose(ts_p6,"additive")
autoplot(decomposed_ir)

decomposed_stk = decompose(ts_p7,"additive")
autoplot(decomposed_stk)


#acf(dd_tp,lag.max=12*16)
#pacf(dd_tp,lag.max=12*16)

acf(decomposed_domestic$random[7:176],lag.max=12*16, main = "ACF of Residual (Domestic Travel)")
pacf(decomposed_domestic$random[7:176],lag.max=12*16, main = "PACF of Residual (Domestic Travel)")

acf(decomposed_intl$random[7:176],lag.max=12*16, main = "ACF of Residual(International Travel)")
pacf(decomposed_intl$random[7:176],lag.max=12*16, main = "PACF of Residual(International Travel)")

acf(decomposed_unemp$random[7:176],lag.max=12*16, main = "ACF of Residual(Unemployment Rate)")
pacf(decomposed_unemp$random[7:176],lag.max=12*16)

acf(decomposed_fuel$random[7:176],lag.max=12*16, main = "ACF of Residual(Jet Fuel Price)")
pacf(decomposed_fuel$random[7:176],lag.max=12*16)

acf(decomposed_ir$random[7:176],lag.max=12*16, main = "ACF of Residual(Interest Rate)")
pacf(decomposed_ir$random[7:176],lag.max=12*16)

acf(decomposed_stk$random[7:176],lag.max=12*16, main = "ACF of Residual(Opening XAL)")
pacf(decomposed_stk$random[7:176],lag.max=12*16)



## Split training and test(for forecasting)
n = nrow(data)
data.train=data[1:(n-12),]
data.test=data[(n-11):n,]


## make 2 data sets
vars_del1 <- c("Int.Travel","Total.Travel")
vars_del_tf <- names(data.train) %in% vars_del1
data.train_model1 <- data.train[!vars_del_tf] # Domestic travel data

vars_del2 <- c("Domestic.Travel","Total.Travel")
vars_del_tf <- names(data.train) %in% vars_del2
data.train_model2 <- data.train[!vars_del_tf] # international travel data

## Create log-differenced time series
data.train.ts1 = diff(log(ts(data.train_model1,start=c(2002,10), freq=12)))
data.train.ts1.noinf = ts(data.train.ts1[!rowSums(!is.finite(data.train.ts1)),], start = c(2002,10), freq=12)

data.train.ts2 = diff(log(ts(data.train_model2,start=c(2002,10), freq=12)))
data.train.ts2.noinf = ts(data.train.ts2[!rowSums(!is.finite(data.train.ts2)),], start = c(2002,10), freq=12)

#Domestic ARIMA
arima_DP<-auto.arima(data.train.ts1.noinf[,1])
arima_DP
ggtsdiag(arima_DP)
forecast_DP <- forecast(arima_DP, level= c(95), h=12)
autoplot(forecast_DP)
forecast_DP$mean

#Domestic Forecast
dom.ar.fcst = forecast_DP$mean
pred.dom.ar = rep(0,12)
lng = nrow(data.train)
pred.dom.ar[1] = log(data.train$Domestic.Travel)[lng]+dom.ar.fcst[1]
pred.dom.ar[2] = pred.dom.ar[1]+dom.ar.fcst[2]
pred.dom.ar[3] = pred.dom.ar[2]+dom.ar.fcst[3]
pred.dom.ar[4] = pred.dom.ar[3]+dom.ar.fcst[4]
pred.dom.ar[5] = pred.dom.ar[4]+dom.ar.fcst[5]
pred.dom.ar[6] = pred.dom.ar[5]+dom.ar.fcst[6]
pred.dom.ar[7] = pred.dom.ar[6]+dom.ar.fcst[7]
pred.dom.ar[8] = pred.dom.ar[7]+dom.ar.fcst[8]
pred.dom.ar[9] = pred.dom.ar[8]+dom.ar.fcst[9]
pred.dom.ar[10] = pred.dom.ar[9]+dom.ar.fcst[10]
pred.dom.ar[11] = pred.dom.ar[10]+dom.ar.fcst[11]
pred.dom.ar[12] = pred.dom.ar[11]+dom.ar.fcst[12]

pred.dom.org.ar = exp(pred.dom.ar)
pred.dom.org.ar
cbind(pred.dom.org.ar, data.test$Domestic.Travel)

par(mfrow = c(1,1))
plot(ts(ts_p1[150:183]),ylab=NULL, main = "Actual and Predicted Number of Domestic Flights")
points(23:34,pred.dom.org.ar, col="red")

plot(data.test$Domestic.Travel)
lines(pred.dom.org.ar)

#International ARIMA
arima_IP<-auto.arima(data.train.ts2.noinf[,1])
arima_IP
ggtsdiag(arima_IP)
forecast_IP <- forecast(arima_IP, leve = c(95), h=12)
autoplot(forecast_IP)
forecast_IP$mean

#International Forecast
int.ar.fcst = forecast_IP$mean
pred.int.ar = rep(0,12)
lng = nrow(data.train)
pred.int.ar[1] = log(data.train$Int.Travel)[lng]+int.ar.fcst[1]
pred.int.ar[2] = pred.int.ar[1]+int.ar.fcst[2]
pred.int.ar[3] = pred.int.ar[2]+int.ar.fcst[3]
pred.int.ar[4] = pred.int.ar[3]+int.ar.fcst[4]
pred.int.ar[5] = pred.int.ar[4]+int.ar.fcst[5]
pred.int.ar[6] = pred.int.ar[5]+int.ar.fcst[6]
pred.int.ar[7] = pred.int.ar[6]+int.ar.fcst[7]
pred.int.ar[8] = pred.int.ar[7]+int.ar.fcst[8]
pred.int.ar[9] = pred.int.ar[8]+int.ar.fcst[9]
pred.int.ar[10] = pred.int.ar[9]+int.ar.fcst[10]
pred.int.ar[11] = pred.int.ar[10]+int.ar.fcst[11]
pred.int.ar[12] = pred.int.ar[11]+int.ar.fcst[12]

pred.int.org.ar = exp(pred.int.ar)

par(mfrow = c(1,1))
plot(ts(ts_p2[150:183]),ylab=NULL, main = "Actual and Predicted Number of International Flights")
points(23:34,pred.int.org.ar, col="red")

plot(data.test$Int.Travel)
lines(pred.int.org.ar)

### Mean Squared Prediction Error (MSPE)
mean((pred.dom.org.ar - data.test$Domestic.Travel)^2)
mean((pred.int.org.ar - data.test$Int.Travel)^2)
### Mean Absolute Prediction Error (MAE)
mean(abs(pred.dom.org.ar - data.test$Domestic.Travel))
mean(abs(pred.int.org.ar - data.test$Int.Travel))
### Mean Absolute Percentage Error (MAPE)
mean(abs(pred.dom.org.ar - data.test$Domestic.Travel)/data.test$Domestic.Travel)
mean(abs(pred.int.org.ar - data.test$Int.Travel)/data.test$Int.Travel)
### Precision Measure (PM)
sum((pred.dom.org.ar - data.test$Domestic.Travel)^2)/sum((data.test$Domestic.Travel-mean(data.test$Domestic.Travel))^2)
sum((pred.int.org.ar - data.test$Int.Travel)^2)/sum((data.test$Int.Travel-mean(data.test$Int.Travel))^2)


#plot(domestic_passenger)

#diff_tp=diff(total_passenger)
#diff_tp=diff_tp/1000
#plot(diff_tp)

#har1=harmonic(diff_tp,1)
#model1=lm(diff_tp~har1)
#summary(model1)
#har2=harmonic(diff_tp,2)
#model2=lm(diff_tp~har2)
#summary(model2)

#dd_tp = ts(diff_tp-fitted(model2),start=c(2002,10), frequency=12)
#plot(dd_tp)

#decomposed_domestic = decompose(domestic_passenger,"additive")
#autoplot(decomposed_domestic)

#decomposed_intl = decompose(intl_passenger,"additive")
#autoplot(decomposed_intl)

#acf(dd_tp,lag.max=12*16)
#pacf(dd_tp,lag.max=12*16)

#acf(decomposed_domestic$random[7:176],lag.max=12*16)
#pacf(decomposed_domestic$random[7:176],lag.max=12*16)

#acf(decomposed_intl$random[7:176],lag.max=12*16)
#pacf(decomposed_intl$random[7:176],lag.max=12*16)

#arimaPS<-auto.arima(total_passenger)
#arimaPS
#arimaorder(arimaPS)

#arimaPS<-auto.arima(total_passenger,max.q=0,max.p=0,max.d=0)
#arimaPS
#ggtsdiag(arimaPS)

#forecastAP <- forecast(arimaPS, level = c(95),h=36)
#autoplot(forecastAP)

#arimaPS<-auto.arima(ln(total_passenger),max.q=0,max.p=0,max.d=0)
#arimaPS
#forecastAP <- forecast(arimaPS, level = c(95),h=36)
#autoplot(forecastAP)

#ts.dom = ts(data[,1],start=c(2002,10), frequency=12)
#month = season(ts.dom)
#model = lm(ts.dom~month-1)
#summary(model)
#resids = ts.dom - ts(fitted(model), start = c(2002,10), freq=12)
#acf(resids)
#pacf(resids)
#diff_deseason = diff(resids)
#acf(diff_deseason)
#pacf(diff_deseason)

#ts.dom_arima<-auto.arima(diff_deseason)
#ts.dom_arima
