library(TSA)
library(data.table)
library(vars)

setwd('/Users/wlee/Desktop/Georgia Tech/Spring 2018/ISYE 6402 Time Series Analysis/Project')

## Import data
data = read.csv("TSData.csv",  header = TRUE)[2:8]

## Split training and test(for forecasting)
n = nrow(data)
data.train=data[1:(n-12),]
data.test=data[(n-11):n,]

## De-trend, de-seasonalize travel data

ts.dom = ts(data.train[, 'Domestic.Travel'], start = c(2002,10), freq=12)
ts.int = ts(data.train[, 'Int.Travel'], start = c(2002,10), freq=12)
ts.unemp = ts(data.train[, 'Unemployment.rate'], start = c(2002,10), freq=12)
ts.fuel = ts(data.train[, 'Jet.fuel.price'], start = c(2002,10), freq=12)
ts.rate = ts(data.train[, 'Interest.Rates'], start = c(2002,10), freq=12)
ts.xal = ts(data.train[, 'Opening.XAL'], start = c(2002,10), freq=12)

month = season(ts.dom)
model = lm(ts.dom~month-1)
summary(model)
resids = ts.dom - ts(fitted(model), start = c(2002,10), freq=12)
diff_deseason = diff(resids)
par(mfrow = c(2,2))
acf(ts.dom, main = "Original Domestic TS")
acf(diff(ts.dom), main = "Differenced Domestic TS")
acf(resids, main = "Deseasonalized Domestic TS")
acf(diff_deseason, main = "Differenced & Deseasonalized Domestic TS")

month = season(ts.int)
model2 = lm(ts.int~month-1)
summary(model2)
resids2 = ts.int - ts(fitted(model2), start = c(2002,10), freq=12)
diff_deseason2 = diff(resids2)
par(mfrow = c(2,2))
acf(ts.int, main = "Original Int'l TS")
acf(diff(ts.int), main = "Differenced Int'l TS")
acf(resids2, main = "Deseasonalized Int'l TS")
acf(diff_deseason2, main = "Differenced & Deseasonalized Int'l TS")

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

## Domestic travel
mod_dom = VAR(data.train.ts1.noinf,lag.max=20,ic="AIC", type="both")
pord_4 = mod_dom$p
## Fit VAR Model with Selected Order
mod_dom = VAR(data.train.ts1.noinf, pord_4, type="both")
summary(mod_dom)

pred.model=predict(mod_dom,n.ahead=12)
dom.fcst = pred.model[[1]]$Domestic.Travel[,1]
pred.dom = rep(0,12)
lng = nrow(data.train)
pred.dom[1] = log(data.train$Domestic.Travel)[lng]+dom.fcst[1]
pred.dom[2] = pred.dom[1]+dom.fcst[2]
pred.dom[3] = pred.dom[2]+dom.fcst[3]
pred.dom[4] = pred.dom[3]+dom.fcst[4]
pred.dom[5] = pred.dom[4]+dom.fcst[5]
pred.dom[6] = pred.dom[5]+dom.fcst[6]
pred.dom[7] = pred.dom[6]+dom.fcst[7]
pred.dom[8] = pred.dom[7]+dom.fcst[8]
pred.dom[9] = pred.dom[8]+dom.fcst[9]
pred.dom[10] = pred.dom[9]+dom.fcst[10]
pred.dom[11] = pred.dom[10]+dom.fcst[11]
pred.dom[12] = pred.dom[11]+dom.fcst[12]

pred.dom.org = exp(pred.dom)
pred.dom.org
cbind(pred.dom.org, data.test$Domestic.Travel)

par(mfrow = c(1,1))
plot(data.test$Domestic.Travel)
lines(pred.dom.org)

## International travel
mod_int = VAR(data.train.ts2.noinf,lag.max=20,ic="AIC", type="both")
pord_4 = mod_int$p
## Fit VAR Model with Selected Order
mod_int = VAR(data.train.ts2.noinf, pord_4, type="both")
summary(mod_int)

pred.model=predict(mod_int,n.ahead=12)
int.fcst = pred.model[[1]]$Int.Travel[,1]
pred.int = rep(0,12)
lng = nrow(data.train)
pred.int[1] = log(data.train$Int.Travel)[lng]+int.fcst[1]
pred.int[2] = pred.int[1]+int.fcst[2]
pred.int[3] = pred.int[2]+int.fcst[3]
pred.int[4] = pred.int[3]+int.fcst[4]
pred.int[5] = pred.int[4]+int.fcst[5]
pred.int[6] = pred.int[5]+int.fcst[6]
pred.int[7] = pred.int[6]+int.fcst[7]
pred.int[8] = pred.int[7]+int.fcst[8]
pred.int[9] = pred.int[8]+int.fcst[9]
pred.int[10] = pred.int[9]+int.fcst[10]
pred.int[11] = pred.int[10]+int.fcst[11]
pred.int[12] = pred.int[11]+int.fcst[12]

pred.int.org = exp(pred.int)

par(mfrow = c(1,1))
plot(data.test$Int.Travel)
lines(pred.int.org)

## Model Fitting: Restricted VAR

## Domestic travel
mod_dom.restrict=restrict(mod_dom)  
summary(mod_dom.restrict)

pred.model=predict(mod_dom.restrict,n.ahead=12)
dom.restrict.fcst = pred.model[[1]]$Domestic.Travel[,1]
pred.dom.restrict = rep(0,12)
lng = nrow(data.train)
pred.dom.restrict[1] = log(data.train$Domestic.Travel)[lng]+dom.restrict.fcst[1]
pred.dom.restrict[2] = pred.dom.restrict[1]+dom.restrict.fcst[2]
pred.dom.restrict[3] = pred.dom.restrict[2]+dom.restrict.fcst[3]
pred.dom.restrict[4] = pred.dom.restrict[3]+dom.restrict.fcst[4]
pred.dom.restrict[5] = pred.dom.restrict[4]+dom.restrict.fcst[5]
pred.dom.restrict[6] = pred.dom.restrict[5]+dom.restrict.fcst[6]
pred.dom.restrict[7] = pred.dom.restrict[6]+dom.restrict.fcst[7]
pred.dom.restrict[8] = pred.dom.restrict[7]+dom.restrict.fcst[8]
pred.dom.restrict[9] = pred.dom.restrict[8]+dom.restrict.fcst[9]
pred.dom.restrict[10] = pred.dom.restrict[9]+dom.restrict.fcst[10]
pred.dom.restrict[11] = pred.dom.restrict[10]+dom.restrict.fcst[11]
pred.dom.restrict[12] = pred.dom.restrict[11]+dom.restrict.fcst[12]

pred.dom.restrict.org = exp(pred.dom.restrict)
pred.dom.restrict.org
cbind(pred.dom.restrict.org, data.test$Domestic.Travel)

par(mfrow = c(1,1))
plot(data.test$Domestic.Travel, xlab="Time", ylab="Domestic Travel")
lines(pred.dom.org, col = "red")
lines(pred.dom.restrict.org, col = "blue")

## International Travel
mod_int.restrict=restrict(mod_int)  
summary(mod_int.restrict)

pred.model=predict(mod_int.restrict,n.ahead=12)
int.restrict.fcst = pred.model[[1]]$Int.Travel[,1]
pred.int.restrict = rep(0,12)
lng = nrow(data.train)
pred.int.restrict[1] = log(data.train$Int.Travel)[lng]+int.restrict.fcst[1]
pred.int.restrict[2] = pred.int.restrict[1]+int.restrict.fcst[2]
pred.int.restrict[3] = pred.int.restrict[2]+int.restrict.fcst[3]
pred.int.restrict[4] = pred.int.restrict[3]+int.restrict.fcst[4]
pred.int.restrict[5] = pred.int.restrict[4]+int.restrict.fcst[5]
pred.int.restrict[6] = pred.int.restrict[5]+int.restrict.fcst[6]
pred.int.restrict[7] = pred.int.restrict[6]+int.restrict.fcst[7]
pred.int.restrict[8] = pred.int.restrict[7]+int.restrict.fcst[8]
pred.int.restrict[9] = pred.int.restrict[8]+int.restrict.fcst[9]
pred.int.restrict[10] = pred.int.restrict[9]+int.restrict.fcst[10]
pred.int.restrict[11] = pred.int.restrict[10]+int.restrict.fcst[11]
pred.int.restrict[12] = pred.int.restrict[11]+int.restrict.fcst[12]

pred.int.restrict.org = exp(pred.int.restrict)
pred.int.restrict.org
cbind(pred.int.restrict.org, data.test$Int.Travel)

par(mfrow = c(1,1))
plot(data.test$Int.Travel, xlab="Time", ylab="Internation Travel")
lines(pred.int.org, col = "red")
lines(pred.int.restrict.org, col = "blue")

## Compute Accuracy Measures 

### Mean Squared Prediction Error (MSPE)
mean((pred.dom.org - data.test$Domestic.Travel)^2)
mean((pred.dom.restrict.org - data.test$Domestic.Travel)^2)
mean((pred.int.org - data.test$Int.Travel)^2)
mean((pred.int.restrict.org - data.test$Int.Travel)^2)
### Mean Absolute Prediction Error (MAE)
mean(abs(pred.dom.org - data.test$Domestic.Travel))
mean(abs(pred.dom.restrict.org - data.test$Domestic.Travel))
mean(abs(pred.int.org - data.test$Int.Travel))
mean(abs(pred.int.restrict.org - data.test$Int.Travel))
### Mean Absolute Percentage Error (MAPE)
mean(abs(pred.dom.org - data.test$Domestic.Travel)/data.test$Domestic.Travel)
mean(abs(pred.dom.restrict.org - data.test$Domestic.Travel)/data.test$Domestic.Travel)
mean(abs(pred.int.org - data.test$Int.Travel)/data.test$Int.Travel)
mean(abs(pred.int.restrict.org - data.test$Int.Travel)/data.test$Int.Travel)
### Precision Measure (PM)
sum((pred.dom.org - data.test$Domestic.Travel)^2)/sum((data.test$Domestic.Travel-mean(data.test$Domestic.Travel))^2)
sum((pred.dom.restrict.org - data.test$Domestic.Travel)^2)/sum((data.test$Domestic.Travel-mean(data.test$Domestic.Travel))^2)
sum((pred.int.org - data.test$Int.Travel)^2)/sum((data.test$Int.Travel-mean(data.test$Int.Travel))^2)
sum((pred.int.restrict.org - data.test$Int.Travel)^2)/sum((data.test$Int.Travel-mean(data.test$Int.Travel))^2)

