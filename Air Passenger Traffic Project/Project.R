library(TSA)
library(data.table)
library(vars)

setwd('/Users/wlee/Desktop/Georgia Tech/Spring 2018/ISYE 6402 Time Series Analysis/Project')

# import data
data = read.csv("TSData.csv",  header = TRUE)[2:8]
data

# make 2 data sets
vars_del1 <- c("Int.Travel","Total.Travel")
vars_del_tf <- names(data) %in% vars_del1
data_model1 <- data[!vars_del_tf] # Domestic travel data

vars_del2 <- c("Domestic.Travel","Total.Travel")
vars_del_tf <- names(data) %in% vars_del2
data_model2 <- data[!vars_del_tf] # international travel data

# remove rows with inf
data.ts1 = diff(log(ts(data_model1,start=c(2002,10), freq=12)))
data.ts1.noinf = ts(data.ts1[!rowSums(!is.finite(data.ts1)),], start = c(2002,10), freq=12)

data.ts2 = diff(log(ts(data_model2,start=c(2002,10), freq=12)))
data.ts2.noinf = ts(data.ts2[!rowSums(!is.finite(data.ts2)),], start = c(2002,10), freq=12)

##################################### EDA #########################################

ts_p1=ts(data[,"Domestic.Travel"],start=c(2002,10), freq=12)
ts_p2=ts(data[,"Int.Travel"],start=c(2002,10), freq=12)
ts_p3=ts(data[,"Total.Travel"],start=c(2002,10), freq=12)
ts_p4=ts(data[,"Unemployment.rate"],start=c(2002,10), freq=12)
ts_p5=ts(data[,"Jet.fuel.price"],start=c(2002,10), freq=12)
ts_p6=ts(data[,"Interest.Rates"],start=c(2002,10), freq=12)
ts_p7=ts(data[,"Opening.XAL"],start=c(2002,10), freq=12)

par(mfrow=c(4,2))

plot(ts_p1,xlab="Year",ylab="Number of Flights",main="Domestic Travel",type="l")
plot(diff(log(ts_p1)),xlab="Year",ylab="Number of Flights",main="Differenced Domestic Travel",type="l")
plot(ts_p2,xlab="Year",ylab="Number of Flights",main="International Travel",type="l")
plot(diff(log(ts_p2)),xlab="Year",ylab="Number of Flights",main="Differenced International Travel",type="l")
plot(ts_p3,xlab="Year",ylab="Number of Flights",main="Total Travel",type="l")
plot(diff(log(ts_p3)),xlab="Year",ylab="Number of Flights",main="Differenced Total Travel",type="l")
plot(ts_p4,xlab="Year",ylab="Average Monthly Rate",main="Umemployment Rate",type="l")
plot(diff(log(ts_p4)),xlab="Year",ylab="Average Monthly Rate",main="Differenced Umemployment Rate",type="l")
plot(ts_p5,xlab="Year",ylab="Average Monthly Price",main="Jet Fuel Price",type="l")
plot(diff(log(ts_p5)),xlab="Year",ylab="Average Monthly Price",main="Differenced Jet Fuel Price",type="l")
plot(ts_p6,xlab="Year",ylab="Average Monthly Rate",main="Interest Rate",type="l")
plot(diff(log(ts_p6)),xlab="Year",ylab="Average Monthly Rate",main="Differenced Interest Rate",type="l")
plot(ts_p7,xlab="Year",ylab="Price",main="Opening XAL",type="l")
plot(diff(log(ts_p7)),xlab="Year",ylab="Price",main="Differenced Opening XAL",type="l")


plot(data.ts1.noinf, type="l",main="")
acf(data.ts1.noinf)
pacf(data.ts1.noinf)
cor(data.ts1.noinf)

plot(data.ts2.noinf, type="l",main="")
acf(data.ts2.noinf)
pacf(data.ts2.noinf)
cor(data.ts2.noinf)

################################# VAR ##############################################

# Domestic travel
mod_dom = VAR(data.ts1.noinf,lag.max=20,ic="AIC", type="both")
pord_4 = mod_dom$p
## Fit VAR Model with Selected Order
mod_dom = VAR(data.ts1.noinf,pord_4, type="both")
summary(mod_dom)
## Residual Analysis: Constant Variance Assumption
arch.test(mod_dom)
## Residual Analysis: Normality Assumption
normality.test(mod_dom)
## Residual Analysis: Uncorrelated Errors Assumption
serial.test(mod_dom)
serialtest = serial.test(mod_dom)
plot(serialtest)
## roots analysis: is this VAR process stable?
roots_mod = roots(mod_dom)
sum(roots_mod>=1)

# International travel
mod_int = VAR(data.ts2.noinf,lag.max=20,ic="AIC", type="both")
pord_4 = mod_int$p
## Fit VAR Model with Selected Order
mod_int = VAR(data.ts1.noinf,pord_4, type="both")
summary(mod_int)
## Residual Analysis: Constant Variance Assumption
arch.test(mod_int)
## Residual Analysis: Normality Assumption
normality.test(mod_int)
## Residual Analysis: Uncorrelated Errors Assumption
serial.test(mod_int)
serialtest = serial.test(mod_int)
plot(serialtest)
## roots analysis: is this VAR process stable?
roots_mod = roots(mod_int)
sum(roots_mod>=1)

################################### Forecast ######################################
