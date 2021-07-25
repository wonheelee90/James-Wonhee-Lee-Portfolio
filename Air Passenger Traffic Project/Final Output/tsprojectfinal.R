library(TSA)
library(data.table)
library(vars)
library(usdm)

getwd()
setwd("/Users/mackenzieward")

# import data
data = read.csv("Documents/Time Series/Project/TSData.csv",  header = TRUE)[2:8]
data[1:5,]

############################### make 2 data sets #############################
vars_del1 <- c("Int.Travel","Total.Travel")
vars_del_tf <- names(data) %in% vars_del1
data_model1 <- data[!vars_del_tf] # Domestic travel data

vars_del2 <- c("Domestic.Travel","Total.Travel")
vars_del_tf <- names(data) %in% vars_del2
data_model2 <- data[!vars_del_tf] # international travel data

################################# regression #########################
reg1 = lm(data_model1[,1]~ data_model1[,2] +data_model1[,3]+ data_model1[,4] +data_model1[,5])
summary(reg1) # XAL and jet fuel are sig
vif(data_model1)
reg2 = lm(data_model2[,1]~ data_model2[,2] +data_model2[,3]+ data_model2[,4]+data_model2[,5])
summary(reg2) # XAL, jet fuel, and int rate are sig
vif(data_model2)

########################## full models ########################

# remove rows with inf
data.ts1 = diff(diff(diff((ts(data_model1,start=c(2002,10), freq=12)))))
data.ts1.noinf = ts(data.ts1[!rowSums(!is.finite(data.ts1)),], start = c(2002,10), freq=12)

data.ts2 = diff(diff(diff(ts(data_model2,start=c(2002,10), freq=12))))
data.ts2.noinf = ts(data.ts2[!rowSums(!is.finite(data.ts2)),], start = c(2002,10), freq=12)

ts_p1=ts(data[,"Domestic.Travel"],start=c(2002,10), freq=12)
ts_p2=ts(data[,"Int.Travel"],start=c(2002,10), freq=12)

ts_p3=ts(data[,"Total.Travel"],start=c(2002,10), freq=12)
ts_p4=ts(data[,"Unemployment.rate"],start=c(2002,10), freq=12)
ts_p5=ts(data[,"Jet.fuel.price"],start=c(2002,10), freq=12)
ts_p6=ts(data[,"Interest.Rates"],start=c(2002,10), freq=12)
ts_p7=ts(data[,"Opening.XAL"],start=c(2002,10), freq=12)

par(mfrow=c(3,2))
plot(ts_p1,xlab="Year",ylab="Number of Flights",main="Domestic Travel",type="l")
plot(diff(((ts_p1))),xlab="Year",ylab="Number of Flights",main="Differenced Domestic Travel",type="l")
plot(ts_p2,xlab="Year",ylab="Number of Flights",main="International Travel",type="l")
plot(diff(((ts_p2))),xlab="Year",ylab="Number of Flights",main="Differenced International Travel",type="l")
plot(ts_p3,xlab="Year",ylab="Number of Flights",main="Total Travel",type="l")
plot(diff(((ts_p3))),xlab="Year",ylab="Number of Flights",main="Differenced Total Travel",type="l")
plot(ts_p4,xlab="Year",ylab="Average Monthly Rate",main="Umemployment Rate",type="l")
plot(diff(((ts_p4))),xlab="Year",ylab="Average Monthly Rate",main="Differenced Umemployment Rate",type="l")
plot(ts_p5,xlab="Year",ylab="Average Monthly Price",main="Jet Fuel Price",type="l")
plot(diff(((ts_p5))),xlab="Year",ylab="Average Monthly Price",main="Differenced Jet Fuel Price",type="l")
plot(ts_p6,xlab="Year",ylab="Average Monthly Rate",main="Interest Rate",type="l")
plot(diff(((ts_p6))),xlab="Year",ylab="Average Monthly Rate",main="Differenced Interest Rate",type="l")
plot(ts_p7,xlab="Year",ylab="Price",main="Opening XAL",type="l")
plot(diff(((ts_p7))),xlab="Year",ylab="Price",main="Differenced Opening XAL",type="l")

par(mfrow=c(1,1))
plot(data.ts1.noinf, type="l",main="")
acf(data.ts1.noinf, main = 'Domestic Travel')
pacf(data.ts1.noinf)
cor(data.ts1.noinf)

plot(data.ts2.noinf, type="l",main="")
acf(data.ts2.noinf, main = 'International Travel')
pacf(data.ts2.noinf)
cor(data.ts2.noinf)


# Domestic travel
mod_dom = VAR(data.ts1.noinf,lag.max=40,ic="AIC", type="both")
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


################################ subset dom model ######################

vars_del1 <- c("Domestic.Travel","Jet.fuel.price", "Opening.XAL")
vars_del_tf <- names(data_model1) %in% vars_del1
subdata_model1 <- data_model1[vars_del_tf] # Domestic travel data
subdata.ts1.train =diff((ts(subdata_model1[1:171,],start=c(2002,10), freq=12)))
plot(subdata.ts1.train, main = "Differenced Domestic Data")

mod_dom = VAR(subdata.ts1.train,lag.max=20,ic="AIC", type="both")
pord_4 = mod_dom$p
## Fit VAR Model with Selected Order
mod_dom = VAR(subdata.ts1.train,pord_4, type="both")
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


################################# forecast sub dom #################################

dom.predict = predict(mod_dom,n.ahead=12)
varpred1 = dom.predict[[1]]$Domestic.Travel[,1]
domupper = dom.predict[[1]]$Domestic.Travel[,3]
domlower = dom.predict[[1]]$Domestic.Travel[,2]

plot(data[150:183,1],type="l", ylim=c(51078172, 68575481), xlab="Time", ylab="",main = "Actual and Predicted Number of Domestic Flights")
points((22:34), diffinv(varpred1,differences = 1, xi = 59178833),col="red")

varpred1 = diffinv(varpred1,differences = 1, xi = 59178833)
### Mean Squared Prediction Error (MSPE)
mean((varpred1 - data[171:183,1])^2)
### Mean Absolute Prediction Error (MAE)
mean(abs(varpred1 - data[171:183,1]))
### Mean Absolute Percentage Error (MAPE)
mean(abs(varpred1 - data[171:183,1])/data[171:183,1])
### Precision Measure (PM)
sum((varpred1 - data[171:183,1])^2)/sum((data[171:183,1]-mean(data[171:183,1]))^2)



################################### subset int model ##############################

vars_del1 <- c("Unemployment.rate")
vars_del_tf <- names(data_model2) %in% vars_del1
subdata_model2 <- data_model2[!vars_del_tf] # Domestic travel data
subdata.ts2.train = diff(log(ts(subdata_model2[1:171,],start=c(2002,10), freq=12)))
subdata.ts2.noinf = ts(subdata.ts2.train[!rowSums(!is.finite(subdata.ts2.train)),], start = c(2002,10), freq=12)
plot(subdata.ts2.noinf)

mod_int = VAR(subdata.ts2.noinf,lag.max=20,ic="AIC", type="both")
pord_4 = mod_int$p
## Fit VAR Model with Selected Order
mod_int = VAR(subdata.ts2.train,pord_4, type="both")
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

################################# forecast int dom #################################

int.predict = predict(mod_int,n.ahead=12)
varpred2 = int.predict[[1]]$Int.Travel[,1]
idk = exp(diffinv(varpred2,differences = 1, xi = 16.70669))
domupper = int.predict[[1]]$Int.Travel[,3]
domlower = int.predict[[1]]$Int.Travel[,2]

plot(data[150:183,2],type="l", ylim=c(14006160, 22506521), xlab="Time", ylab="",main = "Actual and Predicted Number of International Flights")
points((22:34), idk,col="red")

### Mean Squared Prediction Error (MSPE)
mean((idk - data[171:183,2])^2)
### Mean Absolute Prediction Error (MAE)
mean(abs(idk - data[171:183,2]))
### Mean Absolute Percentage Error (MAPE)
mean(abs(idk - data[171:183,2])/data[171:183,2])
### Precision Measure (PM)
sum((idk - data[171:183,2])^2)/sum((data[171:183,2]-mean(data[171:183,2]))^2)


