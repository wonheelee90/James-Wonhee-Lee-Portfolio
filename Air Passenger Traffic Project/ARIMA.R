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

ts.dom = diff(log(ts(data.train[, 'Domestic.Travel'], start = c(2002,10), freq=12)))
ts.int = diff(log(ts(data.train[, 'Int.Travel'], start = c(2002,10), freq=12)))

n = length(ts.dom)

## ARIMA
## Order selection -- Domestic Travel
dom.aic=Inf
dom.order=c(0,0,0)
for (p in 1:12) for (d in 0:1) for (q in 1:12) {
  current.aic=AIC(arima(ts.dom,order=c(p, d, q)))
  if(current.aic<final.aic) 
  {
    dom.aic=current.aic
    dom.order=c(p,d,q)
    dom.arima=arima(ts.dom, order=dom.order)
  }
}
dom.order

## Forecasting
nfit = n-12
outvol = arima(ts.dom[1:nfit], order = dom.order, method = "ML")
out_pred = as.vector(predict(outvol,n.ahead=12))

timevol=time(ts.dom)
plot(timevol[(n-11):n],ts.dom[(n-11):n],type="l", xlab="Time", ylab="Domestic.Travel")
points(timevol[(nfit+1):n],out_pred$pred,col="red")

## Order selection -- International Travel
int.aic=Inf
int.order=c(0,0,0)
for (p in 1:12) for (d in 0:1) for (q in 1:12) {
  current.aic=AIC(arima(ts.int,order=c(p, d, q)))
  if(current.aic<final.aic) 
  {
    int.aic=current.aic
    int.order=c(p,d,q)
    int.arima=arima(ts.int, order=int.order)
  }
}
int.order

## Forecasting
nfit = n-12
outvol = arima(ts.dom[1:nfit], order = dom.order, method = "ML")
out_pred = as.vector(predict(outvol,n.ahead=12))

timevol=time(volume.ts)
ubound = out_pred$pred+1.96*out_pred$se
lbound = out_pred$pred-1.96*out_pred$se
ymin = min(lbound)
ymax = max(ubound)
plot(timevol[(n-56):n],volume.ts[(n-56):n],type="l", ylim=c(ymin,ymax), xlab="Time", ylab="ED Volume")
points(timevol[(nfit+1):n],out_pred$pred,col="red")
lines(timevol[(nfit+1):n],ubound,lty=3,lwd= 2, col="blue")
lines(timevol[(nfit+1):n],lbound,lty=3,lwd= 2, col="blue")














#Residual Analysis
resids = resid(final.arima)[-1]
squared.resids=resids^2

par(mfcol=c(2,1))
plot(resids,main='Residuals of Domestic.Travel ARIMA Fit')
plot(squared.resids,main='Squared Residuals of Domestic.Travel ARIMA Fit')

par(mfcol=c(2,1))
acf(resids,main='ACF Residuals of Domestic.Travel ARIMA Fit')
acf(squared.resids,main='ACF Squared Residuals of Domestic.Travel ARIMA Fit')

#test for serial correlation
Box.test(resids,lag=12,type='Ljung',fitdf=11)
#test for arch effect
Box.test((resids)^2,lag=12,type='Ljung',fitdf=11)
