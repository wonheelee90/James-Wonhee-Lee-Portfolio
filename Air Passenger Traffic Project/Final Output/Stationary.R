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

## Extract dometic and international travel
ts.dom = ts(data.train[, 'Domestic.Travel'], start = c(2002,10), freq=12)
ts.int = ts(data.train[, 'Int.Travel'], start = c(2002,10), freq=12)

## Difference, de-seasonalize domestic travel
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

## Difference, de-seasonalize international travel
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