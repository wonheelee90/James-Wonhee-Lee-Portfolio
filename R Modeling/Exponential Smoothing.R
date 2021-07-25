install.packages("xlsx")
library(xlsx)

#read in weather data
data = (as.data.frame(read.table("temps.txt",header=T)))

#stack up the temperature data in one column for ease of transfer on time series data
data<-data.frame(data[,1],stack(data[,2:21]))

#Convert the data into time serise data format
ts_data<-ts(data$values,frequency=123,start=c(1996))

#Obtain the exponential smoothing fitted value Using HoltWinters function    
fit1 <-HoltWinters(ts_data, seasonal = c("additive"))

#Get alpha = 0.6610618, beta = 0, gamma = 0.6248075
fit1

#Check the fitted value
fit1$fitted

#To see summer has gotten later over 20 years, we will use CUSUM method on 'season' value. To do that, store the season value.  
season <- fit1$fitted[,4]

#Export season value as xlsx
season <- as.data.frame(matrix(season, nrow = 123, ncol = 19))
write.csv(season, file="season.csv")
