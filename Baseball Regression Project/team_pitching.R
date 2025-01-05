#Team Pitching Analysis
setwd('/Users/Sagar/Documents/ISYE6414/project/the-history-of-baseball')
data <- read.csv('team.csv', header = TRUE)
defence = data[,c('franchise_id', 'w', 'l', 'era','ha','hra','bba','soa','e')]
defence = na.omit(defence)
head(defence)
attach(defence)

#Exploratory Data Analysis : Data distribution
data_y=data.frame(value=defence['era'])
ggplot(data_y, aes(x=defence['era'])) + geom_histogram(binwidth = 0.2, aes(fill = ..count..) ) + labs(x="Earned run average(response)")

# Exploratory Data Analysis : Scatter plots
library(ggplot2)
ggplot(defence, aes(x=ha, y=era)) + geom_point() + labs(x="Hits allowed") + labs(y="Earned run average")
ggplot(defence, aes(x=hra, y=era)) + geom_point() + labs(x="Home runs allowed") + labs(y="Earned run average")
ggplot(defence, aes(x=bba, y=era)) + geom_point() + labs(x="Walks allowed") + labs(y="Earned run average")
ggplot(defence, aes(x=soa, y=era)) + geom_point() + labs(x="Strikeouts") + labs(y="Earned run average")
library(car)
scatterplotMatrix(~ era + ha + hra + bba + soa, data = defence, smooth = FALSE)

# Exploratory Data Analysis :Correlation
cor(defence[4:8])

# Scaling the predictors 
s_ha= scale(ha)
s_hra = scale(hra)
s_bba = scale(bba)
s_soa = scale(soa)

# Multiple linear regression scaled model
model <- lm(era ~ s_ha+s_hra+s_bba+s_soa, data = defence)
summary(model)

# Confidence interval
confint(model,level = 0.99)

#Checking for outliers
cook = cooks.distance(model)
plot(cook,type="h",lwd=2,ylab="Cook's Distance")

#Residual Analysis
ggplot(model, aes(x=model$fitted, y=model$residuals)) + geom_point() + labs(x="Fitted values") + labs(y="Residuals")
qqPlot(model$residuals,ylab="Residuals",xlab="Norm Quantiles")
ggplot(model, aes(x=model$residuals)) + geom_histogram(binwidth = 0.2, aes(fill = ..count..) ) + labs(x="Residuals")

#Checking for multicollinearity
vif(model)




