setwd('/Users/Wonhee/Desktop/Georgia Tech/Fall 2017/ISYE 6414 Regression Analysis/Final Project/the-history-of-baseball')
## Load team data
baseball = read.csv('team.csv', header = TRUE)
head(team)

## Subset for offensive categories
offense = baseball[,c('franchise_id', 'w', 'l', 'r', 'h', 'double', 'triple', 'hr', 'bb', 'so', 'sb', 'cs', 'hbp', 'sf')]
head(offense)

## Filter out missing data
offense_complete = offense[complete.cases(offense), ]
head(offense_complete)

## Exploratory Data Analysis
library(ggplot2)
# Runs vs. Hits
ggplot(offense_complete, aes(x=h, y=r)) + geom_point() + labs(x="Hits") + labs(y="Runs scored")
# Runs vs. Doubles
ggplot(offense_complete, aes(x=double, y=r)) + geom_point() + labs(x="Doubles") + labs(y="Runs scored")
# Runs vs. Triples
ggplot(offense_complete, aes(x=triple, y=r)) + geom_point() + labs(x="Triples") + labs(y="Runs scored")
# Runs vs. Home Runs
ggplot(offense_complete, aes(x=hr, y=r)) + geom_point() + labs(x="Home Runs") + labs(y="Runs scored")
# Runs vs. Base-on-balls
ggplot(offense_complete, aes(x=bb, y=r)) + geom_point() + labs(x="Base-on-balls(Walks)") + labs(y="Runs scored")
# Runs vs. Strikeouts
ggplot(offense_complete, aes(x=so, y=r)) + geom_point() + labs(x="Strikeouts") + labs(y="Runs scored")
# Runs vs. Stolen bases
ggplot(offense_complete, aes(x=sb, y=r)) + geom_point() + labs(x="Stolen bases") + labs(y="Runs scored")
# Runs vs. Caught Stealing
ggplot(offense_complete, aes(x=cs, y=r)) + geom_point() + labs(x="Caught Stealing") + labs(y="Runs scored")
# Runs vs. Hit-by-pitch
ggplot(offense_complete, aes(x=hbp, y=r)) + geom_point() + labs(x="Hit-by-pitch") + labs(y="Runs scored")
# Runs vs. Sacrifice Fly
ggplot(offense_complete, aes(x=sf, y=r)) + geom_point() + labs(x="Sacrifice Flies") + labs(y="Runs scored")

## Correlations between correlations runs and other categories
cor(offense_complete[4:14])

## Multiple linear regression model
mlr <- lm(r ~ h + double + triple + hr + bb + so + sb + cs + hbp + sf, data = offense_complete)
summary(mlr)

## Confidence intervals
confint(mlr, level = 0.99)

## Residual Analysis
plot(mlr$fitted.values, mlr$residuals)

## Plots(includes normality plot)
plot(mlr)

## Checking for outliers
cook = cooks.distance(mlr)
plot(cook,type="h",lwd=2,ylab="Cook's Distance")

## Checking for multicolinearity
library(car)
vif = vif(mlr)
vif

## Variable selection
# Step(AIC)
step(mlr, direction = "both")
# Lasso Regression
library(glmnet)
Y = as.matrix(offense_complete$r)
X = model.matrix(mlr)[,-1]
lasso.cv=cv.glmnet(X,Y,family="gaussian",alpha=1 , nfolds = 10)
lasso = glmnet(X, Y,family="gaussian", alpha = 1, nlambda = 1000)
lambda_min = lasso.cv$lambda.min
lasso_val = coef(lasso, s = lambda_min)
lasso_val
length(attributes(lasso_val)$x)
plot(lasso,xvar="lambda", label = TRUE, lwd=2)
abline(v=log(lambda_min), lwd = 3, lty = 3)

## Which players were the most valuable run producers in 2015?
batting = read.csv('batting.csv', header = TRUE)
batting_2015 = subset(batting, year == 2015)
batting_2015 = batting_2015[c('player_id', 'h', 'double', 'triple', 'hr', 'bb', 'so', 'sb', 'cs', 'hbp', 'sf')]
predictions = predict(mlr, newdata = batting_2015[, 2:11])
top5 = predictions[order(predictions, decreasing = TRUE)[1:5]]
top5_index = c(attributes(top5)$names)
top5_preds = predictions[top5_index]
cbind(batting_2015[top5_index,], top5_preds)
