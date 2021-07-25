data <- read.table("uscrime.txt", header = TRUE)

## Get principal components
pr <- prcomp(data[,1:15], scale = TRUE)

model <- lm(data$Crime ~ ., data)
## Fit regression model with first 4 principal componenets
prmodel <- lm(data$Crime ~ pr$x[,1] + pr$x[,2] + pr$x[,3] + pr$x[,4])
## Regression coefficients for the PCA regression model
bk <- prmodel$coefficients[2:5]
eigenvs <- pr$rotation
vjk <- t(eigenvs[,1:4])
orcoef <- bk%*%vjk
orcoef

library(MASS)
model2 <- stepAIC(model)
c(summary(model2)$adj.r.squared, summary(prmodel)$adj.r.squared)

## We can see that our previous model is superior in terms of adj.r-squared.