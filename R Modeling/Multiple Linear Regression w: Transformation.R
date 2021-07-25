load('AmesHousing.RData')
head(house)

# Exploratory Data Analysis
attach(house)
library(car)
scatterplotMatrix(~ SalePrice + MS.Zoning + Lot.Area + Lot.Shape
                  + Land.Contour, data = house, smooth = FALSE)
scatterplotMatrix(~ SalePrice + Neighborhood + Condition.1 + Bldg.Type
                  + House.Style, data = house, smooth = FALSE)
scatterplotMatrix(~ SalePrice + Overall.Qual + Overall.Cond
                  + Year.Built + Year.Remod.Add, data = house, smooth = FALSE)
scatterplotMatrix(~ SalePrice + Roof.Style + Mas.Vnr.Type
                  + Mas.Vnr.Area + Exter.Qual, data = house, smooth = FALSE)
scatterplotMatrix(~ SalePrice + Exter.Cond + Foundation
                  + Bsmt.Qual + Bsmt.Cond, data = house, smooth = FALSE)
scatterplotMatrix(~ SalePrice + BsmtFin.SF + Total.Bsmt.SF
                  + Heating.QC + Central.Air, data = house, smooth = FALSE)
scatterplotMatrix(~ SalePrice + X1st.Flr.SF + Gr.Liv.Area
                  + Bedroom.AbvGr + Kitchen.Qual, data = house, smooth = FALSE)
scatterplotMatrix(~ SalePrice + TotRms.AbvGrd + Fireplaces
                  + Fireplace.Qu + Garage.Type, data = house, smooth = FALSE)
scatterplotMatrix(~ SalePrice + Garage.Finish + Garage.Cars
                  + Garage.Area + Paved.Drive, data = house, smooth = FALSE)
scatterplotMatrix(~ SalePrice + Wood.Deck.SF + Open.Porch.SF
                  + Enclosed.Porch + X3Ssn.Porch, data = house, smooth = FALSE)
scatterplotMatrix(~ SalePrice + Screen.Porch + Pool.Area + Fence
                  + Sale.Type, data = house, smooth = FALSE)
scatterplotMatrix(~ SalePrice + Sale.Condition + Yr.Sold + Bath,
                  data = house, smooth = FALSE)

## Model & Residual analysis
mod = lm(SalePrice ~ ., house)
plot(fitted(mod), residuals(mod))

X = model.matrix(lm(SalePrice ~ ., data = house))[,-1]
# Construct the model matrix of predictors
X = cbind(SalePrice, X) #attach sale price to predictors
library(corrplot)
corrplot(cor(X), tl.cex = .1) #product correlation plot

# Box-cox transformation
library(MASS)
bc <- boxcox(mod)
lambda <- bc$x[which.max(bc$y)]

# Log-transformation
mod.log <- lm(log(SalePrice) ~ ., house)
plot(fitted(mod.log), residuals(mod.log))
fitted(mod.log)

# Influential Observations
# added variable plots 
av.Plots(mod.log)
# Cook's D plot
# identify D values > 1
plot(mod.log, which=4, cook.levels=1)
# Influence Plot 
influencePlot(mod.log,	id.method="identify", main="Influence Plot")

# Model w/o outliers
newhouse = house[-c(182, 2180, 2181), ]
mod.new_log <- lm(log(SalePrice) ~., newhouse)
summary(mod.new_log)

# Check for normality of new model
qqnorm(mod.new_log$residuals)

# Drop 1 varaible
drop <- drop1(mod.new_log)
which.min(drop$AIC)

#Stepwise selection w/ AIC
stepAIC(mod.new_log, direction = "both")

# Lasso Regression
set.seed(1)
library(glmnet)
Y = as.matrix(log(newhouse$SalePrice))
X = model.matrix(mod.new_log)[,-1]
lasso.cv=cv.glmnet(X,Y,family="gaussian",alpha=1 , nfolds = 10)
lasso = glmnet(X, Y,family="gaussian", alpha = 1, nlambda = 100)
lasso.cv$lambda.min
lasso_val = coef(lasso, s = lasso.cv$lambda.min)
lasso_val
length(attributes(lasso_val)$x)
plot(lasso,xvar="lambda", label = TRUE, lwd=2)

# Elastic Net
enet.cv=cv.glmnet(X,Y,family="gaussian",alpha= 0.5, nfolds = 10)
enet = glmnet(X, Y,family="gaussian", alpha = 0.5, nlambda = 100)
enet.cv$lambda.min
enet_val = coef(enet, s = enet.cv$lambda.min)
enet_val
length(attributes(enet_val)$x)
plot(enet, xvar="lambda", label = TRUE, lwd=2)

# Group LASSO
library(gglasso)
preds = newhouse[,-c(SalePrice)]
groups = c()
for (i in 1:ncol(preds)) {
    if (class(preds[,i]) == 'factor') {
        grp_len = length(attributes(preds[,i])$levels)
        groups = c(groups, rep(i, grp_len))
    }
    else {
        groups = c(groups, i)
    }
}
gglasso.cv = cv.gglasso(X, Y, nfolds = 10)
gglasso.cv = cv.gglasso(X, Y, loss = 'ls')
