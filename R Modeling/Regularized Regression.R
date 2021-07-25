data = read.csv("GA_EDVisits.csv",header=TRUE)
data = na.omit(data)
# Get names of the column
names = colnames(data)
attach(data)
#standardized predictors
sAvgDist = scale(log(SpecDist))
sAvgDistP = scale(log(PedDist))
sMedianIncome = scale(MedianIncome)
sNumHospitals = scale(No.Hospitals)
sPercentLessHS = scale(PercentLessHS)
sPercentHS = scale(PercentHS)

# Logistic Regression Model
newdata = as.data.frame(cbind(ED.visits, Asthma_children, A5.9, A10.14, A15.17, sNumHospitals, sPercentLessHS, sPercentHS, sMedianIncome, sAvgDist, sAvgDistP))
colnames(newdata) = c("ED.visits", "Asthma_children", "A5.9", "A10.14", "A15.17", "sNumHospitals", "sPercentLessHS", "sPercentHS", "sMedianIncome", "sAvgDist", "sAvgDistP")
head(newdata)

mod1 <- glm(ED.visits/Asthma_children ~ ., data = newdata, weight = Asthma_children, family = "binomial")
summary(mod1)

# Create interaction terms
DistA5.9 = sAvgDist*A5.9
DistA10.14 = sAvgDist* A10.14
DistIncome = sAvgDist*sMedianIncome
DistLessHS =  sAvgDist*sPercentLessHS
DistHS = sAvgDist*sPercentHS
DistPA5.9 = sAvgDistP*A5.9
DistPA10.14 = sAvgDistP* A10.14
DistPIncome = sAvgDistP*sMedianIncome
DistPLessHS =  sAvgDistP*sPercentLessHS
DistPHS = sAvgDistP*sPercentHS
newdata2 = cbind(newdata, DistA5.9, DistA10.14, DistIncome, DistLessHS, DistHS, DistPA5.9, DistPA10.14, DistPIncome, DistPLessHS, DistPHS)
dim(newdata2)

# Logistic Regression w/ Interactions
mod2 <- glm(ED.visits/Asthma_children ~ ., data = newdata2, weight = Asthma_children, family = "binomial")
summary(mod2)

# Forward stepwise regression
mod.null <- glm(ED.visits/Asthma_children ~ 1, data = newdata2, weight = Asthma_children, family = "binomial")
step(mod1, scope = list(lower= mod1, upper = mod2), direction = "forward")

# Selected model summary
summary(glm(formula = ED.visits/Asthma_children ~ A5.9 + A10.14 + A15.17 + 
        sNumHospitals + sPercentLessHS + sPercentHS + sMedianIncome + 
        sAvgDist + sAvgDistP + DistPLessHS + DistHS + DistPHS + DistIncome + 
        DistPA5.9 + DistPA10.14 + DistA10.14 + DistLessHS, family = "binomial", 
    data = newdata, weights = Asthma_children))

# Backward stepwise regression
step(mod2, scope = list(lower= mod1, upper = mod2), direction = "backward")

# Selected model(backward)
glm(formula = ED.visits/Asthma_children ~ A5.9 + A10.14 + A15.17 + 
        sNumHospitals + sPercentLessHS + sPercentHS + sMedianIncome + 
        sAvgDist + sAvgDistP + DistA10.14 + DistIncome + DistLessHS + 
        DistHS + DistPA5.9 + DistPA10.14 + DistPLessHS + DistPHS, 
    family = "binomial", data = newdata2, weights = Asthma_children)

## Lasso Regression
Y = cbind(ED.visits, Asthma_children-ED.visits)
X = cbind(A5.9, A10.14, sAvgDist, sAvgDistP, sMedianIncome,
          sPercentLessHS, sPercentHS, sNumHospitals,DistA5.9,
          DistA10.14, DistIncome, DistLessHS, DistHS, DistPA5.9,
          DistPA10.14, DistPIncome, DistPLessHS, DistPHS)
library(glmnet)
lasso.cv=cv.glmnet(X,Y,family=c("binomial"),alpha=1, type.measure="deviance",nfolds=10)
lasso = glmnet(X, Y,family=c("binomial"), alpha = 1, nlambda = 100)
lasso_val = coef(lasso, s = lasso.cv$lambda.min)
lasso_val
length(attributes(lasso_val)$x)
plot(lasso,xvar="lambda",lwd=2)

## Elastic Net
en.cv=cv.glmnet(X,Y,family=c("binomial"),alpha= 0.5,type.measure="deviance",nfolds=10)
en = glmnet(X, Y,family=c("binomial"), alpha = 0.5, nlambda = 100)
en_val = coef(en,s=en.cv$lambda.min)
length(attributes(en_val)$x)
plot(en,xvar="lambda",lwd=2)
