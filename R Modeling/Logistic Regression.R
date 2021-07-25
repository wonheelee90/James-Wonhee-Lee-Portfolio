mx1 = as.data.frame(cbind(A10.14, logit(ED.visits/Asthma_children)))
mx1 = mx1[mx1$V2 != -Inf, ]
cor(mx1[,1], mx1[,2])

data = read.csv("GA_EDVisits.csv",header=TRUE)
data = na.omit(data)
# Get names of the column
names = colnames(data)
attach(data)
#standardized predictors
sAvgDistS = scale(log(SpecDist))
sAvgDistP = scale(log(PedDist))
sMedianIncome = scale(MedianIncome)
sNumHospitals = scale(No.Hospitals)
sPercentLessHS = scale(PercentLessHS)
sPercentHS = scale(PercentHS)

# New data based on scaled predictors
newdata = as.data.frame(cbind(ED.visits, Asthma_children, A5.9, A10.14, A15.17, sNumHospitals, sPercentLessHS, sPercentHS, sMedianIncome, sAvgDistS, sAvgDistP))
colnames(newdata) = c("ED.visits", "Asthma_children", "A5.9", "A10.14", "A15.17", "sNumHospitals", "sPercentLessHS", "sPercentHS", "sMedianIncome", "sAvgDistS", "sAvgDistP")
head(newdata)

# Fit model
mod <- glm(ED.visits/Asthma_children ~ ., data = newdata, weight = Asthma_children, family = "binomial")
summary(mod)

# Calculate residual deviance
c(deviance(mod), 1-pchisq(deviance(mod),447))
pearres = residuals(mod,type="pearson")
pearson.tvalue = sum(pearres^2)
c(pearson.tvalue, 1-pchisq(pearson.tvalue,447))

attr(,"scaled:center")
[1] 0.9276316
attr(,"scaled:scale")
[1] 1.188416

