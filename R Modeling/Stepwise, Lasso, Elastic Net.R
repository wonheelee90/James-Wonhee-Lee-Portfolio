crime <- read.table("uscrime.txt", header =T)

#Stepwise regression

set.seed(1000)
null = lm(Crime~1, data=crime)
full = lm(Crime~.,data=crime)
stepwise = step(null, scope=list(upper=full),data=crime,direction="both")
summary(stepwise)


#convert data to matrix
crime <- data.matrix(crime)


library("glmnet")


#lasso
set.seed(1000)
lasso <- cv.glmnet(crime[,1:15],crime[,16],family='gaussian',alpha=1, standardize = TRUE )
coef(lasso,s="lambda.min")


#elastic net

alphas = seq(0, 1, 0.1)
MSEs = c()
for (a in alphas) {
    set.seed(1000)
    elastic <- cv.glmnet(crime[,1:15],crime[,16],family='gaussian',alpha=a, standardize = TRUE )
    minMSE <- elastic$cvm[which(elastic$lambda == elastic$lambda.min)] # get the lowest MSE value
    MSEs = c(MSEs, minMSE)
}
#The model with minimum MSE : alpha = 1
which.min(MSEs)

# Retrieve coefficients of the model
set.seed(1000)
elastic <- cv.glmnet(crime[,1:15],crime[,16],family='gaussian',alpha=1, standardize = TRUE )
coef(elastic,s="lambda.min")
