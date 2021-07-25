## Read data
ccdata <- read.table("credit_card_data.txt")
colnames(ccdata) = c('a1', 'a2', 'a3', 'a4', 'a5', 'a6', 'a7', 'a8', 'a9', 'a10', 'y')
head(ccdata)
install.packages("kernlab")
library(kernlab)
## Split data into training and test sets using the caret package
install.packages("caret")
library(caret)
set.seed(1)
intrain <- createDataPartition(y = as.factor(ccdata$y), p = .8, list = FALSE)
training <- ccdata[intrain,]
testing <- ccdata[-intrain, ]
invalid <- createDataPartition(y = as.factor(training$y), p = .25, list = FALSE)
validation <- training[invalid, ]
training <- training[-invalid, ]

## Generate several svm models with different "C" values
svm0 <- ksvm(as.matrix(training[,1:10]), as.factor(training[,11]), type="C-svc", kernel="vanilladot", C=0.1, scaled=TRUE)
svm1 <- ksvm(as.matrix(training[,1:10]), as.factor(training[,11]), type="C-svc", kernel="vanilladot", C=1, scaled=TRUE)
svm2 <- ksvm(as.matrix(training[,1:10]), as.factor(training[,11]), type="C-svc", kernel="vanilladot", C=10, scaled=TRUE)
svm3 <- ksvm(as.matrix(training[,1:10]), as.factor(training[,11]), type="C-svc", kernel="vanilladot", C=100, scaled=TRUE)
svm4 <- ksvm(as.matrix(training[,1:10]), as.factor(training[,11]), type="C-svc", kernel="vanilladot", C=1000, scaled=TRUE)
svm5 <- ksvm(as.matrix(training[,1:10]), as.factor(training[,11]), type="C-svc", kernel="vanilladot", C=10000, scaled=TRUE)
svm6 <- ksvm(as.matrix(training[,1:10]), as.factor(training[,11]), type="C-svc", kernel="vanilladot", C=100000, scaled=TRUE)
svm7 <- ksvm(as.matrix(training[,1:10]), as.factor(training[,11]), type="C-svc", kernel="vanilladot", C=1000000, scaled=TRUE)

## Validate different models with validation set, compare accuracies
val0 <- predict(svm0, validation[, 1:10])
val0.acc <- confusionMatrix(val0, validation$y)$overall[1]
val1 <- predict(svm1, validation[, 1:10])
val1.acc <- confusionMatrix(val1, validation$y)$overall[1]
val2 <- predict(svm2, validation[, 1:10])
val2.acc <- confusionMatrix(val2, validation$y)$overall[1]
val3 <- predict(svm3, validation[, 1:10])
val3.acc <- confusionMatrix(val3, validation$y)$overall[1]
val4 <- predict(svm4, validation[, 1:10])
val4.acc <- confusionMatrix(val4, validation$y)$overall[1]
val5 <- predict(svm5, validation[, 1:10])
val5.acc <- confusionMatrix(val5, validation$y)$overall[1]
val6 <- predict(svm6, validation[, 1:10])
val6.acc <- confusionMatrix(val6, validation$y)$overall[1]
val7 <- predict(svm7, validation[, 1:10])
val7.acc <- confusionMatrix(val7, validation$y)$overall[1]
val.acc <- cbind(val0.acc, val1.acc, val2.acc, val3.acc, val4.acc, val5.acc, val6.acc, val7.acc)
val.acc
## Via val.acc, we can see that validation results from models 0-6 are equal at 85.6%.

## Make predictions with the testing set based on the models, compare accuracies
pred0 <- predict(svm0, testing[,1:10])
pred0.acc <- confusionMatrix(pred0, testing$y)$overall[1]
pred1 <- predict(svm1, testing[,1:10])
pred1.acc <- confusionMatrix(pred1, testing$y)$overall[1]
pred2 <- predict(svm2, testing[,1:10])
pred2.acc <- confusionMatrix(pred2, testing$y)$overall[1]
pred3 <- predict(svm3, testing[,1:10])
pred3.acc <- confusionMatrix(pred3, testing$y)$overall[1]
pred4 <- predict(svm4, testing[,1:10])
pred4.acc <- confusionMatrix(pred4, testing$y)$overall[1]
pred5 <- predict(svm5, testing[,1:10])
pred5.acc <- confusionMatrix(pred5, testing$y)$overall[1]
pred6 <- predict(svm6, testing[,1:10])
pred6.acc <- confusionMatrix(pred6, testing$y)$overall[1]
pred7 <- predict(svm7, testing[,1:10])
pred7.acc <- confusionMatrix(pred7, testing$y)$overall[1]
pred.acc <- cbind(pred0.acc, pred1.acc, pred2.acc, pred3.acc, pred4.acc, pred5.acc, pred6.acc, pred7.acc)
pred.acc
## Again, prediction accuracies tested on the testing set are identical for models 0-6.
## Thus, we conclude that the highest prediction accuracy can be obtained from a variety of models involving "C" values from 0.1 to 100000.

## As an example, the equation of the classifier using C = 100 is a[1]*V1 + a[2]*V2 + ... + a[10]*V10 + a0 = 0 given a and a0 below.
## Compute a1...am
a <- colSums(svm3@xmatrix[[1]] * svm3@coef[[1]])
## Compute a0
a0 <- -svm3@b
As <- data.frame("a" = c("a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10"), 
                 "value" = c(a0, a))
As
