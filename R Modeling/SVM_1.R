## Read data
ccdata <- read.table("credit_card_data.txt")
head(ccdata)
install.packages("kernlab")
library(kernlab)
## Call ksvm model
model <- ksvm(as.matrix(ccdata[,1:10]), as.factor(ccdata[,11]), type="C-svc", kernel="vanilladot", C=100, scaled=TRUE)
## Compute a1...am
a <- colSums(model@xmatrix[[1]] * model@coef[[1]])
## Compute a0
a0 <- -model@b
## Make prediction based on model
pred <- predict(model, ccdata[,1:10])

## Measure model accuracy
acc1 <- sum(pred == ccdata[,11])/nrow(ccdata)

## Repeat process for different values of "C"
## C = 50
model2 <- ksvm(as.matrix(ccdata[,1:10]), as.factor(ccdata[,11]), type="C-svc", kernel="vanilladot", C=50, scaled=TRUE)
pred2 <- predict(model2, ccdata[,1:10])
acc2 <- sum(pred2 == ccdata[,11])/nrow(ccdata)

## C = 1000
model3 <- ksvm(as.matrix(ccdata[,1:10]), as.factor(ccdata[,11]), type="C-svc", kernel="vanilladot", C=1000, scaled=TRUE)
pred3 <- predict(model3, ccdata[,1:10])
acc3 <- sum(pred3 == ccdata[,11])/nrow(ccdata)

## C = 1
model4 <- ksvm(as.matrix(ccdata[,1:10]), as.factor(ccdata[,11]), type="C-svc", kernel="vanilladot", C=1, scaled=TRUE)
pred4 <- predict(model4, ccdata[,1:10])
acc4 <- sum(pred4 == ccdata[,11])/nrow(ccdata)

## C = 100000000000
model5 <- ksvm(as.matrix(ccdata[,1:10]), as.factor(ccdata[,11]), type="C-svc", kernel="vanilladot", C=100000000000, scaled=TRUE)
pred5 <- predict(model5, ccdata[,1:10])
acc5 <- sum(pred5 == ccdata[,11])/nrow(ccdata)

## C = 0.00000001
model6 <- ksvm(as.matrix(ccdata[,1:10]), as.factor(ccdata[,11]), type="C-svc", kernel="vanilladot", C=0.00000001, scaled=TRUE)
pred6 <- predict(model6, ccdata[,1:10])
acc6 <- sum(pred6 == ccdata[,11])/nrow(ccdata)

## C = 0.5
model7 <- ksvm(as.matrix(ccdata[,1:10]), as.factor(ccdata[,11]), type="C-svc", kernel="vanilladot", C=0.5, scaled=TRUE)
pred7 <- predict(model7, ccdata[,1:10])
acc7 <- sum(pred7 == ccdata[,11])/nrow(ccdata)

## Compare model accuracy for different values of "C"
c_vs_acc <- data.frame("C" = c(0.00000001, 0.5, 1, 50, 100, 1000, 100000000000), "acc" = c(acc6, acc7, acc4, acc2, acc1, acc3, acc5))
c_vs_acc

## After trial-and-error, C values from 0.5 to 100 yield roughly identical accuracies in 0.8639144.
## The equation of the classifier using C = 100 is a[1]*V1 + a[2]*V2 + ... + a[10]*V10 + a0 = 0 given a and a0 above.
As <- data.frame("A" = c("a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10"), 
                 "Value" = c(a0, a))
As