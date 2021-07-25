install.packages("kknn")
library(kknn)
## Read in data
ccdata <- read.table("credit_card_data.txt")
## Designate column names
colnames(ccdata) <- c('a1', 'a2', 'a3', 'a4', 'a5', 'a6', 'a7', 'a8', 'a9', 'a10', 'y')
## Split data into training & test set using the caret package
install.packages("caret")
library(caret)
set.seed(1)
intrain <- createDataPartition(y = as.factor(ccdata$y), p = .8, list = FALSE)
training <- ccdata[intrain,]
testing <- ccdata[-intrain, ]

## Create folds for cross-validation
folds <- createFolds(y= as.factor(training$y), k = 4, list = TRUE)
fold1 <- training[folds[[1]],]
fold2 <- training[folds[[2]],]
fold3 <- training[folds[[3]],]
fold4 <- training[folds[[4]],]
train1 <- rbind(fold2, fold3, fold4)
train2 <- rbind(fold1, fold3, fold4)
train3 <- rbind(fold1, fold2, fold4)
train4 <- rbind(fold1, fold2, fold3)

## Develop models based on cross-validation & evaluate accuracy
kn_mod1.1 <- kknn(y ~ ., train1, fold1, distance = 2, k = 5, kernel = "optimal", scale = TRUE)
kn_mod1.2 <- kknn(y ~ ., train2, fold2, distance = 2, k = 5, kernel = "optimal", scale = TRUE)
kn_mod1.3 <- kknn(y ~ ., train3, fold3, distance = 2, k = 5, kernel = "optimal", scale = TRUE)
kn_mod1.4 <- kknn(y ~ ., train4, fold4, distance = 2, k = 5, kernel = "optimal", scale = TRUE)

acc_mod1.1 <- confusionMatrix(round(predict(kn_mod1.1)), fold1$y)$overall[1]
acc_mod1.2 <- confusionMatrix(round(predict(kn_mod1.2)), fold2$y)$overall[1]
acc_mod1.3 <- confusionMatrix(round(predict(kn_mod1.3)), fold3$y)$overall[1]
acc_mod1.4 <- confusionMatrix(round(predict(kn_mod1.4)), fold4$y)$overall[1]
acc_mod1 <- cbind(acc_mod1.1, acc_mod1.2, acc_mod1.3, acc_mod1.4)

kn_mod2.1 <- kknn(y ~ ., train1, fold1, distance = 2, k = 20, kernel = "optimal", scale = TRUE)
kn_mod2.2 <- kknn(y ~ ., train2, fold2, distance = 2, k = 20, kernel = "optimal", scale = TRUE)
kn_mod2.3 <- kknn(y ~ ., train3, fold3, distance = 2, k = 20, kernel = "optimal", scale = TRUE)
kn_mod2.4 <- kknn(y ~ ., train4, fold4, distance = 2, k = 20, kernel = "optimal", scale = TRUE)
acc_mod2.1 <- confusionMatrix(round(predict(kn_mod2.1)), fold1$y)$overall[1]
acc_mod2.2 <- confusionMatrix(round(predict(kn_mod2.2)), fold2$y)$overall[1]
acc_mod2.3 <- confusionMatrix(round(predict(kn_mod2.3)), fold3$y)$overall[1]
acc_mod2.4 <- confusionMatrix(round(predict(kn_mod2.4)), fold4$y)$overall[1]
acc_mod2 <- cbind(acc_mod2.1, acc_mod2.2, acc_mod2.3, acc_mod2.4)

kn_mod3.1 <- kknn(y ~ ., train1, fold1, distance = 2, k = 50, kernel = "optimal", scale = TRUE)
kn_mod3.2 <- kknn(y ~ ., train2, fold2, distance = 2, k = 50, kernel = "optimal", scale = TRUE)
kn_mod3.3 <- kknn(y ~ ., train3, fold3, distance = 2, k = 50, kernel = "optimal", scale = TRUE)
kn_mod3.4 <- kknn(y ~ ., train4, fold4, distance = 2, k = 50, kernel = "optimal", scale = TRUE)
acc_mod3.1 <- confusionMatrix(round(predict(kn_mod3.1)), fold1$y)$overall[1]
acc_mod3.2 <- confusionMatrix(round(predict(kn_mod3.2)), fold2$y)$overall[1]
acc_mod3.3 <- confusionMatrix(round(predict(kn_mod3.3)), fold3$y)$overall[1]
acc_mod3.4 <- confusionMatrix(round(predict(kn_mod3.4)), fold4$y)$overall[1]
acc_mod3 <- cbind(acc_mod3.1, acc_mod3.2, acc_mod3.3, acc_mod3.4)

kn_mod4.1 <- kknn(y ~ ., train1, fold1, distance = 2, k = 100, kernel = "optimal", scale = TRUE)
kn_mod4.2 <- kknn(y ~ ., train2, fold2, distance = 2, k = 100, kernel = "optimal", scale = TRUE)
kn_mod4.3 <- kknn(y ~ ., train3, fold3, distance = 2, k = 100, kernel = "optimal", scale = TRUE)
kn_mod4.4 <- kknn(y ~ ., train4, fold4, distance = 2, k = 100, kernel = "optimal", scale = TRUE)
acc_mod4.1 <- confusionMatrix(round(predict(kn_mod4.1)), fold1$y)$overall[1]
acc_mod4.2 <- confusionMatrix(round(predict(kn_mod4.2)), fold2$y)$overall[1]
acc_mod4.3 <- confusionMatrix(round(predict(kn_mod4.3)), fold3$y)$overall[1]
acc_mod4.4 <- confusionMatrix(round(predict(kn_mod4.4)), fold4$y)$overall[1]
acc_mod4 <- cbind(acc_mod4.1, acc_mod4.2, acc_mod4.3, acc_mod4.4)

## Table the accuracies where row = k and col = training set used.
acc_all <- rbind(acc_mod1, acc_mod2, acc_mod3, acc_mod4)
row.names(acc_all) <- c('k = 5', 'k = 20', 'k = 50', 'k = 100')
colnames(acc_all) <- c('train1', 'train2', 'train3', 'train4')

## Compute average accuracy depending on k
acc_all <- cbind(acc_all, "avg.acc" = apply(acc_all, 2, mean))
acc_all

## From the table, we can see that the best average accuracy of 85.38% is achieved when k=5.
## Therefore, we go back to the full training set and train the model with k=5.
kn_best <- kknn(y~., training, testing, distance = 2, k = 5, kernel = "optimal", scale = TRUE)
best.acc <- confusionMatrix(round(fitted(kn_best)), testing$y)$overall[1]
best.acc

## The model yields roughly a 79.23% accuracy on the testing set data.
