install.packages("kknn")
library(kknn)
## Read in data
ccdata <- read.table("credit_card_data.txt")
## Designate column names
colnames(ccdata) <- c('a1', 'a2', 'a3', 'a4', 'a5', 'a6', 'a7', 'a8', 'a9', 'a10', 'r')
## Split data into training & test set using random sampling
set.seed(125)
training <- sample(1:dim(ccdata)[1], size = round(dim(ccdata)[1]*.8), replace = FALSE)
train <- ccdata[training, ]
test <- ccdata[-training, ]
## Use train.kknn to determine optimal "k" <= 100
kn_mod <- train.kknn(r ~., train, kmax = 100, scale = TRUE)
## Predict model on test set. Round the prediction result so that it will yield either 0 or 1 as the outcome.
fit <- predict(kn_mod, test)
pred <- table(test$r, round(fit))
## Gauge model accuracy
mod.acc <- sum(pred[1,1], pred[2,2])/sum(pred[1,1], pred[1,2], pred[2,1], pred[2,2])
## Call regular kknn model to check result. Use parameters used previously from train.kknn.
kn_mod2 <- kknn(r ~ ., train, test, distance = 2, k = 54, kernel = "optimal", scale = TRUE)
fit2 <- fitted(kn_mod2)
pred2 <- table(test$r, round(fit2))
mod.acc2 <- sum(pred[1,1], pred[2,2])/sum(pred[1,1], pred[1,2], pred[2,1], pred[2,2])

## Compare the two procedures, confirm they yield the same result.
c(mod.acc, mod.acc2)

## Optimal model yields a prediction accuracy of 84.73% for this particular sample of training/test data.