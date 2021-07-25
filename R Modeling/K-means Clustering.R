## Load data (iris is included in R)
data("iris")
dim(iris)

## Make various combinations of predictors
preds = iris[,1:4]
cols <- do.call(expand.grid, rep(list(c(F, T)), ncol(preds)))[-1,]

preds01 <- as.data.frame(preds[, as.logical(cols[1,])])
preds02 <- as.data.frame(preds[, as.logical(cols[2,])])
preds03 <- as.data.frame(preds[, as.logical(cols[3,])])
preds04 <- as.data.frame(preds[, as.logical(cols[4,])])
preds05 <- as.data.frame(preds[, as.logical(cols[5,])])
preds06 <- as.data.frame(preds[, as.logical(cols[6,])])
preds07 <- as.data.frame(preds[, as.logical(cols[7,])])
preds08 <- as.data.frame(preds[, as.logical(cols[8,])])
preds09 <- as.data.frame(preds[, as.logical(cols[9,])])
preds10 <- as.data.frame(preds[, as.logical(cols[10,])])
preds11 <- as.data.frame(preds[, as.logical(cols[11,])])
preds12 <- as.data.frame(preds[, as.logical(cols[12,])])
preds13 <- as.data.frame(preds[, as.logical(cols[13,])])
preds14 <- as.data.frame(preds[, as.logical(cols[14,])])
preds15 <- as.data.frame(preds[, as.logical(cols[15,])])

## To find out appropriate number of clusters, we create the elbow graphs below.

wss01 <- (nrow(preds01)-1)*sum(apply(preds01, 2, var))
for (i in 2:15) wss01[i] <- sum(kmeans(preds01, centers=i)$withinss)
plot(1:15, wss01, type="b", xlab="Number of Clusters", ylab="Within Groups Sum of Squares")

wss02 <- (nrow(preds02)-1)*sum(apply(preds02, 2, var))
for (i in 2:15) wss02[i] <- sum(kmeans(preds02, centers=i)$withinss)
plot(1:15, wss02, type="b", xlab="Number of Clusters", ylab="Within Groups Sum of Squares")

wss03 <- (nrow(preds03)-1)*sum(apply(preds03, 2, var))
for (i in 2:15) wss03[i] <- sum(kmeans(preds03, centers=i)$withinss)
plot(1:15, wss03, type="b", xlab="Number of Clusters", ylab="Within Groups Sum of Squares")

wss04 <- (nrow(preds04)-1)*sum(apply(preds04, 2, var))
for (i in 2:15) wss04[i] <- sum(kmeans(preds04, centers=i)$withinss)
plot(1:15, wss04, type="b", xlab="Number of Clusters", ylab="Within Groups Sum of Squares")

wss05 <- (nrow(preds05)-1)*sum(apply(preds05, 2, var))
for (i in 2:15) wss05[i] <- sum(kmeans(preds05, centers=i)$withinss)
plot(1:15, wss05, type="b", xlab="Number of Clusters", ylab="Within Groups Sum of Squares")

wss06 <- (nrow(preds06)-1)*sum(apply(preds06, 2, var))
for (i in 2:15) wss06[i] <- sum(kmeans(preds06, centers=i)$withinss)
plot(1:15, wss06, type="b", xlab="Number of Clusters", ylab="Within Groups Sum of Squares")

wss07 <- (nrow(preds07)-1)*sum(apply(preds07, 2, var))
for (i in 2:15) wss07[i] <- sum(kmeans(preds07, centers=i)$withinss)
plot(1:15, wss07, type="b", xlab="Number of Clusters", ylab="Within Groups Sum of Squares")

wss08 <- (nrow(preds08)-1)*sum(apply(preds08, 2, var))
for (i in 2:15) wss08[i] <- sum(kmeans(preds08, centers=i)$withinss)
plot(1:15, wss08, type="b", xlab="Number of Clusters", ylab="Within Groups Sum of Squares")

wss09 <- (nrow(preds09)-1)*sum(apply(preds09, 2, var))
for (i in 2:15) wss09[i] <- sum(kmeans(preds09, centers=i)$withinss)
plot(1:15, wss09, type="b", xlab="Number of Clusters", ylab="Within Groups Sum of Squares")

wss10 <- (nrow(preds10)-1)*sum(apply(preds10, 2, var))
for (i in 2:15) wss10[i] <- sum(kmeans(preds10, centers=i)$withinss)
plot(1:15, wss10, type="b", xlab="Number of Clusters", ylab="Within Groups Sum of Squares")

wss11 <- (nrow(preds11)-1)*sum(apply(preds11, 2, var))
for (i in 2:15) wss11[i] <- sum(kmeans(preds11, centers=i)$withinss)
plot(1:15, wss11, type="b", xlab="Number of Clusters", ylab="Within Groups Sum of Squares")

wss12 <- (nrow(preds12)-1)*sum(apply(preds12, 2, var))
for (i in 2:15) wss12[i] <- sum(kmeans(preds12, centers=i)$withinss)
plot(1:15, wss12, type="b", xlab="Number of Clusters", ylab="Within Groups Sum of Squares")

wss13 <- (nrow(preds13)-1)*sum(apply(preds13, 2, var))
for (i in 2:15) wss13[i] <- sum(kmeans(preds13, centers=i)$withinss)
plot(1:15, wss13, type="b", xlab="Number of Clusters", ylab="Within Groups Sum of Squares")

wss14 <- (nrow(preds14)-1)*sum(apply(preds14, 2, var))
for (i in 2:15) wss14[i] <- sum(kmeans(preds14, centers=i)$withinss)
plot(1:15, wss14, type="b", xlab="Number of Clusters", ylab="Within Groups Sum of Squares")

wss15 <- (nrow(preds15)-1)*sum(apply(preds15, 2, var))
for (i in 2:15) wss15[i] <- sum(kmeans(preds15, centers=i)$withinss)
plot(1:15, wss15, type="b", xlab="Number of Clusters", ylab="Within Groups Sum of Squares")

## For each plot, starting at 3 clusters, the marginal decrease in WSS per adding a cluster diminishes vastly. 
## While we can reduce the WSS by adding more clusters, it is not relatively worthwhile to do so.

## Next, we figure out which combination of predictors best models the data. 
## We use nstart = 10000 to stabilize the result.
## For each combination, we compute the accuracy of the model via a table with iris$Species.
kmns01 <- kmeans(preds01, 3, nstart = 10000)
table(kmns01$cluster, iris$Species)
acc1 <- (45+35+26)/150

kmns02 <- kmeans(preds02, 3, nstart = 10000)
table(kmns02$cluster, iris$Species)
acc2 <- (31+21+34)/150

kmns03 <- kmeans(preds03, 3, nstart = 10000)
table(kmns03$cluster, iris$Species)
acc3 <- (50+38+35)/150

kmns04 <- kmeans(preds04, 3, nstart = 10000)
table(kmns04$cluster, iris$Species)
acc4 <- (50+48+44)/150

kmns05 <- kmeans(preds05, 3, nstart = 10000)
table(kmns05$cluster, iris$Species)
acc5 <- (50+45+37)/150

kmns06 <- kmeans(preds06, 3, nstart = 10000)
table(kmns06$cluster, iris$Species)
acc6 <- (50+48+41)/150

kmns07 <- kmeans(preds07, 3, nstart = 10000)
table(kmns07$cluster, iris$Species)
acc7 <- (50+45+37)/150

kmns08 <- kmeans(preds08, 3, nstart = 10000)
table(kmns08$cluster, iris$Species)
acc8 <- (50+48+46)/150

kmns09 <- kmeans(preds09, 3, nstart = 10000)
table(kmns09$cluster, iris$Species)
acc9 <- (50+37+35)/150

kmns10 <- kmeans(preds10, 3, nstart = 10000)
table(kmns10$cluster, iris$Species)
acc10 <- (49+46+44)/150

kmns11 <- kmeans(preds11, 3, nstart = 10000)
table(kmns11$cluster, iris$Species)
acc11 <- (50+39+35)/150

kmns12 <- kmeans(preds12, 3, nstart = 10000)
table(kmns12$cluster, iris$Species)
acc12 <- (50+48+46)/150

kmns13 <- kmeans(preds13, 3, nstart = 10000)
table(kmns13$cluster, iris$Species)
acc13 <- (50+48+36)/150

kmns14 <- kmeans(preds14, 3, nstart = 10000)
table(kmns14$cluster, iris$Species)
acc14 <- (50+48+45)/150

kmns15 <- kmeans(preds15, 3, nstart = 10000)
table(kmns15$cluster, iris$Species)
acc15 <- (50+48+36)/150

acc = cbind(acc1, acc2,acc3,acc4,acc5,acc6,acc7,acc8,acc9,acc10,acc11,acc12,acc13,acc14,acc15)
which(acc == max(acc))
cols[which(acc == max(acc)), ]

## The most accurate models are 8, and 12 with 3 clusters.
## These are models involving Petal.Width and both (Petal.Length and Petal.Width) as predictors.
## These models predict iris types at a 96% rate for the given data set.