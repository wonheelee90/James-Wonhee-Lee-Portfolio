library(glmnet)
library(MASS)
library(ResourceSelection)
library(caret)
library(ggplot2)
library(car)

setwd("C:/Felipe/GaTech/ISyE 6414/Project/the-history-of-baseball/")

# import hall of fame batters dataset

batting_hof <- read.table("batting_hof.csv", header = TRUE, sep=",")

# remove the index, games and at bat colums

batting_hof <- subset(batting_hof, select = -c(X, g, ab))

# remove outlier

batting_hof <- batting_hof[-c(328),]

# creates a training set and test set, 80% and 20%

set.seed(3456)
trainIndex <- createDataPartition(batting_hof$inducted, p = .8, list = FALSE, times = 1)

# scales the training dataset

batting_hoft <- batting_hof[trainIndex,]

s_r <- c(scale(batting_hoft$r))
s_h <- c(scale(batting_hoft$h))
s_double <- c(scale(batting_hoft$double))
s_triple <- c(scale(batting_hoft$triple))
s_hr <- c(scale(batting_hoft$hr))
s_rbi <- c(scale(batting_hoft$rbi))
s_sb <- c(scale(batting_hoft$sb))
s_cs <- c(scale(batting_hoft$cs))
s_bb <- c(scale(batting_hoft$bb))
s_so <- c(scale(batting_hoft$so))
s_ibb <- c(scale(batting_hoft$ibb))
s_hbp <- c(scale(batting_hoft$hbp))
s_sh <- c(scale(batting_hoft$sh))
s_sf <- c(scale(batting_hoft$sf))
s_g_idp <- c(scale(batting_hoft$g_idp))
s_seasons <- c(scale(batting_hoft$seasons))

# initial model

batting.model <- glm(batting_hoft$inducted~s_r+s_h+s_double+s_triple+s_hr+s_rbi+s_sb+s_cs+
                       s_bb+s_so+s_ibb+s_hbp+s_sh+s_sf+s_g_idp+s_seasons, data=batting_hoft, family = "binomial")

summary(batting.model)

# calculates cooks distance to find possible outliers

cook = cooks.distance(batting.model)
plot(cook,type="h",lwd=2,ylab="Cook's Distance")

outliers <- which.max(cook)

plot(batting.model)

# lasso

X_batting <- cbind(s_r,s_h,s_double,s_triple,s_hr,s_rbi,s_sb,s_cs,s_bb,s_so,s_ibb,s_hbp,s_sh,s_sf,s_g_idp,s_seasons)

lasso.batting.cv <- cv.glmnet(X_batting, batting_hoft$inducted, family='binomial', alpha=1, type.measure = 'deviance', nfolds=10)

lasso.batting <- glmnet(X_batting, batting_hoft$inducted, family='binomial', alpha=1, nlambda=100)

coef(lasso.batting.cv, s=lasso.batting.cv$lambda.min)

batting.model.lasso <- glm(batting_hoft$inducted~s_h+s_hr+s_triple+s_r+s_sb+s_hbp+
                             s_sh+s_g_idp+s_sf, data=batting_hoft, family = "binomial")

summary(batting.model.lasso)

# lasso hoslem

for(i in 5:15){
  hosmer <- hoslem.test(batting_hoft$inducted,batting.model.lasso$fitted.values, g=i)
  print(hosmer$p.value)
}

# lasso prediction

batting.prediction.lasso <- function(h, hr, triple, ibb, sb, cs, sf, g_idp, sh, seasons, ab){
  p_h <- (h-mean(batting_hof$h))/sd(batting_hof$h)
  p_hr <- (hr-mean(batting_hof$hr))/sd(batting_hof$hr)
  p_triple <- (triple-mean(batting_hof$triple))/sd(batting_hof$triple)
  p_ibb <- (ibb-mean(batting_hof$ibb))/sd(batting_hof$ibb)
  p_sb <- (sb-mean(batting_hof$sb))/sd(batting_hof$sb)
  p_cs <- (cs-mean(batting_hof$cs))/sd(batting_hof$cs)
  p_sf <- (sf-mean(batting_hof$sf))/sd(batting_hof$sf)
  p_g_idp <- (g_idp-mean(batting_hof$g_idp))/sd(batting_hof$g_idp)
  p_sh <- (sh-mean(batting_hof$sh))/sd(batting_hof$sh)
  p_seasons <- (seasons-mean(batting_hof$seasons))/sd(batting_hof$seasons)
  p_ab <- (ab-mean(batting_hof$ab))/sd(batting_hof$ab)
  return(predict(batting.model.lasso, newdata = data.frame(s_h = p_h, s_hr = p_hr, s_triple = p_triple, s_ibb = p_ibb,
                                                           s_sb = p_sb, s_cs = p_cs, s_sf = p_sf, s_g_idp = p_g_idp,
                                                           s_sh = p_sh, s_seasons = p_seasons, s_ab = p_ab), type="response"))
  
}
results_pred <- c()
batting_hoftest <- batting_hof[-trainIndex,]
for(i in 1:nrow(batting_hoftest)){
  prediction <- batting.prediction.lasso(batting_hoftest[i,]$h,batting_hoftest[i,]$hr,batting_hoftest[i,]$triple,batting_hoftest[i,]$ibb,batting_hoftest[i,]$sb,batting_hoftest[i,]$cs,batting_hoftest[i,]$bb, batting_hoftest[i,]$r,batting_hoftest[i,]$sh,batting_hoftest[i,]$seasons, batting_hoftest[i,]$ab)
  results_pred <- c(results_pred, prediction)
} 

# classify data, using 0.5 as threshold

results_binary <- ifelse(results_pred<0.5, 0, 1)

# confusion matrix for lasso

cmlasso <- confusionMatrix(results_binary,batting_hof[-trainIndex,]$inducted)

# stepwise model

batting.step <- stepAIC(batting.model)

batting.model.step <- glm(batting_hoft$inducted~s_h+s_hr+s_ibb+s_sb+s_cs+
                            s_so+s_sh+s_seasons, data=batting_hoft, family = "binomial")

summary(batting.model.step)



# stepwise hoslem

for(i in 5:15){
  hosmer <- hoslem.test(batting_hoft$inducted,batting.model.step$fitted.values, g=i)
  print(hosmer$p.value)
}

plot(batting.model.step)

vif(batting.model.step)



# stepwise prediction

batting.prediction.step <- function(h, hr, ibb, sb, cs, so, sh, seasons){
  p_h <- (h-mean(batting_hof$h))/sd(batting_hof$h)
  p_hr <- (hr-mean(batting_hof$hr))/sd(batting_hof$hr)
  p_ibb <- (ibb-mean(batting_hof$ibb))/sd(batting_hof$ibb)
  p_sb <- (sb-mean(batting_hof$sb))/sd(batting_hof$sb)
  p_cs <- (cs-mean(batting_hof$cs))/sd(batting_hof$cs)
  p_so <- (so-mean(batting_hof$so))/sd(batting_hof$so)
  p_sh <- (sh-mean(batting_hof$sh))/sd(batting_hof$sh)
  p_seasons <- (seasons-mean(batting_hof$seasons))/sd(batting_hof$seasons)
  return(predict(batting.model.step, newdata = data.frame(s_h = p_h, s_hr = p_hr, s_ibb = p_ibb,
                                                          s_sb = p_sb, s_cs = p_cs, s_so = p_so,
                                                          s_sh = p_sh, s_seasons = p_seasons), type="response"))
  
}
results_pred <- c()
batting_hoftest <- batting_hof[-trainIndex,]
for(i in 1:nrow(batting_hoftest)){
  prediction <- batting.prediction.step(batting_hoftest[i,]$h,batting_hoftest[i,]$hr,batting_hoftest[i,]$ibb,batting_hoftest[i,]$sb,batting_hoftest[i,]$cs,batting_hoftest[i,]$so,batting_hoftest[i,]$sh,batting_hoftest[i,]$seasons)
  results_pred <- c(results_pred, prediction)
} 

hist(results_pred,breaks = 50)

# classify data, using 0.5 as threshold

results_binary <- ifelse(results_pred<0.5, 0, 1)

# confusion matrix for AIC

cmaic <- confusionMatrix(results_binary,batting_hof[-trainIndex,]$inducted)

ggplot(batting_hof, aes(x=h, y=inducted)) + geom_point() + labs(x="Hits") + labs(y="Inducted")
ggplot(batting_hof, aes(x=hr, y=inducted)) + geom_point() + labs(x="Home Runs") + labs(y="Inducted")
ggplot(batting_hof, aes(x=triple, y=inducted)) + geom_point() + labs(x="Triple Runs") + labs(y="Inducted")
ggplot(batting_hof, aes(x=ibb, y=inducted)) + geom_point() + labs(x="Intentional base on balls") + labs(y="Inducted")
ggplot(batting_hof, aes(x=sb, y=inducted)) + geom_point() + labs(x="Stolen Bases") + labs(y="Inducted")
ggplot(batting_hof, aes(x=cs, y=inducted)) + geom_point() + labs(x="Caught Steeling") + labs(y="Inducted")
ggplot(batting_hof, aes(x=bb, y=inducted)) + geom_point() + labs(x="BBase on Balls") + labs(y="Inducted")
ggplot(batting_hof, aes(x=r, y=inducted)) + geom_point() + labs(x="Runs") + labs(y="Inducted")
ggplot(batting_hof, aes(x=sh, y=inducted)) + geom_point() + labs(x="Sacrifice Hits") + labs(y="Inducted")
ggplot(batting_hof, aes(x=seasons, y=inducted)) + geom_point() + labs(x="Seasons") + labs(y="Inducted")
ggplot(batting_hof, aes(x=ab, y=inducted)) + geom_point() + labs(x="At Bat") + labs(y="Inducted")
ggplot(batting_hof, aes(x=double, y=inducted)) + geom_point() + labs(x="Double Runs") + labs(y="Inducted")
ggplot(batting_hof, aes(x=g, y=inducted)) + geom_point() + labs(x="Games") + labs(y="Inducted")
ggplot(batting_hof, aes(x=rbi, y=inducted)) + geom_point() + labs(x="Runs Batted In") + labs(y="Inducted")
ggplot(batting_hof, aes(x=so, y=inducted)) + geom_point() + labs(x="Strike Out") + labs(y="Inducted")
ggplot(batting_hof, aes(x=hbp, y=inducted)) + geom_point() + labs(x="Times Hit by Pitches") + labs(y="Inducted")
ggplot(batting_hof, aes(x=sf, y=inducted)) + geom_point() + labs(x="Sacrifice Fliers") + labs(y="Inducted")
ggplot(batting_hof, aes(x=g_idp, y=inducted)) + geom_point() + labs(x="Grounded into double play") + labs(y="Inducted")

# correlation for the dataset

correlation <- cor(batting_hof)

# VIF for the AIC model

vif(batting.model.step)

hist(batting.model.step$residuals)

plot(batting.model.step)

