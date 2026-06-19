rm(list=ls())
library(ISLR2)
library(gbm)
library(randomForest)
library(glmnet)
set.seed(1)



# (a)
Hitters <- na.omit(Hitters)
Hitters$Salary <- log(Hitters$Salary)



# (b)
Hitters.train <- Hitters[1:200,]
Hitters.test <- Hitters[-(1:200),]



# (c)
lambdas <- exp(seq(log(0.001), log(0.1), length.out=100))
boost.fits <- lapply(1:100, function(i) {
  gbm(Salary ~., data = Hitters.train,
      distribution = "gaussian",
      n.trees = 1000, shrinkage = lambdas[i])
})
trainMSEs <- sapply(1:100, function(i) {
  boost.fits[[i]]$train.error[1000]
})
plot(lambdas, trainMSEs)



# (d)
testMSEs <- sapply(1:100, function(i) {
  pred <- predict(boost.fits[[i]], Hitters.test)
  return (mean((pred - Hitters.test$Salary)^2))
})
plot(lambdas, testMSEs)
# The test MSE (for reasonable shrinkage) ranges from
# 0.25 to 0.28



# (e)
lm.fit <- lm(Salary ~., data=Hitters.train)
lm.pred <- predict(lm.fit, Hitters.test)
lm.testMSE <- mean((lm.pred - Hitters.test$Salary)^2)
# The linear regression test MSE is 0.492

x <- model.matrix(Salary ~., Hitters.train)[,-1]
y <- Hitters.train$Salary
x.test <- model.matrix(Salary ~., Hitters.test)[,-1]
lasso.fit <- glmnet(x, y, alpha=1)
lasso.pred <- predict(lasso.fit, newx=x.test)
lasso.testMSE <- mean((lasso.pred -
                         Hitters.test$Salary)^2)
# The lasso test MSE is 0.476

ridge.fit <- glmnet(x, y, alpha=0)
ridge.pred <- predict(ridge.fit, newx=x.test)
ridge.testMSE <- mean((ridge.pred -
                         Hitters.test$Salary)^2) 
# The ridge test MSE is 0.515


# (f)
summary(boost.fits[[100]])
# CAtBat, CRBI



# (g)
bag.fit <- randomForest(Salary ~., data = Hitters.train,
                        mtry=19, importance=TRUE)
bag.pred <- predict(bag.fit, Hitters.test)
bag.testMSE <- mean((bag.pred - Hitters.test$Salary)^2)
# The bagging test MSE is 0.233