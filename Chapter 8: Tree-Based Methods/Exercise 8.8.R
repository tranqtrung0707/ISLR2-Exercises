rm(list=ls())
library(ISLR2)
attach(Carseats)
library(randomForest)
library(BART)



# (a)
n <- nrow(Carseats)
p <- ncol(Carseats) - 1
set.seed(10)
train <- sample(1:n, n/2)
Carseats.train <- Carseats[train,]
Carseats.test <- Carseats[-train,]



# (b)
tree.fit <- tree(Sales ~., data = Carseats.train)
plot(tree.fit)
text(tree.fit)
tree.pred <- predict(tree.fit,
                     newdata = Carseats.test)
tree.testMSE <-
  mean((tree.pred - Carseats.test$Sales)^2)
# The test MSE is 5.202



# (c)
cv.tree.fit <- cv.tree(tree.fit,
                       FUN = prune.tree)
bestSize <-
  cv.tree.fit$size[which.min(cv.tree.fit$dev)]
tree.prune <- prune.tree(tree.fit, best=bestSize)
tree.prune.pred <- predict(tree.prune,
                               Carseats.test)
tree.prune.testMSE <-
  mean((tree.prune.pred - Carseats.test$Sales)^2)
# The pruned tree test MSE is 4.855, better than
# the unpruned tree test MSE.



# (d)
bag.fit <- randomForest(Sales ~., data=Carseats.train,
                        mtry=p, importance=TRUE)
bag.pred <- predict(bag.fit, Carseats.test)
bag.testMSE <- mean((bag.pred - Carseats.test$Sales)^2)
# The bagging test MSE is 2.999, better than
# the pruned tree test MSE.
importance(bag.fit)
# The most important variables are shelveloc and price



# (e)
rf.fits <- lapply(1:p, function(i) {
  randomForest(Sales ~., data=Carseats.train,
                       mtry=i, importance=TRUE)
})
rf.preds <- lapply(1:p, function(i) {
  predict(rf.fits[[i]], Carseats.test)
})
rf.testMSEs <- sapply(1:p, function(i) {
  mean((rf.preds[[i]] - Carseats.test$Sales)^2)
})
plot(rf.testMSEs)
# The highest test MSE is 4.44 when m=1. The lowest
# test MSE is 2.928 when m=9.
# As m increases from 1 to 3 (sqrt of p),
# the test MSE declines rapidly. It does not vary much
# going from m=3 to m=12 (which is bagging).
importance(rf.fits[[9]])
# ShelveLoc and Price are the most important variables



# (f)
x <- Carseats[,-1]
y <- Carseats[,1]
xtrain <- x[train,]
ytrain <- y[train]
xtest <- x[-train,]
ytest <- y[-train]
bart.fit <- gbart(xtrain, ytrain, xtest)
bart.pred <- bart.fit$yhat.test.mean
bart.testMSE <- mean((bart.pred - ytest)^2)
# The BART test MSE is 1.575