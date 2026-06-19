rm(list=ls())
library(ISLR2)
attach(Boston)
set.seed(1)
library(leaps)
library(glmnet)
library(pls)



n <- nrow(Boston)
train <- sort(sample(1:n, n/2))
test <- (1:n)[-train]
Boston.train <- Boston[train,]
Boston.test <- Boston[test,]

y.train <- as.vector(Boston[train, 1])
x.train <- as.matrix(Boston[train, -1])

y.test <- as.vector(Boston[test, 1])
x.test <- as.matrix(Boston[test, -1])



# (a)
# Best subset selection
bss.fit <- regsubsets(crim ~., Boston.train, nvmax=12)
bss.bestIndex <- which.min(summary(bss.fit)$bic)

# Lasso
lasso.fit <- glmnet(x.train, y.train, alpha=1)
lasso.bestLambda <- cv.glmnet(x.train, y.train,
                              alpha=1)$lambda.min
lasso.fit.best <- glmnet(x.train, y.train, alpha=1,
                         lambda=lasso.bestLambda)

# Ridge
ridge.fit <- glmnet(x.train, y.train, alpha=0)
ridge.bestLambda <- cv.glmnet(x.train, y.train,
                              alpha=0)$lambda.min
ridge.fit.best <- glmnet(x.train, y.train, alpha=0,
                         lambda=ridge.bestLambda)

# PCR
pcr.fit <- pcr(crim ~., data=Boston.train, scale=TRUE,
               validation="CV")
pcr.bestncomp <- 10


# (b)
# Best subset selection
predict.regsubsets <-
  function(object, newdata, id, ...) {
    form <- as.formula(object$call[[2]])
    mat <- model.matrix(form, newdata)
    coefi <- coef(object, id=id)
    xvars <- names(coefi)
    mat[,xvars] %*% coefi
  }
bss.pred <- predict(bss.fit, Boston.test,
                    id=bss.bestIndex)
bss.testMSE <- mean((bss.pred - y.test)^2)

# Lasso
lasso.pred <- predict(lasso.fit.best, newx=x.test)
lasso.testMSE <- mean((lasso.pred - y.test)^2)

# Ridge
ridge.pred <- predict(ridge.fit.best, newx=x.test)
ridge.testMSE <- mean((ridge.pred - y.test)^2)

# PCR
pcr.pred <- predict(pcr.fit, x.test,
                    ncomp=pcr.bestncomp)
pcr.testMSE <- mean((pcr.pred - y.test)^2)

# The model that seems to perform best (using
# validation set error) is ridge regression at
# lambda = 0.5919, with test MSE = 40.17



# (c)
predict(ridge.fit.best, type="coefficients")
# the Chosen model involves all of the features in
# the data set.