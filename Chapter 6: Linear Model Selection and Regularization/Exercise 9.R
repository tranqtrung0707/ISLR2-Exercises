rm(list=ls())
library(ISLR2)
attach(College)
set.seed(1)


# (a)
n <- nrow(College)
train <- sample(c(TRUE, FALSE), n, replace=TRUE)
test <- !train
data.train <- College[train,]
data.test <- College[test,]

# (b)
linear.fit <- lm(Apps ~., data=College, subset=train)
linear.pred <- predict(linear.fit, data.test)
linear.testerror <-
  mean((linear.pred - data.test$Apps)^2)
# The MSE is 984743

# (c)
library(glmnet)
x <- model.matrix(Apps ~., data.train)[,-1]
y <- data.train$Apps
ridge.cv <- cv.glmnet(x, y, alpha=0)
plot(ridge.cv)
ridge.bestlam <- ridge.cv$lambda.min
ridge.mod <- glmnet(x, y, alpha=0, lambda=ridge.bestlam)
ridge.pred <- predict(ridge.mod, s=ridge.bestlam,
                      newx =
                        model.matrix(Apps ~., data.test)[,-1])
ridge.testerror <-
  mean((ridge.pred - data.test$Apps)^2)
# The MSE is 941653

# (d)
lasso.cv <- cv.glmnet(x, y, alpha=1)
plot(lasso.cv)
lasso.bestlam <- lasso.cv$lambda.min
lasso.mod <- glmnet(x, y, alpha=1, lambda=lasso.bestlam)
lasso.pred <- predict(lasso.mod, s=lasso.bestlam,
                      newx =
                        model.matrix(Apps ~., data.test)[,-1])
lasso.testerror <-
  mean((lasso.pred - data.test$Apps)^2)
# The MSE is 979301
predict(lasso.mod, type="coefficients",
        s=lasso.bestlam)

# (e)
library(pls)
pcr.fit <- pcr(Apps ~., data=College, subset=train,
               scale=TRUE, validation="CV")
validationplot(pcr.fit, val.type="MSEP")
# M selected by cross-validation is 17
pcr.pred <- predict(pcr.fit, data.test[,-2], ncomp=17)
mean((pcr.pred - data.test[,2])^2)
# The MSE is 984743.1

# (f)
pls.fit <- plsr(Apps ~., data=College, subset=train,
                scale=TRUE, validation="CV")
validationplot(pls.fit, val.type="MSEP")
# M selected by cross-validation is 7
pls.pred <- predict(pls.fit, data.test[,-2], ncomp=7)
mean((pls.pred - data.test[,2])^2)
# The MSE is 1000494

# (g)
# We can predict the number of applications somewhat
# accurately. There is not much difference among the
# test errors resulting from these five approaches.