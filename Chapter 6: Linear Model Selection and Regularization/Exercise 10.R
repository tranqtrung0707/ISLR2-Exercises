rm(list=ls())
set.seed(1)
library(leaps)



# (a)
X <- rnorm(20*1000)
X <- matrix(X, 1000, 20)
colnames(X) <- sapply(1:20, function(i) paste0("X",i))
beta <- sample(c(0,1,2), 20, replace=TRUE)
names(beta) <- colnames(X)
e <- rnorm(1000)
Y <- X %*% beta + e
Y <- drop(Y)
data <- as.data.frame(cbind(Y, X))

# (b)
train <- sort(sample(1:1000, 900))
test <- (1:1000)[-train]
data.train <- data[train,]
data.test <- data[test,]

# (c)
bss.fit <- regsubsets(Y~., data=data, subset=train,
                      nvmax=20)
bss.summary <- summary(bss.fit)
bss.MSE.train <- bss.summary$rss / 1000
plot(bss.MSE.train, type="l")

# (d)
predict.regsubsets <-
  function(object, newdata, id, ...) {
    form <- as.formula(object$call[[2]])
    mat <- model.matrix(form, newdata)
    coefi <- coef(object, id=id)
    xvars <- names(coefi)
    mat[,xvars] %*% coefi
  }

bss.MSE.test <- sapply(1:20, function(i) {
  pred <- predict(bss.fit, data[test,],
                             id=i)
  #print(dim(pred))
  return (mean((pred - data[test,1])^2))
})

plot(bss.MSE.test, type="l")


# (e)
bss.best.index <- which.min(bss.MSE.test)
# the best model size based on test MSE has 11 coefs,
# which is exactly the number of betas that are not 0.

# (f)
coef(bss.fit, bss.best.index)
beta
# The coefficient values for the model with 11 coefs
# are close to the true value

# (g)
coef.dist <- sapply(1:20, function(i) {
  beta_hat <- coef(bss.fit, i)[-1]
  beta_true <- beta[names(beta_hat)]
  return (sqrt(sum((beta_true - beta_hat)^2)))
})
plot(coef.dist, type="l")
# In terms of minimizing the distance between beta and
# beta_hat, the best model is the one that contains
# 11 coefficients. This is also the model with the
# lowest test MSE