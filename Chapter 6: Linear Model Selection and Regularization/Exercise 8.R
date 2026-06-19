rm(list=ls())



# (a)
X <- rnorm(100, mean=10, sd=2)
e <- rnorm(100)

# (b)
Y <- 1 + 2*X + 3*X^2 + 4*X^3 + e



# (c)
data <- cbind(Y, sapply(1:10, function(i) X^i))
colnames(data)[-1] <- sapply(1:10,
                          function(i) paste0("X^", i))
data <- data.frame(data)

library(leaps)
regfit.full <- regsubsets(Y~., data=data, nvmax=10)
regfit.full.summary <- summary(regfit.full)

par(mfrow = c(2,2))

plot(regfit.full.summary$rss,
     xlab="Number of variables", ylab="RSS", type="l")

plot(regfit.full.summary$adjr2,
     xlab="Number of variables",
     ylab="Adjusted R-squared", type="l")
max_adjr2_loc <- which.max(regfit.full.summary$adjr2)
points(max_adjr2_loc,
       regfit.full.summary$adjr2[max_adjr2_loc],
       col="red", cex=2)
# Best model: p=9
coef(regfit.full, 9)

plot(regfit.full.summary$cp,
     xlab="Number of variables",
     ylab="C_p", type="l")
min_cp_loc <- which.min(regfit.full.summary$cp)
points(min_cp_loc,
       regfit.full.summary$cp[min_cp_loc],
       col="red", cex=2)
# Best model: p=3
coef(regfit.full, 3)

plot(regfit.full.summary$bic,
     xlab="Number of variables",
     ylab="BIC", type="l")
min_bic_loc <- which.min(regfit.full.summary$bic)
points(min_bic_loc,
       regfit.full.summary$bic[min_bic_loc],
       col="red", cex=2)
# Best model: p=2
coef(regfit.full, 2)



# (d)
regfit.fwd <- regsubsets(Y~., data=data, nvmax=19,
                         method="forward")

regfit.bwd <- regsubsets(Y~., data=data, nvmax=19,
                         method="backward")

# (e)
library(glmnet)
lasso.mod <- glmnet(as.matrix(data[,-1]),
                   data[,1], alpha=1,
                   lambda = 10^seq(10, -2, length=100))
par(mfrow = c(1,1))
plot(lasso.mod)
cv.lasso <- cv.glmnet(as.matrix(data[,-1]), data[,1],
                      alpha=1)
bestlam <- cv.lasso$lambda.min
plot(cv.lasso)


# (f)