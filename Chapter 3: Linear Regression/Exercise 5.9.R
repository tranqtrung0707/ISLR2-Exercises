rm(list = ls())
library(ISLR2)
attach(Boston)



# (a)
mu_hat <- mean(medv)
# mu_hat = 22.53

# (b)
n <- nrow(Boston)
SE_mu_hat <- sd(medv)/sqrt(n)
# The estimated standard error of mu_hat is 0.409

# (c)
library(boot)
boot.fn <- function(data, index) mean(data[index])
boot(medv, boot.fn, R=10000)
# The bootstrap standard error is 0.409, virtually
# the same as the classic estimate standard error.

# (d)
boot.confint <- mu_hat + SE_mu_hat*c(-2,2)
# the bootstrap confidence interval is very slightly
# wider than the t-test confidence interval

# (e)
mu_hat_med <- median(medv)
# mu_hat_med is 21.2

# (f)
boot.fn2 <- function(data, index) median(data[index])
boot(medv, boot.fn2, R=10000)
# The bootstrap standard error for mu_hat_med is 0.375,
# smaller than the bootstrap standard error for mu_hat

# (g)
mu_hat_0.1 <- quantile(medv, 0.1)
# mu_hat_0.1 is 12.8

# (h)
boot.fn3 <- function(data, index)
              quantile(data[index], 0.1)
boot(medv, boot.fn3, R=10000)
# The bootstrap standard error of mu_hat_0.1 is 0.503
# This is larger than the bootstrap standard errors
# for the mean and median.