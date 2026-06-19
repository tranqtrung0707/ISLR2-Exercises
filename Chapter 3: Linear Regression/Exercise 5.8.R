rm(list=ls())



# (a)
set.seed(3)
x <- rnorm(100)
y <- x - 2*x^2 + rnorm(100)
# In this data set, n is 100 and p is 2. The model is:
# Y = X - 2*X^2 + e

# (b)
plot(x, y)
# y is a quadratic function of x, as expected

# (c)
data <- data.frame(y, x)
loocv_errors <- rep(0, 4)
models <- list()
for (i in 1:4) {
  models[[i]] <- glm(y ~ poly(x, i, raw=T),
                     data = data)
  loocv_errors[i] <- cv.glm(data, models[[i]])$delta[1]
}
loocv_errors

# (d)
# Using a different random seed, the results are
# materially different. The reason is randomness.
# Also, LOOCV is known to have high variance.

# (e)
# The 2nd model has the smallest LOOCV error. This is
# what I expected, because the true relationship is
# quadratic, so the model with the smallest test error
# should be quadratic.

# (f)
sapply(1:4,
       function(i) summary(models[[i]])$coefficients)
# In the linear model, the intercept estimate is very
# inaccuarate and significant.
# In the quadratic model, the intercept is close to 0
# and not statistically significant. The other 2
# parameters are close to their true values and
# statistically significant.
# In the cubic model, the parameter of x^3 is not
# statistically significant. Ditto for the parameters
# of x^3 and x^4 in the quartic model.