rm(list=ls())

set.seed(1)

#(a)
x <- rnorm(100)

#(b)
eps <- rnorm(100, sd=sqrt(0.25))

#(c)
y <- -1 + 0.5*x + eps
length(y)
# length(y)=100. beta_0 = -1. beta_1 = 0.5

#(d)
par(mfrow=c(1,1))
plot(x, y)
# The plot indicates a linear relationship,
# though the variation is high

#(e)
lm.fit <- lm(y ~ x)
# beta0 hat = -1.0188 and beta1 hat = 0.4995
# This is very close to the real coefficients

#(f)
abline(lm.fit, col="red")
abline(a=-1, b=0.5, col="blue")
legend("topleft", legend = c("Sample", "Population"),
       col = c("red", "blue"), lwd = 2)

#(g)
lm.fit2 <- lm(y ~ x + I(x^2))
summary(lm.fit2)
# There is no evidence the quadratic term improves
# the model fit, as the p-value of x^2 is 0.164.



#(h)
set.seed(1)

#(a)
x <- rnorm(100)

#(b)
eps <- rnorm(100, sd=sqrt(0.04))

#(c)
y <- -1 + 0.5*x + eps
length(y)
# length(y)=100. beta_0 = -1. beta_1 = 0.5

#(d)
par(mfrow=c(1,1))
plot(x, y)
# The plot indicates a linear relationship,
#  the variation is small

#(e)
lm.fit_h <- lm(y ~ x)
# beta0 hat = -1.0075 and beta1 hat = 0.4998
# This is even closer to the real coefficients

#(f)
abline(lm.fit_h, col="red")
abline(a=-1, b=0.5, col="blue")
legend("topleft", legend = c("Sample", "Population"),
       col = c("red", "blue"), lwd = 2)

#(g)
lm.fit2_h <- lm(y ~ x + I(x^2))
summary(lm.fit2_h)
# There is no evidence the quadratic term improves
# the model fit, as the p-value of x^2 is 0.164.



#(i)
set.seed(1)

#(a)
x <- rnorm(100)

#(b)
eps <- rnorm(100, sd=sqrt(0.64))

#(c)
y <- -1 + 0.5*x + eps
length(y)
# length(y)=100. beta_0 = -1. beta_1 = 0.5

#(d)
par(mfrow=c(1,1))
plot(x, y)
# The plot indicates a linear relationship,
#  the variation is higher than the initial model

#(e)
lm.fit_i <- lm(y ~ x)
# beta0 hat = -1.0302 and beta1 hat = 0.4992
# This is close to the real coefficients, 
# but not as close as the initial model.

#(f)
abline(lm.fit_i, col="red")
abline(a=-1, b=0.5, col="blue")
legend("topleft", legend = c("Sample", "Population"),
       col = c("red", "blue"), lwd = 2)

#(g)
lm.fit2_i <- lm(y ~ x + I(x^2))
summary(lm.fit2_i)
# There is no evidence the quadratic term improves
# the model fit, as the p-value of x^2 is 0.164.



#(j)
confint(lm.fit)
confint(lm.fit_h)
confint(lm.fit_i)
# The confidence intervals based on the less noisy
# data set is narrower than the ones based on the
# original dataset.
# On the other hand, the confidence intervals based on
# the noisier data set is wider than the ones
# based on the original dataset.