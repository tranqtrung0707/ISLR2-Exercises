rm(list=ls())

#(a)
set.seed(1)
x1 <- runif(100)
x2 <- 0.5*x1 + rnorm(100)/10
y <- 2 + 2*x1 + 0.3*x2 + rnorm(100)
# The form of the linear model is:
# y = 2 + 2*x1 + 0.3*x2 + e

#(b)
cor(x1, x2)
plot(x1, x2)
# The correlation between x1 and x2 is 0.835

#(c)
lm.fit <- lm(y ~ x1 + x2)
summary(lm.fit)
# beta0 hat = 2.13, beta1 hat = 1.44, beta2 hat = 1.01
# beta0 hat is fairly close to beta0,
# but beta1 hat and beta2 hat is not close to
# beta1 and beta2, respectively
# We can reject the null hypothesis beta1=0
# with p-value 0.049
# We cannot reject the null hypothesis beta2=0

#(d)
lm.fit2 <- lm(y ~ x1)
summary(lm.fit2)
# beta1 hat = 1.98 is now close to beta1
# We can reject the null hypothesis beta1=0

#(e)
lm.fit3 <- lm(y ~ x2)
summary(lm.fit3)
# beta1 hat = 2.90 is now further away to beta1
# We can reject the null hypothesis beta1=0

#(f)
# No. Because x1 and x2 are highly correlated,
# fitting either one of them in the model leads to
# statistically significant result. But fitting both
# leads to a scenario where neither coefficients are
# statistically significant.



#(g)
x1 <- c(x1, 0.1)
x2 <- c(x2, 0.8)
y <- c(y, 6)

lm.fit_g <- lm(y ~ x1 + x2)
summary(lm.fit_g)
# Adding just one observation makes x2 statistically
# significant, while making x1 no longer
# statistically significant.

lm.fit2_g <- lm(y ~ x1)
summary(lm.fit2_g)
# beta1 hat is now much smaller with just one
# extra observation

lm.fit3_g <- lm(y ~ x2)
summary(lm.fit3_g)
# beta1 hat is now much larger with just one
# extra observation
