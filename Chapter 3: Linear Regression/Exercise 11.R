rm(list=ls())

set.seed(1)
x <- rnorm(100)
y <- 2*x + rnorm(100)

#(a)
lm.fit1 <- lm(y ~ x + 0)
summary(lm.fit1)
# beta hat is 1.9939
# SE of beta hat is 0.1065
# t-statistic of beta hat is 18.73
# p-value of the null hypothesis beta=0 is near 0
# Comment: This is basically the true underlying model

#(b)
lm.fit2 <- lm(x ~ y + 0)
summary(lm.fit2)
# beta hat is 0.39111
# SE of beta hat is 0.02089
# t-statistic of beta hat is 18.73
# p-value of the null hypothesis beta=0 is near 0
# Comment: The coefficient is not 1/2 (like it should be)
# because the regressor is correlated with the error term

#(c)
# The t-statistics and p-values are the same

#(d)
# See .docx file
n <- length(x)
Tstat <- sqrt(n-1) * sum(x*y) /
          sqrt(sum(x^2)*sum(y^2) - (sum(x*y))^2)

#(e)
# Switching x and y in the formula in (d) results in
# the same formula

#(f)
lm.fit3 <- lm(y ~ x)
lm.fit4 <- lm(x ~ y)
summary(lm.fit3)$coefficients
summary(lm.fit4)$coefficients
# Both t-stats are 18.56