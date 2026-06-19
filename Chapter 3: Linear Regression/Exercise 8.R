rm(list=ls())
library(ISLR2)
attach(Auto)

#(a)
lm.fit <- lm(mpg ~ horsepower)
summary(lm.fit)
# i. Yes, there is
# ii. Strong, p-value near 0
# iii. Negative
predict(lm.fit, data.frame(horsepower=98),
        interval="confidence")
predict(lm.fit, data.frame(horsepower=98),
        interval="prediction")
# iv. Confidence interval: 23.97 - 24.96
# prediction interval: 14.81 - 34.12

#(b)
plot(horsepower, mpg)
abline(lm.fit, col="red")

#(c)
par(mfrow = c(2,2))
plot(lm.fit)
# The residuals seem to follow a pattern