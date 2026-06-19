rm(list=ls())
library(ISLR2)
attach(Auto)

#(a)
pairs(Auto)

#(b)
cor(Auto[,-9])

#(c)
lm.fit <- lm(mpg ~ . - name, data=Auto)
summary(lm.fit)
#i. There is
#ii. displacement, weight, year and origin
#iii. Holding other variables constant,
# a model one year newer has, on average,
# an mpg 0.75 higher

#(d)
par(mfrow=c(2,2))
plot(lm.fit)
# The residuals seems to follow a pattern:
# higher at low and high mpgs, lower at medium mpgs.
# Observations 323, 326 and 327 have unusually large
# residuals
# Observation 14 has unusually high leverage

#(e)
lm.fit2 <- lm(mpg ~ (. - name)^2, data=Auto)
summary(lm.fit2)
# The interaction terms that appear to be
# statistically significant are:
#   displacement:year, acceleration:year, and
# acceleration:origin

#(f)
lm.fit3 <- lm(mpg ~ I(log(cylinders))
              + I(log(displacement))
              + I(log(horsepower)) + I(log(weight))
              + I(log(acceleration)) + I(log(year))
              + origin)
summary(lm.fit3)
