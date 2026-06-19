rm(list=ls())
library(ISLR2)

attach(Carseats)

#(a)
lm.fit <- lm(Sales ~ Price + Urban + US)

#(b)
summary(lm.fit)
# Each 1 dollar increase in price company charges
# for car seats at each site is associated with 54 fewer
# seats sold.
# An urban store sells 22 fewer seats than a similar
# store in rural area (Note that this coefficient
#                      is not statistically significant)
# A store in the US sells 1200 more car seats than
# a similar store outside the US.

#(c)
# Sales = 13.04 - 0.05*Price - 0.02*UrbanYes
          # + 1.2*USYes

#(d)
# Price and USYes

#(e)
lm.fit2 <- lm(Sales ~ Price + US)

#(f)
anova(lm.fit, lm.fit2)

#(g)
confint(lm.fit2)
# Price: -0.065 -> -0.044
# USYes: 0.692 -> 1.708

#(h)
par(mfrow=c(2,2))
plot(lm.fit2)
# There is an observation with unusually
# high leverage but it does not have an outlier value