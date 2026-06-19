rm.list=ls()

#(a)
# See .docx file

#(b)
# See exercise 11

#(c)
x <- rnorm(100)
z <- 2*x + rnorm(100) # z is unscaled y
y <- sqrt(sum(x^2)/sum(z^2)) * z #Scale so that sum(y^2) = sum(x^2)

lm.fit1 <- lm(y ~ x + 0)
lm.fit2 <- lm(x ~ y + 0)
lm.fit1
lm.fit2