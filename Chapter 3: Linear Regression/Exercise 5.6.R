rm(list=ls())
library(ISLR2)

attach(Default)



# (a)
model <- glm(default ~ income + balance, data = Default,
             family = binomial)
SE1 <- summary(model)$coefficients[,2]

# (b)
boot.fn <- function(data, index) {
  glm(default ~ income + balance, data = Default,
      subset = index, family = binomial) |> coef()
}

# (c)
library(boot)
boot(Default, boot.fn, R=100)

# (d)
# The estimated standard error for income using
# the bootstrap is smaller than that using the glm()
# function.
# The estimated standard error for balance using
# the bootstrap is smaller than that using the glm()
# function.

