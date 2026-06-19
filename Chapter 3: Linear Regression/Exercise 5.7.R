rm(list = ls())
library(ISLR2)
attach(Weekly)



# (a)
model <- glm(as.numeric(Direction)-1 ~ Lag1 + Lag2,
             data = Weekly, family = binomial)

# (b)
model_b <- glm(as.numeric(Direction)-1 ~ Lag1 + Lag2,
               data = Weekly, subset = (-1),
               family = binomial)

# (c)
prob1 <- predict.glm(model_b, Weekly[1, 2:3],
                       type="response")
fitted1 <- ifelse(prob1 > 0.5, "Up", "Down")
fitted1 == Weekly$Direction[1]
# Observation 1 was not correctly classified

# (d)
n <- nrow(Weekly)
errors <- rep(0, n)
for (i in 1:n) {
  model_LOOCV <- glm(as.numeric(Direction)-1 ~ Lag1 + Lag2,
                     data = Weekly, subset = -i,
                     family = binomial)
  prob_i <- predict.glm(model_LOOCV, Weekly[i, 2:3],
                        type="response")
  fitted_i <- ifelse(prob_i > 0.5, "Up", "Down")
  errors[i] = (fitted_i != Weekly$Direction[1])
}

# (e)
LOOCV_test_error <- mean(errors)
