rm(list = ls())
library(ISLR2)
attach(Default)


# (a)
model <- glm(default ~ income + balance, data = Default,
             family = binomial(link = "logit"))

# (b)
# i.
n <- nrow(Default)
training <- sample(n, n/2)
# ii.
model_trained <- glm(default ~ income + balance,
                     data = Default, subset = training,
                     family = binomial(link="logit"))
# iii.
prob_test <- predict(model_trained,
                     Default[-training,c(4,3)],
                     type = "response")
prediction_test <- prob_test > 0.5
# iv.
trueValue_test <- Default$default[-training]
VS_error <- mean(prediction_test !=
                   ifelse(trueValue_test == "Yes",
                          1, 0))

# (c)
# The error rate from 0.023 to 0.031

# (d)
# ii.
model_trained <- glm(default ~ income + balance + student,
                     data = Default, subset = training,
                     family = binomial(link="logit"))
# iii.
prob_test <- predict(model_trained,
                     Default[-training,c(4,3,2)],
                     type = "response")
prediction_test <- prob_test > 0.5
# iv.
trueValue_test <- Default$default[-training]
VS_error <- mean(prediction_test !=
                   ifelse(trueValue_test == "Yes",
                          1, 0))

# Including the dummy variable for student does not
# lead to a reduction in the test error rate.