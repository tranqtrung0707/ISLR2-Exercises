rm(list=ls())
library(ISLR2)
set.seed(2)
library(gbm)
library(class)



# (a)
Caravan$Purchase <- as.integer(Caravan$Purchase == "Yes")
Caravan.train <- Caravan[1:1000,]
Caravan.test <- Caravan[-(1:1000),]



# (b)
boost.fit <- gbm(Purchase ~., data = Caravan.train,
                 distribution = "bernoulli",
                 n.trees = 1000, shrinkage = 0.01)
summary(boost.fit)
# PPERSAUT and MKOOPKLA appear to be the most important



# (c)
boost.prob <- predict(boost.fit, Caravan.test,
                      type = "response")
boost.pred <- ifelse(boost.prob > 0.2, "Yes", "No")
boost.confusion <- table(boost.pred,
                         Caravan.test$Purchase)
# There were 156 people predicted to make a purchase.
# 33 of them did in fact make one, for a percentage
# of 21.15%

x.train <- subset(Caravan.train, select = -Purchase)
x.test <- subset(Caravan.test, select = -Purchase)
knn.pred <- knn(x.train, x.test,
                Caravan.train$Purchase, k=5)
knn.confusion <- table(knn.pred,
                       Caravan.test$Purchase)
# There were 34 people predicted to make a purchase.
# 4 of them did in fact make one, for a percentage
# of 11.76%.

glm.fit <- glm(Purchase ~., data = Caravan.train,
               family = binomial)
glm.prob <- predict(glm.fit, Caravan.test,
                    type = "response")
glm.pred <- ifelse(glm.prob > 0.2, "Yes", "No")
glm.confusion <- table(glm.pred,
                       Caravan.test$Purchase)
glm.confusion
# There were 408 people predicted to make a purchase.
# 58 of them did in fact make one, for a percentage
# of 14.22%.