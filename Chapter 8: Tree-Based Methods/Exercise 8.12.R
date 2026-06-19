rm(list=ls())
library(ISLR2)

time.start <- Sys.time()



n <- nrow(Bikeshare)
train <- sort(sample(1:n, n/2))
Bikeshare.train <- subset(Bikeshare[train,],
                          select = -c(casual, registered))
Bikeshare.test <- subset(Bikeshare[-train,],
                         select = -c(casual, registered))



# Boosting
library(gbm)
boost.fit <- gbm(bikers ~., data = Bikeshare.train,
                 distribution = "gaussian")
boost.pred <- predict(boost.fit, Bikeshare.test)
boost.MSE <- mean((boost.pred - Bikeshare.test$bikers)^2)



# Bagging
library(randomForest)
bag.fit <- randomForest(bikers ~.,
                        data = Bikeshare.train, mtry=14)
bag.pred <- predict(bag.fit, Bikeshare.test)
bag.MSE <- mean((bag.pred - Bikeshare.test$bikers)^2)



# Random forest
rf.fit <- randomForest(bikers ~.,
                       data = Bikeshare.train, mtry=4)
rf.pred <- predict(rf.fit, Bikeshare.test)
rf.MSE <- mean((rf.pred - Bikeshare.test$bikers)^2)



# BART
library(BART)
bart.fit <- gbart(subset(Bikeshare.train,
                         select = -bikers),
                  Bikeshare.train$bikers,
                  subset(Bikeshare.test,
                         select = -bikers))
bart.pred <- bart.fit$yhat.test.mean
bart.MSE <- mean((bart.pred - Bikeshare.test$bikers)^2)



# Linear regression
lm.fit <- lm(bikers ~., data = Bikeshare.train)
Bikeshare.test2 <-
  Bikeshare.test[Bikeshare.test$weathersit
                 != "heavy rain/snow",]
lm.pred <- predict(lm.fit, Bikeshare.test2)
lm.MSE <- mean((lm.pred - Bikeshare.test2$bikers)^2)
summary(lm.fit)



# Summary
MSE <- c(boost.MSE, bag.MSE, rf.MSE, bart.MSE, lm.MSE)
names(MSE) <- c("Boosting", "Bagging",
                "Random forest", "BART",
                "Linear Regression")
MSE



# Bagging yields the best performance



time.end <- Sys.time()
time.elapsed <- time.end - time.start
print(paste("The whole exercise takes",
            60*time.elapsed,
            "seconds to run."))
