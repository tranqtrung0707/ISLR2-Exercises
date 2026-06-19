rm(list=ls())
library(ISLR2)
attach(Boston)
library(randomForest)
set.seed(1)



n <- nrow(Boston)
train <- sort(sample(1:n, n/2))
test <- (1:n)[-train]
p <- ncol(Boston) - 1

testError <- function(ntree, m) {
  rf.fit <- randomForest(medv ~., data=Boston,
                         subset=train, ntree = ntree,
                         mtry=m, importance=TRUE)
  rf.pred <- predict(rf.fit, newdata=Boston[test,])
  return (mean((rf.pred - Boston[test, "medv"])^2))
}

time.start <- Sys.time()

testErrors.1 <- sapply(1:500, function(i) {
  testError(i, p)
})

testErrors.2 <- sapply(1:500, function(i) {
  testError(i, p/2)
})

testErrors.3 <- sapply(1:500, function(i) {
  testError(i, sqrt(p))
})

time.end <- Sys.time()

testErrors.matrix <- cbind(testErrors.1, testErrors.2,
                           testErrors.3)
colnames(testErrors.matrix) <- c("m=p", "m=p/2",
                                 "m=sqrt(p)")
matplot(testErrors.matrix, type="l", lty=1,
        xlab="ntree", ylab="MSE",
        col = c("blue", "red", "green"))
legend("topright", legend=colnames(testErrors.matrix),
       col = c("blue", "red", "green"), lty=1)

# For each m, the MSE seems to settle around ntree=25.
# The ultimate MSE is smallest for m=sqrt(p), and
# largest for m=p (which is bagging). In this case,
# the smaller p is, the smaller the ultimate MSE is.