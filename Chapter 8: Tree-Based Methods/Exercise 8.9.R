rm(list=ls())
library(ILSR2)
attach(OJ)



# (a)
train <- sort(sample(1:nrow(OJ), 800))
OJ.train <- OJ[train,]
OJ.test <- OJ[-train,]



# (b)
tree.fit <- tree(Purchase ~., OJ.train)
summary(tree.fit)
# The tree is constructed using only 3 variables:
# LoyalCH, PriceDiff and ListPriceDiff.
# The training error rate is 16.62%.
# It has 6 terminal nodes



# (c)
tree.fit
# I pick the terminal node #4. First, LoyalCH < 0.482.
# Second, LoyalCH < 0.276. The predicted value for
# Purchase is 0.894, which is the predicted
# probability of buying Minute Maid instead of
# Citrus Hill.
# Note that LoyalCH measures a customer's loyalty to
# Citrus Hill.
# Thus, this tree predicts that the probability of
# a customer buying Minuted Maid is very high if 
# he has low loyalty to Citrus Hill.



# (d)
plot(tree.fit)
text(tree.fit, pretty=0)
# If LoyalCH < 0.48, the algorithm predicts Minute Maid.
# Else, if LoyalCH < 0.765 and PriceDiff(MM-CH) < 0.085
# and ListPriceDiff(MM-CH) < 0.235, the algorithm
# predicts Citrus Hill. Else, the algorithm predicts
# Minute Maid.



# (e)
tree.pred <- predict(tree.fit, OJ.test, type="class")
tree.confusion <- table(tree.pred, OJ.test$Purchase)
tree.testErrorRate <- (tree.confusion[1,2] +
                         tree.confusion[2,1]) / 
                        sum(tree.confusion)
# The test error rate is 16.67%



# (f)
tree.cv <- cv.tree(tree.fit, FUN = prune.misclass)
# The optimal tree size is 5



# (g)
plot(tree.cv$size, tree.cv$dev / nrow(OJ.train),
     xlab = "Tree size",
     ylab = "Cross-validated classification error rate")



# (h)
# 5



# (i)
tree.prune <- prune.misclass(tree.fit, best=5)



# (j)
summary(tree.prune)
# The training error rate for the pruned tree is 16.62%
# This is exactly the same as the unpruned tree.



# (k)
tree.prune.pred <- predict(tree.prune, OJ.test,
                           type="class")
tree.prune.confusion <- table(tree.prune.pred,
                              OJ.test$Purchase)
tree.prune.testErrorRate <-
  (tree.prune.confusion[1,2] +
     tree.prune.confusion[2,1]) /
  sum(tree.prune.confusion)
# The test error rate for the pruned tree is 16.67%.
# This is exactly the same as the unpruned tree.