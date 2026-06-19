rm(list=ls())

#(a)
library(ISLR2)
#506 rows, 13 columns
#Boston is a data set containing housing values in 506 suburbs of Boston.

#(b)
pairs(Boston[,-13])

#(c)


#(d)
lapply(Boston, range)

#(e)
sum(Boston$chas)
#35 census tracts

#(f)
median(Boston$ptratio)
#19.05

#(g)
Boston[which.min(Boston$medv),]

#(h)
nrow(Boston[Boston$rm > 7,])
#64 census tracts average >7 rooms per dwelling
nrow(Boston[Boston$rm > 8,])
#13 census tracts average >7 rooms per dwelling
Boston[Boston$rm > 8,]
