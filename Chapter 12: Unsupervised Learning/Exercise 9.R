rm(list=ls())



#(a)
data <- USArrests
clust.out <- hclust(dist(data))



#(b)
clust.cut <- cutree(clust.out, k=3)
clust.cut



#(c)
data.scale <- scale(USArrests)
clust.out.scaled <- hclust(dist(data.scale))



#(d)
par(mfrow = c(1,2))
plot(clust.out)
plot(clust.out.scaled)
# Scaling changes the hierarchical tree significantly.
# In this case, because the variables are measured
# using different units and have different scales,
# the variables should be scaled so that variables
# with higher values do not dominate the computed
# dissimilarities