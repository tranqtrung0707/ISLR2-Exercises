rm(list=ls())



# (a)
data <- read.csv("D:/MEGA/Actuary/SRM/ISLR/Ch12Ex13.csv",
                 header = F) |> t()



# (b)
dist <- as.dist(1 - cor(t(data)))

par(mfrow = c(2,2))

hc.complete <- hclust(dist, method = "complete")
plot(hc.complete)

hc.single <- hclust(dist, method = "single")
plot(hc.single)

hc.average <- hclust(dist, method = "average")
plot(hc.average)

hc.centroid <- hclust(dist, method = "centroid")
plot(hc.centroid)

hc.complete.cut <- cutree(hc.complete, k = 2)
hc.single.cut <- cutree(hc.single, k = 2)
hc.average.cut <- cutree(hc.average, k = 2)
hc.centroid.cut <- cutree(hc.centroid, k = 2)
plot(hc.complete.cut)
plot(hc.single.cut)
plot(hc.average.cut)
plot(hc.centroid.cut)

# Results depend very much on the type of linkage used.
# In this case, single and centroid linkage result in
# one of the two clusters being singleton.
# Complete linkage was able to identify 10 out of the
# first 20 observations as being in one cluster.
# Average linkage was able to identify 9 out of the
# first 20 observations as being in one cluster.

table(hc.complete.cut)
# Complete linkage result in two clusters with 10 and 30
# observations.



# (c)
data.1 <- data[1:20,]
data.2 <- data[21:40,]

dist.complete <- rep(0, 1000)
dist.single <- rep(0, 1000)
dist.average <- rep(0, 1000)
dist.centroid <- rep(0, 1000)

for (k in 1:1000) {
  dist_ij = rep(0, 20*20)
  for (i in 1:20) {
    for (j in 1:20) {
      dist_ij[20*(i-1) + j] =
        abs(data.1[i, k] - data.2[j, k])
    }
  }
  dist.complete[k] = max(dist_ij)
  dist.single[k] = min(dist_ij)
  dist.average[k] = mean(dist_ij)
  dist.centroid[k] = abs(mean(data.1[,k]) -
                           mean(data.2[,k]))
}

which.max(dist.complete) # =12
which.max(dist.single) # =535
which.max(dist.average) # =600
which.max(dist.centroid) # =600
