rm(list=ls())
par(mfrow = c(1,1))



# (a)
classes <- c(rep(1, 20), rep(2, 20), rep(3, 20))
data <- matrix(rnorm(60*50), nrow=60)
data[1:20,] <- data[1:20,] - 1
data[41:60,] <- data[41:60,] + 1



#(b)
pca <- prcomp(data)
plot(pca$x[,1], pca$x[,2], col = classes)



#(c)
km <- kmeans(data, centers = 3, nstart = 20)
table(classes, km$cluster)
# The clustering here is perfect.



#(d)
km <- kmeans(data, centers = 2, nstart = 20)
table(classes, km$cluster)
plot(pca$x[,1], pca$x[,2], col = km$cluster)
# Classes 1 and 3, which are the most different from
# each other, are assigned to different clusters.
# Class 2, which is in the middle, is split roughly
# evenly between the two clusters.



#(e)
km <- kmeans(data, centers = 4, nstart = 20)
table(classes, km$cluster)
plot(pca$x[,1], pca$x[,2], col = km$cluster)
# Classes 2 and 3 are perfectly clustered.
# Class 1 is split evenly between 2 different clusters.



#(f)
data.pc <- cbind(pca$x[,1], pca$x[,2])
km <- kmeans(data, centers = 3, nstart = 20)
table(classes, km$cluster)
plot(pca$x[,1], pca$x[,2], col = km$cluster)
# The clustering here is perfect.



#(g)
km <- kmeans(scale(data), centers = 3, nstart = 20)
table(classes, km$cluster)
# The clustering here is perfect.