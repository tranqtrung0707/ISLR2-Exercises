rm(list=ls())



Euclidian <- as.matrix(dist(t(scale(t(USArrests))),
                            diag = T, upper = T))
correlation <- cor(t(USArrests))
plot(Euclidian^2, 1-correlation)
Euclidian^2 / (1-correlation)
