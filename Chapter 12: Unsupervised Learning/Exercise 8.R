rm(list=ls())



# (a)
pc.out <- prcomp(USArrests, scale. = TRUE)
PVE1 <- (pc.out$sdev^2) / sum((pc.out$sdev^2))


# (b)
PVE2 <- colSums(pc.out$x^2) /
  sum(colSums(scale(USArrests)^2))

PVE1
PVE2