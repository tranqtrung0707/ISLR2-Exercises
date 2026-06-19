rm(list=ls())

Auto <- read.csv("D:/MEGA/Actuary/SRM/ISLR/Auto.csv",
                 na.strings = "?", stringsAsFactors = T)
Auto <- na.omit(Auto)

#(a)
head(Auto)
# Only origin and name are qualitative, the rest are quantitative

#(b)
lapply(Auto[,1:7], range)

#(c)
lapply(Auto[,1:7], mean)
lapply(Auto[,1:7], sd)

#(d)
Auto2 <- Auto[-(10:85),]
lapply(Auto2[,1:7], range)
lapply(Auto2[,1:7], mean)
lapply(Auto2[,1:7], sd)

#(e)
pairs(Auto[,1:7])

#(f)