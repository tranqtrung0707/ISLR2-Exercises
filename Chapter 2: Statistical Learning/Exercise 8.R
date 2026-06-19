rm(list=ls())

#(a)
college <- read.csv("D:/MEGA/Actuary/SRM/ISLR/College.csv")

#(b)
rownames(college) <- college[,1]
college <- college[,-1]

#(c)
attach(college)
#(i)
summary(college)
#(ii)
college2 <- college
college2$Private <- Private == "Yes" #Convert string to boolean
pairs(college2[,1:10])
#(iii)
plot(as.factor(Private), Outstate)
#(iv)
Elite <- rep("No", nrow(college))
Elite[college$Top10perc > 50] <- "Yes"
Elite <- as.factor(Elite)
college <- data.frame(college, Elite)
summary(Elite)
plot(Elite, Outstate)
#(v)
par(mfrow = c(2,2))
for (i in 2:5) {
    hist(college[,i])
}
#(vi)
