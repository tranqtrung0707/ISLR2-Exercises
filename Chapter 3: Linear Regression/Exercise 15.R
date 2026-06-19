rm(list=ls())
library(ISLR2)

attach(Boston)


#(a)
preds <- subset(Boston, select = -crim)
response <- Boston$crim
# running 12 univariate regressions
lm.fit_unis <- lapply(names(preds), function(x) {
  lm(response ~ preds[[x]])
})
# extracting beta1 hat
betas <- sapply(lm.fit_unis,
                function(m) coef(m)[2])
# extracting p-values
pvals <- sapply(lm.fit_unis,
                function(m) summary(m)$coefficients[2,4])
# produce a summary table
results <- data.frame(
  variable = names(preds),
  beta = betas,
  p_value = pvals
)
results
# Statistically significant models:
# zn, indus, nox, rm, age, dis, rad, tax, ptratio,
# lstat, medv

par(mfrow = c(3,4)) #3x4 layout
for (i in 1:12) {
  plot(preds[,i], response,
       xlab = names(preds)[i], ylab = "crim",
       main = paste("crim ~", names(preds)[i]))
  abline(lm(response ~ preds[,i]), col="red", lwd=2)
}

par(mfrow = c(1,1)) #Reset layout



#(b)
lm.fit_all <- lm(crim ~ ., data=Boston)
summary(lm.fit_all)
# Statistically significant predictors:
# zn, dis, rad, medv

#(c)
names(betas) <- names(preds)
URC <- betas
MRC <- lm.fit_all$coefficients[-1]
# plot(URC, MRC)
# identify(URC, MRC, labels = names(MRC))
library(plotly)
plot_ly(x = ~URC, y = ~MRC, type="scatter",
        mode = "markers", text = ~names(MRC),
        hoverinfo = "text")

#(d)
# running 12 cubic regressions
lm.fit_cubics <- lapply(names(preds), function(x) {
  lm(response ~ preds[[x]] + I(preds[[x]]^2)
    + I(preds[[x]]^3))
})
# extracting beta2 hat
beta2s <- sapply(lm.fit_cubics, function(m) coef(m)[3])
# extracting beta3 hat
beta3s <- sapply(lm.fit_cubics, function(m) coef(m)[4])
# extracting p-value of beta2 hat
pvals2 <- sapply(1:12, function(i) {
  tryCatch(
    summary(lm.fit_cubics[[i]])$coefficients[3,4],
    error = function(e) NA
  )
  })
# extracting p-value of beta3 hat
pvals3 <- sapply(1:12, function(i) {
  tryCatch(
    summary(lm.fit_cubics[[i]])$coefficients[4,4],
    error = function(e) NA
  )
})
# produce a summary table
results_cubic <- data.frame(
  variable = names(preds),
  beta2 = beta2s, beta3 = beta3s,
  p_value2 = pvals2, p_value3 = pvals3
)
results_cubic

# There is evidence of cubic relationship between crim and
# indus, nox, age, dis, ptratio, medv