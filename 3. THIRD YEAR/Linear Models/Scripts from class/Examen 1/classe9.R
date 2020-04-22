install.packages("faraway")
library(faraway)
data(gala, package = "faraway")
lmod <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, gala)

#nullmod <- lm(Species ~ 1, gala)
#anova(nullmod, lmod)
#confint(lmod)

install.packages("ellipse")
library(ellipse)
plot(ellipse(lmod, c(2, 6)), type = "l", ylim = c(-0.13, 0))
points(coef(lmod)[2], coef(lmod)[6], pch = 19)
abline(v = confint(lmod)[2, ], lty = 2) #rectes que corresponen al interval del beta1
abline(h = confint(lmod)[6, ], lty = 2) #rectes que corresponen al interval del beta5

sample(10, rep = TRUE)

set.seed(123)
nb <- 4000
coefmat <- matrix(NA, nb, 6)
resids <- residuals(lmod)
preds <- fitted(lmod)
for (i in 1:nb) {
  booty <- preds + sample(resids, rep = TRUE)
  bmod <- update(lmod, booty ~ .)
  coefmat[i, ] <- coef(bmod)
}
colnames(coefmat) <- c("Intercept", colnames(gala[, 3:7]))
coefmat <- data.frame(coefmat)
apply(coefmat, 2, function(x) quantile(x, c(0.025, 0.975)))


require(ggplot2)
ggplot(coefmat, aes(x = Area)) + geom_density() + geom_vline(xint = c(-0.0628, 
                                                                      0.0185), lty = 2)

Sys.time()


