### R code from vignette source 'Law_School_Bootstrap_Intervals.rnw'
### Encoding: ISO8859-1

###################################################
### code chunk number 1: Law_School_Bootstrap_Intervals.rnw:30-34
###################################################
lawSchool <- read.table(file="Law_School.txt", header = TRUE)

n <- nrow(lawSchool)
n


###################################################
### code chunk number 2: Law_School_Bootstrap_Intervals.rnw:37-39
###################################################
r <- cor(lawSchool)[2,1]
r


###################################################
### code chunk number 3: Law_School_Bootstrap_Intervals.rnw:42-44
###################################################
se.r <- (1 - r*r) / sqrt(n - 3)
se.r


###################################################
### code chunk number 4: Law_School_Bootstrap_Intervals.rnw:50-53
###################################################
set.seed(123)
indexs = sample(1:n,replace=TRUE)
indexs


###################################################
### code chunk number 5: Law_School_Bootstrap_Intervals.rnw:56-57
###################################################
lawSchool[indexs, ]


###################################################
### code chunk number 6: Law_School_Bootstrap_Intervals.rnw:63-65
###################################################
r.boot <- cor(lawSchool[indexs,])[2,1]
r.boot


###################################################
### code chunk number 7: Law_School_Bootstrap_Intervals.rnw:68-70
###################################################
t.r.boot <- (r.boot - r) / ((1 - r.boot*r.boot) / sqrt(n - 3))
t.r.boot


###################################################
### code chunk number 8: Law_School_Bootstrap_Intervals.rnw:75-78
###################################################
se.fact <- sqrt(n - 3)
t.r.boot <- se.fact * (r.boot - r) / (1 - r.boot*r.boot)
t.r.boot


###################################################
### code chunk number 9: Law_School_Bootstrap_Intervals.rnw:81-90
###################################################
B <- 10000
se.fact <- sqrt(n - 3)  # calculado al principio, una sola vez
set.seed(123)
t.r.boots <- replicate(B,
{
  r.boot <- cor(lawSchool[sample(1:n, replace = TRUE),])[2,1]
  se.fact * (r.boot - r) / (1 - r.boot*r.boot)
}
)


###################################################
### code chunk number 10: Law_School_Bootstrap_Intervals.rnw:95-96
###################################################
t.r.boots[1:20]


###################################################
### code chunk number 11: Law_School_Bootstrap_Intervals.rnw:99-101
###################################################
alpha <- 0.05
confLevel <- 1 - alpha


###################################################
### code chunk number 12: Law_School_Bootstrap_Intervals.rnw:104-105
###################################################
quantile(t.r.boots, probs = c(alpha/2, 1 - alpha/2))


###################################################
### code chunk number 13: Law_School_Bootstrap_Intervals.rnw:108-112
###################################################
icBoot.t <- r - quantile(t.r.boots, probs = c(1 - alpha/2, alpha/2)) * se.r
names(icBoot.t) <- NULL
attr(icBoot.t, "conf.level") = confLevel
icBoot.t


###################################################
### code chunk number 14: Law_School_Bootstrap_Intervals.rnw:115-120
###################################################
t_alpha <- quantile(abs(t.r.boots), probs = 1 - alpha)
icBoot.t.sym <- r + c(-t_alpha, t_alpha) * se.r
names(icBoot.t.sym) <- NULL
attr(icBoot.t.sym, "conf.level") = confLevel
icBoot.t.sym


###################################################
### code chunk number 15: Law_School_Bootstrap_Intervals.rnw:127-129
###################################################
require(mvtnorm)
rmvnorm(n = 5, mean = c(1,2), sigma = matrix(c(10,3,3,2), ncol = 2))


###################################################
### code chunk number 16: Law_School_Bootstrap_Intervals.rnw:132-138
###################################################
medias = colMeans(lawSchool)
medias
var.covs = cov(lawSchool)
var.covs

rmvnorm(n = n, mean = medias, sigma = var.covs)


###################################################
### code chunk number 17: Law_School_Bootstrap_Intervals.rnw:141-150
###################################################
B <- 10000
se.fact <- sqrt(n - 3)  # computed at first, only once
set.seed(123)
t.r.boots <- replicate(B,
{
  r.boot <- cor(rmvnorm(n = n, mean = medias, sigma = var.covs))[2,1]
  se.fact * (r.boot - r) / (1 - r.boot*r.boot)
}
)


###################################################
### code chunk number 18: Law_School_Bootstrap_Intervals.rnw:153-154
###################################################
t.r.boots[1:20]


###################################################
### code chunk number 19: Law_School_Bootstrap_Intervals.rnw:158-161
###################################################
icBoot.t <- r - quantile(t.r.boots, probs = c(1 - alpha/2, alpha/2)) * se.r
names(icBoot.t) <- NULL
icBoot.t


###################################################
### code chunk number 20: Law_School_Bootstrap_Intervals.rnw:164-169
###################################################
t_alpha <- quantile(abs(t.r.boots), probs = 1 - alpha)
icBoot.t.sym <- r + c(-t_alpha, t_alpha) * se.r
names(icBoot.t.sym) <- NULL
attr(icBoot.t.sym, "conf.level") = confLevel
icBoot.t.sym


###################################################
### code chunk number 21: Law_School_Bootstrap_Intervals.rnw:172-173
###################################################
cor.test(lawSchool[,"LSAT"], lawSchool[,"GPA"])$conf.int


###################################################
### code chunk number 22: Law_School_Bootstrap_Intervals.rnw:179-209
###################################################
seJack.r <- function(xy) {
  n <- nrow(xy)
  r_i <- numeric(n)
  x <- xy[,1]
  y <- xy[,2]
  for (i in 1:n) {
    r_i[i] <- cor(x[-i], y[-i])
  }
  return(sqrt(((n - 1) / n) * sum((r_i - mean(r_i))^2)))
}

# Slightly fastest version:
seJ.r <- function(xy) {
  n <- nrow(xy)
  x <- xy[,1]
  y <- xy[,2]
  r_i <- vapply(1:n, function(i) cor(x[-i], y[-i]), FUN.VALUE = 0.0)
  return(sqrt(((n - 1) / n) * sum((r_i - mean(r_i))^2)))
}

require(microbenchmark)

microbenchmark(
  se.r <- seJack.r(lawSchool),
  se.r <- seJ.r(lawSchool)
)
# Nearly no difference...

se.r <- seJ.r(lawSchool)
se.r


###################################################
### code chunk number 23: Law_School_Bootstrap_Intervals.rnw:212-218
###################################################
t.r.boots <- replicate(B,
{
  lawSchool.boot <- lawSchool[sample(1:n, replace = TRUE),] 
  (cor(lawSchool.boot)[2,1] - r) / seJ.r(lawSchool.boot)
}
)


###################################################
### code chunk number 24: Law_School_Bootstrap_Intervals.rnw:221-225
###################################################
icBoot.t <- r - quantile(t.r.boots, probs = c(1 - alpha/2, alpha/2)) * se.r
names(icBoot.t) <- NULL
attr(icBoot.t, "conf.level") = confLevel
icBoot.t


###################################################
### code chunk number 25: Law_School_Bootstrap_Intervals.rnw:228-234
###################################################
t_alpha <- quantile(abs(t.r.boots), probs = 1 - alpha)
icBoot.t.sym <- r + c(-t_alpha, t_alpha) * se.r
names(icBoot.t.sym) <- NULL
attr(icBoot.t.sym, "conf.level") = confLevel
icBoot.t.sym



###################################################
### code chunk number 26: Law_School_Bootstrap_Intervals.rnw:239-242
###################################################
B <- 10000
alpha <- 0.05
confLevel = 1 - alpha


###################################################
### code chunk number 27: Law_School_Bootstrap_Intervals.rnw:245-246
###################################################
r.boot <- replicate(B, cor(lawSchool[sample(1:n, replace=TRUE),])[2,1])


###################################################
### code chunk number 28: Law_School_Bootstrap_Intervals.rnw:249-250
###################################################
r.boot[1:10]


###################################################
### code chunk number 29: Law_School_Bootstrap_Intervals.rnw:253-257
###################################################
icBoot.perc = quantile(r.boot, probs = c(alpha/2, 1 - alpha/2))
names(icBoot.perc) = NULL
attr(icBoot.perc, "conf.level") = confLevel
icBoot.perc


###################################################
### code chunk number 30: Law_School_Bootstrap_Intervals.rnw:262-263
###################################################
lawSchool[-4,]


###################################################
### code chunk number 31: Law_School_Bootstrap_Intervals.rnw:266-290
###################################################
r_i <- numeric(n)
for (i in 1:n) r_i[i] <- cor(lawSchool[-i,])[2,1] 

r_i <- mean(r_i) - r_i

a <- sum(r_i^3) / (6 * sum(r_i^2)^1.5)
a

z0 <- qnorm(sum(r.boot <= r) / B)
z0

zalpha <- - qnorm(alpha/2)
zalpha

icBoot.BCa = quantile(r.boot, 
  probs = c(
    pnorm(z0 + (z0 - zalpha) / (1 - a * (z0 - zalpha))), 
    pnorm(z0 + (z0 + zalpha) / (1 - a * (z0 + zalpha)))
  )
)

names(icBoot.BCa) = NULL
attr(icBoot.BCa, "conf.level") = confLevel
icBoot.BCa


