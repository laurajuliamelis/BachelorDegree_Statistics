### R code from vignette source 'Dinosaur_extinction_2.Rnw'
### Encoding: ISO8859-1

###################################################
### code chunk number 1: Dinosaur_extinction_2.Rnw:33-46
###################################################
family <- factor(c("Ceratops","Hadro","Pachycephalo","Tyranno","Ornithomo",
                   "Sauronith"),
                 levels=c("Ceratops","Hadro","Pachycephalo","Tyranno",
                          "Ornithomo","Sauronith"))

dinos <- data.frame(
  upper = c(50, 29, 3, 3, 4, 1),
  middle= c(53, 51, 2, 3, 8, 6),
  lower = c(19,  7, 1, 2, 1, 3)
)

rownames(dinos) <- family
dinos


###################################################
### code chunk number 2: Dinosaur_extinction_2.Rnw:71-77
###################################################
# 
shannon <- function(freq.abs)
{
	freq.rels = freq.abs / sum(freq.abs)
	- sum(ifelse(freq.rels > 0, freq.rels * log2(freq.rels), 0))
}


###################################################
### code chunk number 3: Dinosaur_extinction_2.Rnw:81-88
###################################################

# Diversity over a given period:
shannon(dinos[,"middle"])
shannon(dinos[,2])
# Diversity for all periods:
apply(dinos, 2, shannon)
vapply(dinos, shannon, FUN.VALUE = 0.0)


###################################################
### code chunk number 4: Dinosaur_extinction_2.Rnw:104-124
###################################################
shannon <- function(freq.abs, compute.se = TRUE)
{
  n <- sum(freq.abs)
  freq.rels = freq.abs / n
	log2freq.rels <- ifelse(freq.rels > 0, log2(freq.rels), 0)
	result <- - sum(freq.rels * log2freq.rels)
	if (compute.se) {
    attr(result, "se") <- sqrt((sum(freq.rels * log2freq.rels^2) - result^2) / n)
	}
	return(result)
}

biodivs <- lapply(dinos, shannon)
biodivs
unlist(biodivs)
sapply(biodivs, attr, "se")

biodivs <- sapply(dinos, shannon, compute.se = FALSE)
biodivs
sapply(biodivs, attr, "se")


###################################################
### code chunk number 5: Dinosaur_extinction_2.Rnw:141-156
###################################################
freq.abs <- dinos[,"upper"]
freq.abs
shannon.sample <- shannon(freq.abs)
shannon.sample
n <- sum(freq.abs)
n
freq.rels <- freq.abs / n
freq.rels

# One bootstrap multinomial resample:

set.seed(12345)
freq.boot <- rmultinom(1, size = n, prob = freq.rels)
freq.boot
# Caution: it is a 'k x one-column matrix' with k = length(freq.rels)


###################################################
### code chunk number 6: Dinosaur_extinction_2.Rnw:164-191
###################################################
# studentized statistic over the preceding bootstrap sample:
shannon.boot <- shannon(freq.boot)
(shannon.boot - shannon.sample) / attr(shannon.boot, "se")

# Function computing the studentized statistic:
tShannon <- function(freq.abs, shannon.value) {
  shannon.sampl <- shannon(freq.abs)
  (shannon.sampl - shannon.value) / attr(shannon.sampl, "se")
}

tShannon(freq.boot[,1], shannon.sample)

nboot <- 10
# 'nboot' bootstrap resamples:
set.seed(12345)
rmultinom(nboot, size = n, prob = freq.rels)

# Studentized statistic over each resample:
set.seed(12345)
apply(rmultinom(nboot, size = n, prob = freq.rels), 2, tShannon, shannon.value = shannon.sample)

alpha <- 0.05


nboot <- 10000
set.seed(12345)
tBoots <- apply(rmultinom(nboot, size = n, prob = freq.rels), 2, tShannon, shannon.value = shannon.sample)


###################################################
### code chunk number 7: Dinosaur_extinction_2.Rnw:196-204
###################################################
# Bootstrap-t (equals tails) confidence interval:
halfAlpha <- alpha / 2
tLimits <- quantile(tBoots, probs = c(1 - halfAlpha, halfAlpha))
tLimits
ci <- shannon.sample - tLimits * attr(shannon.sample, "se")
names(ci) = NULL
attr(ci, "conf.level") = 1 - alpha
ci


###################################################
### code chunk number 8: Dinosaur_extinction_2.Rnw:207-213
###################################################
# Symmetrized bootstrap-t confidence interval:
tLimit <- quantile(abs(tBoots), probs = 1 - alpha)
ci <- shannon.sample - c(tLimit, -tLimit) * attr(shannon.sample, "se")
names(ci) = NULL
attr(ci, "conf.level") = 1 - alpha
ci


###################################################
### code chunk number 9: Dinosaur_extinction_2.Rnw:217-244
###################################################
# All encapsulated in a function:
shannonBoot.t <- function(freq.abs, alpha = 0.05, symmetrized = FALSE, nboot = 10000)
{
  shannon.sample <- shannon(freq.abs)
  n <- sum(freq.abs)
  tBoots <- apply(
              rmultinom(nboot, size = n, prob = freq.abs / n), 2, tShannon,
              shannon.value = shannon.sample
  )
  if (symmetrized) {
    tLimit <- quantile(abs(tBoots), probs = 1 - alpha)
    ci <- shannon.sample - c(tLimit, -tLimit) * attr(shannon.sample, "se")
  } else {
    halfAlpha <- alpha / 2
    tLimits <- quantile(tBoots, probs = c(1 - halfAlpha, halfAlpha))
    ci <- shannon.sample - tLimits * attr(shannon.sample, "se")
  }
  names(ci) = NULL
  attr(ci, "conf.level") = 1 - alpha
  return(ci)
}

shannonBoot.t(dinos[,"upper"])
sapply(dinos, shannonBoot.t)

shannonBoot.t(dinos[,"upper"], symmetrized = TRUE)
sapply(dinos, shannonBoot.t, symmetrized = TRUE)


###################################################
### code chunk number 10: Dinosaur_extinction_2.Rnw:248-261
###################################################

# PERCENTILE BOOTSTRAP INTERVAL:
# This is the simplest to compute bootstrap confidence interval, it only requires
# the generation of a sample of bootstrap parameter estimations, say, here Shannon's
# biodiversities

nboot = 10000 
set.seed(12345)
shann.boot = replicate(nboot, shannon(rmultinom(1, size = n, prob = freq.rels), compute.se = FALSE))
ci = quantile(shann.boot, probs = c(alpha/2, 1 - alpha/2))
names(ci) = NULL
attr(ci, "conf.level") = 1 - alpha
ci


###################################################
### code chunk number 11: Dinosaur_extinction_2.Rnw:265-281
###################################################
# Percentile CI as a function:
shannonBoot.perc <- function(freq.abs, alpha = 0.05, nboot = 10000)
{
  n <- sum(freq.abs)
  shannBoots <- apply(rmultinom(nboot, size = n, prob = freq.abs / n), 2, shannon, compute.se = FALSE)
  ci = quantile(shannBoots, probs = c(alpha/2, 1- alpha/2))
  names(ci) = NULL
  attr(ci, "conf.level") = 1 - alpha
  return(ci)
}

# Some examples of its use:
p = c(0.6, 0.2, 0.1, 0.05, 0.025, 0.025)
n = 100
shann.real = shannon(p, compute.se = FALSE)
shann.real


###################################################
### code chunk number 12: Dinosaur_extinction_2.Rnw:285-306
###################################################
# A very naive simulation (that would need a lot of improvement)
# comparing the true coverage of bootstrap-t and percentile bootstrap intervals
# for a given configuration of hypothetic multinomial parameters:
p = c(0.6, 0.2, 0.1, 0.05, 0.025, 0.025)
n = 100

set.seed(12345)
nsim = 100
res = replicate(nsim, 
  {
    mostra = rmultinom(1, size = n, prob = p)
    ic.perc = shannonBoot.perc(mostra, nboot = 1000)
    recob.perc = (ic.perc[1] <= shann.real) & (shann.real <= ic.perc[2])
    ic.tSym = shannonBoot.t(mostra, symmetrized = TRUE, nboot = 1000)
    recob.tSym = (ic.tSym[1] <= shann.real) & (shann.real <= ic.tSym[2])
    c(recob.perc, recob.tSym)
  }
)

rownames(res) = c("percentile", "symmetrized bootstrap-t")
rowMeans(res)


###################################################
### code chunk number 13: Dinosaur_extinction_2.Rnw:313-355
###################################################
# BCa INTERVAL:

# BCa bootstrap interval for the Shannon biodiversity:

# Obtaining the Shannon's index jaccknife values
# E.g. for the "upper" period:
freq.abs <- dinos[,"upper"]
freq.abs

# Family "Ceratops" has a frequency of 50: there are 50 opportunities to remove
# one data of this family, but all 'shannon' values will be equal, they will be
# based on the frequencies: 50-1 29  3  3  4  1. Similarly, there will be
# 29 repeated values from the frequencies 50 29-1  3  3  4  1, etc

k <- length(freq.abs)
matrix(freq.abs, ncol = k, nrow = k)
diag(ncol = k, nrow = k)
matrix(freq.abs, ncol = k, nrow = k) - diag(ncol = k, nrow = k)
shannon.jack = apply(matrix(freq.abs, ncol = k, nrow = k) - diag(ncol = k, nrow = k), 
  2, shannon, compute.se = FALSE)
shannon.jack

# Mean of the jackknife values:
shannon.jack. = weighted.mean(shannon.jack, w = freq.abs)

a <- sum((shannon.jack. - shannon.jack)^3 * freq.abs) / 
  (6 * sum((shannon.jack. - shannon.jack)^2 * freq.abs)^1.5)
a

shannon.sample <- shannon(freq.abs, compute.se = FALSE)
z0 <- qnorm(sum(shann.boot <= shannon.sample) / nboot)
z0

zAlpha <- - qnorm(alpha/2)
zAlpha

p1 <- pnorm(z0 + (z0 - zAlpha) / (1 - a * (z0 - zAlpha)))
p2 <- pnorm(z0 + (z0 + zAlpha) / (1 - a * (z0 + zAlpha)))
ci = quantile(shann.boot, probs = c(p1, p2))
names(ci) = NULL
attr(ci, "conf.level") = 1 - alpha
ci


###################################################
### code chunk number 14: Dinosaur_extinction_2.Rnw:360-398
###################################################
# The above calculations may be encapsulated in a function:

# Ad hoc utility function returning the centered jackknife values:
shannon.BCa.jackVals <- function(freq.abs) {
  k <- length(freq.abs)
  jack.vals <- apply(
    matrix(freq.abs, ncol = k, nrow = k) - diag(ncol = k, nrow = k), 
    2, shannon, compute.se = FALSE
  )
  return(weighted.mean(jack.vals, w = freq.abs) - jack.vals)
}

# BCa confidence interval:
shannonBoot.BCa <- function(freq.abs, alpha = 0.05, nboot = 10000) {
  shannon.sample <- shannon(freq.abs, compute.se = FALSE)
  n <- sum(freq.abs)
  freq.rels = freq.abs / n
  shannonBoots <- apply(
    rmultinom(nboot, size = n, prob = freq.rels), 2, shannon,
    compute.se = FALSE
  )
  shannon.jack <- shannon.BCa.jackVals(freq.abs)
  shannon.jack2 <- shannon.jack * shannon.jack * freq.abs
  a <- sum(shannon.jack * shannon.jack2) / (6 * sum(shannon.jack2)^1.5)
  z0 <- qnorm(sum(shannonBoots <= shannon.sample) / nboot)
  zAlpha <- - qnorm(alpha/2)
  p1 <- pnorm(z0 + (z0 - zAlpha) / (1 - a * (z0 - zAlpha)))
  p2 <- pnorm(z0 + (z0 + zAlpha) / (1 - a * (z0 + zAlpha)))
  ci = quantile(shannonBoots, probs = c(p1, p2))
  names(ci) = NULL
  attr(ci, "conf.level") = 1 - alpha
  attr(ci, "a") = a
  attr(ci, "z0") = z0
  return(ci)
}

set.seed(12345)
shannonBoot.BCa(freq.abs)


###################################################
### code chunk number 15: Dinosaur_extinction_2.Rnw:403-425
###################################################
# A very naive simulation (that would need a lot of improvement)
# comparing the true coverage of the percentile, BCa and symmetrized bootstrap-t
# intervals for a given configuration of hypothetic multinomial parameters:

nsim = 1000
res = replicate(nsim, 
{
  mostra = rmultinom(1, size = n, prob = p)
  ic.perc = shannonBoot.perc(mostra, nboot = 1000)
  recob.perc = (ic.perc[1] <= shann.real) & (shann.real <= ic.perc[2])
  ic.BCa = shannonBoot.BCa(mostra, nboot = 1000)
  recob.BCa = (ic.BCa[1] <= shann.real) & (shann.real <= ic.BCa[2])
  ic.t = shannonBoot.t(mostra, symmetrized = FALSE, nboot = 1000)
  recob.t = (ic.t[1] <= shann.real) & (shann.real <= ic.t[2])
  ic.tSym = shannonBoot.t(mostra, symmetrized = TRUE, nboot = 1000)
  recob.tSym = (ic.tSym[1] <= shann.real) & (shann.real <= ic.tSym[2])
  c(recob.perc, recob.BCa, recob.t, recob.tSym)
}
)

rownames(res) = c("percentile", "percentile BCa", "bootstrap-t", "symmetrized bootstrap-t")
rowMeans(res)


###################################################
### code chunk number 16: Dinosaur_extinction_2.Rnw:433-444
###################################################
dShannon <- function(freq.abs1, freq.abs2, compute.se = FALSE) {
  shan1 <- shannon(freq.abs1, compute.se)
  shan2 <- shannon(freq.abs2, compute.se)
  diff <- shan1 - shan2
  if (compute.se)
    attr(diff, "se") <- sqrt(attr(shan1,"se")^2 + attr(shan2,"se")^2)
  return(diff)
}

dShannon(dinos[,"upper"], dinos[,"middle"])
dShannon(dinos[,"upper"], dinos[,"middle"], compute.se = TRUE)


###################################################
### code chunk number 17: Dinosaur_extinction_2.Rnw:449-493
###################################################
freq.abs1 = dinos[,"upper"]
freq.abs2 = dinos[,"middle"]

n1 <- sum(freq.abs1)
freq.rels1 <- freq.abs1 / n1
n2 <- sum(freq.abs2)
freq.rels2 <- freq.abs2 / n2

# Percentile interval:
nboot = 10000
alpha = 0.05

set.seed(12345)
diffBoots <- replicate(nboot,
  dShannon(
    rmultinom(1, size = n1, prob = freq.rels1),
    rmultinom(1, size = n2, prob = freq.rels2)
  )
)

ci = quantile(diffBoots, probs = c(alpha/2, 1 - alpha/2))
names(ci) = NULL
attr(ci, "conf.level") = 1 - alpha
ci

dShannonPerc = function(freq.abs1, freq.abs2, alpha = 0.05, nboot = 10000) {
  n1 <- sum(freq.abs1)
  freq.rels1 <- freq.abs1 / n1
  n2 <- sum(freq.abs2)
  freq.rels2 <- freq.abs2 / n2
  diffBoots <- replicate(nboot,
    dShannon(
      rmultinom(1, size = n1, prob = freq.rels1),
      rmultinom(1, size = n2, prob = freq.rels2)
    )
  )
  ci = quantile(diffBoots, probs = c(alpha/2, 1 - alpha/2))
  names(ci) = NULL
  attr(ci, "conf.level") = 1 - alpha
  return(ci)
}

set.seed(12345)
dShannonPerc(dinos[,"upper"], dinos[,"middle"])


###################################################
### code chunk number 18: Dinosaur_extinction_2.Rnw:497-572
###################################################
# Symmetrized interval:

tdShannon <- function(freq.abs1, freq.abs2, diffParam) {
  diff <- dShannon(freq.abs1, freq.abs2, compute.se = TRUE)
  return((diff - diffParam) / attr(diff,"se"))
}

diff.sample <- dShannon(freq.abs1, freq.abs2, compute.se = TRUE)

set.seed(12345)
tBoots <- replicate(nboot,
  tdShannon(
    rmultinom(1, size = n1, prob = freq.rels1),
    rmultinom(1, size = n2, prob = freq.rels2),
    diffParam = diff.sample
  )
)

tLimit <- quantile(abs(tBoots), probs = 1 - alpha)
ci <- diff.sample - c(tLimit, -tLimit) * attr(diff.sample, "se")
names(ci) = NULL
attr(ci, "conf.level") = 1 - alpha
ci

# Bootstrap-t confidence interval for the difference as a function:
dShannonBoot.t <- function(freq.abs1, freq.abs2,
  alpha = 0.05, symmetrized = FALSE, nboot = 10000)
{
  diff.sample <- dShannon(freq.abs1, freq.abs2, compute.se = TRUE)
  n1 <- sum(freq.abs1)
  freq.rels1 <- freq.abs1 / n1
  n2 <- sum(freq.abs2)
  freq.rels2 <- freq.abs2 / n2
  tBoots <- replicate(nboot,
    tdShannon(
      rmultinom(1, size = n1, prob = freq.rels1),
      rmultinom(1, size = n2, prob = freq.rels2),
      diffParam = diff.sample
    )
  )
  if (symmetrized) {
    tLimit <- quantile(abs(tBoots), probs = 1 - alpha)
    ci <- diff.sample - c(tLimit, -tLimit) * attr(diff.sample, "se")
  } else {
    halfAlpha <- alpha / 2
    tLimits <- quantile(tBoots, probs = c(1 - halfAlpha, halfAlpha))
    ci <- diff.sample - tLimits * attr(diff.sample, "se")
  }
  names(ci) = NULL
  attr(ci, "conf.level") = 1 - alpha
  return(ci)
}

set.seed(12345)
dShannonBoot.t(dinos[,"upper"], dinos[,"middle"])

for (i in 1:(ncol(dinos)-1))
  for (j in (i+1):ncol(dinos)) {
    cat(names(dinos)[i], " - ", names(dinos)[j], "   \n")
    print(dShannonBoot.t(dinos[,i], dinos[,j]))
  }

dShannonBoot.t(dinos[,"upper"], dinos[,"middle"], symmetrized = TRUE)

for (i in 1:(ncol(dinos)-1))
  for (j in (i+1):ncol(dinos)) {
    cat(names(dinos)[i], " - ", names(dinos)[j], "   \n")
    print(dShannonBoot.t(dinos[,i], dinos[,j], symmetrized = TRUE))
  }

for (i in 1:(ncol(dinos)-1))
  for (j in (i+1):ncol(dinos)) {
    cat(names(dinos)[i], " - ", names(dinos)[j], "   \n")
    print(dShannonBoot.t(dinos[,i], dinos[,j], symmetrized = TRUE, alpha = 0.1))
  }


###################################################
### code chunk number 19: Dinosaur_extinction_2.Rnw:576-584
###################################################
# Testing, p-vaule...
t.obs = tdShannon(freq.abs1, freq.abs2, diffParam = 0)
sum(abs(tBoots) >= abs(t.obs)) / nboot
# Or:
(sum(abs(tBoots) >= abs(t.obs)) + 1) / (nboot + 1)

# These intervals and p-values should need an adjustement for multiplicity, 
# but this is another history...


