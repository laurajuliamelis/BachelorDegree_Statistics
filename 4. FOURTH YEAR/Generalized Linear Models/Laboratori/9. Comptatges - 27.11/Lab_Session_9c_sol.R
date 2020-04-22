###################
## DATA          ##
###################

# ofp:  number of physician office visits (response) NUMERO DE VISITES AL METGE

# hosp: number of hospital stays
# health: self-perceived health status 
# numchron: number of chronic conditions 
# gender: male/female 
# school: number of years of education 
# privins: private insurance indicator

# DADES DESAGREGADES!!!

###################
## PRELIMINARIES ##
###################

## Packages
library("sandwich")
library("lmtest")
library("MASS")
library("car")
library("pscl")
library("AER")


## data
setwd('...')   
load("DebTrivedi.rda")
dt <- DebTrivedi[, c(1, 6:8, 13, 15, 18)]   # select variables


###################
## VISUALIZATION ##
###################

## Convenience functions for visualization
clog <- function(x) log(x + 0.5)
cfac <- function(x, breaks = NULL) {
  if(is.null(breaks)) breaks <- unique(quantile(x, 0:10/10))
  x <- cut(x, breaks, include.lowest = TRUE, right = FALSE)
  levels(x) <- paste(breaks[-length(breaks)], ifelse(diff(breaks) > 1,
    c(paste("-", breaks[-c(1, length(breaks))] - 1, sep = ""), "+"), ""), sep = "")
  return(x)
}

## visualization: dependent variables
par(las=1)
plot(table(dt$ofp), xlab = "Number of physician office visits", ylab = "Frequency", xaxt='n')
axis(1, at = 0:9 * 10)

## visualization transformations
par(mfrow = c(1, 2))
plot(ofp ~ numchron, data = dt)
plot(clog(ofp) ~ cfac(numchron), data = dt)

## visualization of each regressor
plot(clog(ofp) ~ health, data = dt, varwidth = TRUE,
     ylab = "Physician office visits (in clogs)", xlab = "Self-perceived health status", main = "health")
plot(clog(ofp) ~ cfac(numchron), data = dt,
  ylab = "Physician office visits (in clogs)", xlab = "Number of chronic conditions", main = "numchron")
plot(clog(ofp) ~ privins, data = dt, varwidth = TRUE,
  ylab = "Physician office visits (in clogs)", xlab = "Covered by private insurance", main = "privins")
plot(clog(ofp) ~ cfac(hosp, c(0:2, 8)), data = dt,
  ylab = "Physician office visits (in clogs)", xlab = "Number of hospital stays", main = "hosp")
plot(clog(ofp) ~ gender, data = dt, varwidth = TRUE,
  ylab = "Physician office visits (in clogs)", xlab = "Gender", main = "gender")
plot(cfac(ofp, c(0:2, 4, 6, 10, 100)) ~ school, data = dt, breaks = 9,
  ylab = "Physician office visits (number of visits)", xlab = "Number of years of education", main = "school")


##############
## Poisson  ##
##############
fm_pois <- glm(ofp ~ ., data = dt, family = poisson)
summary(fm_pois)

## Deviance test
1-pchisq(fm_pois$dev,fm_pois$df.residual)

####################
## OVERdispersion ##
####################

## Test
dispersiontest(fm_pois,trafo=1)
dispersiontest(fm_pois,trafo=2)

######################
## Quasi-Poisson    ##
######################
fm_qpois <- glm(ofp ~ ., data = dt, family = quasipoisson)
summary(fm_qpois)


######################
## Negative binomial #
######################
fm_nbin <- glm.nb(ofp ~ ., data = dt)
summary(fm_nbin)

## Deviance test
1-pchisq(fm_nbin$dev,fm_nbin$df.residual)


###############################
## zero-inflated Poisson regression #
###############################
fm_zip0 <- zeroinfl(ofp ~ ., data = dt)
summary(fm_zip0)

###############################
## zero-inflated NB regression #
###############################
fm_zinb0 <- zeroinfl(ofp ~ ., data = dt, dist = "negbin")
summary(fm_zinb0)



######################
## MODELING SUMMARY ##
######################

## Compare coefficients
fm <- list("ML-Pois" = fm_pois, "Quasi-Pois" = fm_qpois, "NB" = fm_nbin,
           "ZIP" = fm_zip0, "ZINB" = fm_zinb0)
round(sapply(fm, function(x) coef(x)[1:8]),3)

## Compare Standard errors
round(sapply(fm, function(x) sqrt(diag(vcov(x)))[1:8]),3)

## Compare log-likelihood
rbind(logLik = sapply(fm, function(x) round(logLik(x), digits = 0)),
      Df = sapply(fm, function(x) attr(logLik(x), "df")))

## Compare AIC and BIC
rbind(sapply(fm,AIC),sapply(fm,AIC,k=log(nrow(dt))))


## zero-counts
round(c("Obs" = sum(dt$ofp < 1),
        "ML-Pois" = sum(dpois(0, fitted(fm_pois))),
        "NB" = sum(dnbinom(0, mu = fitted(fm_nbin), size = fm_nbin$theta)),
        "ZIP" = sum(predict(fm_zip0, type = "prob")[,1]),
        "ZINB" = sum(predict(fm_zinb0, type = "prob")[,1])))

## zero model coefficients
t(sapply(fm[4:5], function(x) round(x$coefficients$zero, digits = 3)))

