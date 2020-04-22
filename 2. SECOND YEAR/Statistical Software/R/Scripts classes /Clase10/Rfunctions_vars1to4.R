#################################################
###  Software Estadístic, 25.10.2016          ###
###  Functions vars1, vars2, vars3, and vars4 ###
#################################################

## Description: Functions vars1, vars2, vars3, and vars4
##              compute both the empirical variance
##              and the MLE of population variance
## Arguments:
##    x: A numeric vector
##
## Note: Function varML must already exist
##
## Codes:

## Function vars1
vars1 <- function(x)
{
  stopifnot(is.numeric(x))

  val1 <- var(x)
  val2 <- varML(x)
  return(val1)
  return(val2)
}


## Function vars2
vars2 <- function(x)
{
  stopifnot(is.numeric(x))

  val1 <- var(x)
  val2 <- varML(x)
  print(val1)
  cat("ML estimator:", val2, "\n")
}


## Function vars3
vars3 <- function(x)
{
  stopifnot(is.numeric(x))

  val1 <- var(x)
  val2 <- varML(x)
  return(list("Empirical variance" = val1,
              "ML estimator of the variance" = val2))
}


## Function vars4
vars4 <- function(x)
{
  stopifnot(is.numeric(x))

  val1 <- var(x)
  val2 <- varML(x)
  return(c("Var" = val1, "VarML" = val2))
}
