#########################################
###  Software Estadístic, 25.10.2016  ###
###  Function varML                   ###
#########################################

## Description: Function varML computes the MLE of the variance
## Arguments:
##    x: A numeric vector
##
## Note: Function varML does always strip all NA values.
##
## Code:

varML <- function(x)
{
  stopifnot(is.numeric(x))

  n <- sum(complete.cases(x))
  val <- var(x, na.rm = T)*(n-1)/n
  return(val)
}
