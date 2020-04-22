#########################################
###  Software Estadístic, 25.10.2016  ###
###  Function fourGraphs              ###
#########################################

## Description: Function simpGraphs draws four graphs of a numeric vector
## Arguments:
##    x: A numeric vector
##    clr: Color for boxplot and qqline
##    lwi: Line width for density plot and qqline
##    ...: Optional arguments to function par
##
## Code:

fourGraphs <- function(x, clr = 2, lwi = 2, ...)
{
   if (!is.numeric(x))
     stop("The argument must be a numeric vector!")

   windows(width = 8)
   par(mfrow = c(2, 2), ...)
   # The histogram
   hist(x, main = "Histogram of the data")
   # The boxplot
   boxplot(x, main = "Boxplot of the data", col = clr)
   plot(density(x, bw = 2*IQR(x, na.rm = T), na.rm = T), xlab = "x", ylab = "",
        main = "Kernel estimate of the density", type = "l", lwd = lwi)
   # QQ plot
   qqnorm(x)
   qqline(x, col = clr, lwd = lwi)
}
