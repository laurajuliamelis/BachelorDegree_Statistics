#########################################
###  Software Estadístic, 25.10.2016  ###
###  Function simpGraphs              ###
#########################################

## Description: Function simpGraphs draws four graphs of a numeric vector
## Arguments:
##    x: A numeric vector
##
## Code:

simpGraphs <- function(x)
{
   windows()
   par(mfrow = c(2, 2))
   # The histogram
   hist(x, main = "Histogram of the data")
   # The boxplot
   boxplot(x, main = "Boxplot of the data")
   # Kernel density estimation
   plot(density(x, bw = 2*IQR(x, na.rm = T), na.rm = T), xlab = "x", ylab = "",
        main = "Kernel estimate of the density", type = "l")
   # QQ plot
   qqnorm(x)
   qqline(x)
}
