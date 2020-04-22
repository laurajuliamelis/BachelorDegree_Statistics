#########################################
###  Software Estadístic, 25.10.2016  ###
###  Writing your own functions       ###
#########################################
load("ClasseR_Oct25.RData")
ls.str()

## Writing just the name of a function returns its code
tapply
matrix

## Creation of a new function
## ==========================
# An example
varML <- function(x)
{
  n <- sum(complete.cases(x))
  val <- var(x, na.rm = T)*(n-1)/n
  return(val)
}

# Let's check whether varML was created and whether it works
ls()
varML
(x <- 1:10)
mean(x)
var(x)
varML(x)
var(immuno$nkiller1)
varML(immuno$nkiller1)
val                     # There is no such object

# We can also quickly read the code from an external file
rm(varML)
file.show("Rfunction_varML.R")
source("Rfunction_varML.R")
ls()
varML
varML(immuno$lympho1)

# Error messages
var(c(x, "BCN"))
varML(c(x, "BCN"))


## How to program a function so that it returns more than one result?
## ==================================================================
file.show("Rfunctions_vars1to4.R")
source("Rfunctions_vars1to4.R")
ls()

vars1(immuno$nkiller1)
vars2(immuno$nkiller1)
vars3(immuno$nkiller1)
vars4(immuno$nkiller1)

## The output of functions vars3 and vars4 can be stored in a new object
v2 <- vars2(immuno$nkiller1)
v3 <- vars3(immuno$nkiller1)
v4 <- vars4(immuno$nkiller1)
v2              # NULL!
v3              # A list
v4              # A vector

## A second example: function simpGraphs draws 4 graphs
## ====================================================
file.show("Rfunction_simpGraphs.R")
source("Rfunction_simpGraphs.R")
simpGraphs

x<-rnorm(10000, 10, 3)
y<-rchisq(10000, 10)
simpGraphs(x)
simpGraphs(y)

## We can improve that function adding some more arguments
file.show("Rfunction_fourGraphs.R")
source("Rfunction_fourGraphs.R")
# Function fourGraphs
fourGraphs

x <- rnorm(1000)
y <- rchisq(1000, 5)
fourGraphs(x)
fourGraphs(x, "khaki", 3)
fourGraphs(y, lwi = 5)
fourGraphs(y, clr = "steelblue", lwi = 3, las = 1, font = 2, font.axis = 3,
           font.lab = 4)

# Error message:
fourGraphs(c(x, "BCN"))

graphics.off()


## Little exercise:
## Modify function simpGraphs in such a way that the four graphs are stored
## in a pdf file. The users should be able to choose the name of the pdf file.
## ---------------------------------------------------------------------------
simpGraphsV2 <- function(x, name = "Grafics.pdf")
{
  pdf(name)
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
  dev.off()
}
