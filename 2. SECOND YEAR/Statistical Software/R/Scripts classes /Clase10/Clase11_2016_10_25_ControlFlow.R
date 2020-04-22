#########################################
###  Software Estadístic, 25.10.2016  ###
###  Control flow                     ###
#########################################
# install.packages("Hmisc")
library(Hmisc)
load("ClasseR_Oct25.RData")
ls()
head(immuno)
Label(immuno)

## Function for
## ============
## Example 1
## ---------
for (i in 1:5) {
  print(c(i, i^2, i^3))
}


## Example 2
## ---------
for (i in c(1, 4, 9, 10)) {
  print("¡Hola!, ¿Qué tal?")
  cat("La raíz cuadrada de", i, "es", round(sqrt(i), 3), fill = T)
}


## Function while
## ==============
set.seed(2010)
(x <- sort(sample(1:100, 15)))
i <- 1
while (x[i]^2 < 1000) {
  cat("El valor de x[", i, "] es ", x[i], " y el valor al cuadrado es ", x[i]^2,
      ".", sep = "", fill = T)
  i = i + 1
}
rm(i, x)


## An if-else loop
## ===============
(x <- round(rnorm(10, 0, 2), 4))
for (i in 1:length(x)) {
  if (x[i] > 0) {
    cat("La raíz cuadrada de", round(x[i], 3), "es", round(sqrt(x[i]), 3), "\n")
  } else {
    cat("¡Ojo!", x[i], "es un número negativo.\n")
  }
}
rm(i, x)


## Functions repeat and break
## ==========================
repeat {
  x <- rpois(3, 50)
  print(x)
  if (any(duplicated(x)))
    break
}
rm(x)


## Example 3 with for-loops:
## Descriptive analyses of all variables of a data frame
## -----------------------------------------------------
head(immuno)
for (i in c(2, 3, 6:9)) {
  if (is.numeric(immuno[, i])) {
    cat("\n", names(immuno)[i], "\n")
    print(summary(immuno[, i]))
  } else {
    cat("\n", names(immuno)[i], "\n")
    print(table(immuno[, i]))
  }
}


## A little exercise:
## Produce a pdf file that contains the boxplots of all numeric variables
## of the data frame immuno versus study group
## ----------------------------------------------------------------------
pdf("Boxplots.pdf")
par(las = 1, font = 2, font.axis = 2, font.lab = 4)
for (i in 6:9) {
  boxplot(immuno[, i]~group, immuno, xlab = "Study group", col = 2:4, pch = 16)
  title(names(immuno)[i])
}
dev.off()

## Variation of the exercise: One pdf file for each numeric variable
# Version 1
for (i in 6:9) {
  pdf(paste0(names(immuno)[i], ".pdf"))
  par(las = 1, font = 2, font.axis = 2, font.lab = 4)
  boxplot(immuno[, i]~group, immuno, xlab = "Study group", col = 2:4, pch = 16)
  title(names(immuno)[i])
  dev.off()
}

# Version 2
for (i in 6:9) {
  windows()
  par(las = 1, font = 2, font.axis = 2, font.lab = 4)
  boxplot(immuno[, i]~group, immuno, xlab = "Study group", col = 2:4, pch = 16)
  title(names(immuno)[i])
  savePlot(names(immuno)[i], type = "pdf")
}
# Close all graphic devices
graphics.off()


## ¡Ojo! for-loops may require long computation times
## ==================================================
# An example:
# Let's sum two vectors in three different ways
x <- rnorm(80000)
y <- rnorm(80000)

# A very bad habit
z <- NULL
for (i in 1:80000) {
  z <- c(z, x[i] + y[i])
}

# A bit better
z <- numeric(80000)
for (i in 1:80000) {
  z[i] <- x[i] + y[i]
}

# Comparison of computation time
system.time({
  z <- NULL
  for (i in 1:80000) {
    z <- c(z, x[i] + y[i])
  }
})


system.time({
  z <- numeric(80000)
  for (i in 1:80000) {
    z[i] <- x[i] + y[i]
  }
})


# And the winner is (not really a surprise):
system.time(z <- x + y)


## More on control flow:
?Control


## A useful function to avoid for-loops: ifelse
## ============================================
(x <- rbinom(10, 1, .5))
gender <- ifelse(x == 1, "F", "M")
gender
class(gender) # Atention: It is a character!

gender <- factor(ifelse(x == 1, "F", "M"))
class(gender) # Now it is a factor

# Another example
x <- rpois(50, 35)
factor(ifelse(x < 30, "Joven", ifelse(x < 40, "No tan joven", "Un poco mayor")))

# Remember: for the same purpose, we may also use functions cut or cut2 (Hmisc)
# library(Hmisc)
cut2(x, cuts = c(30, 40))


## Functions sapply and lapply
## ===========================
sapply(immuno, summary)
sapply(immuno[6:9], mean)

# If there were missing values
sapply(immuno[6:9], mean, na.rm = T) # Returns a vector (with names)
lapply(immuno[6:9], mean, na.rm = T) # Returns a list

# Position of the numeric variables of the data frame
num <- 

# Therefore, we can do the same in a nice manner:
lapply(immuno[num], mean, na.rm = T) # Returns a list
sapply(immuno[num], mean, na.rm = T) # Returns a vector (with names)


## A little exercise:
## -----------------
# Obtain the boxplot of each numeric variable from the data frame "immuno", and get
# the mean, median and coefficent of variation for each variable. Show the numerical
# results in a matrix format with the corresponding name of the variable.

num <- which(sapply(immuno, is.numeric)) # I get directly the numeric variables

# First attempt for the boxplots
par(las = 1, cex.main = 1)
boxplot(immuno[, num], main = "Cells per microliter vs. control variable",
        xlab = "Control variable", ylab = "Cells per microliter", col = 2:5)


# Second attempt for the boxplots: Better aspect in the left margin (axis and labels)
par(las = 1, oma = c(0, 1.5 ,0 ,0), cex.main = 1)
boxplot(immuno[, num], ylim = c(0,2500), main = "Cells per microliter vs. control variable",
        xlab = "Control variable", ylab = "", col = 2:5)
mtext("Cells per microliter", side = 2, line = 4, las = 0)

round(sapply(immuno[6:9], function(x)
     {c(mean = mean(x), med = round(median(x)), CV = sd(x)/mean(x))}), 2)
