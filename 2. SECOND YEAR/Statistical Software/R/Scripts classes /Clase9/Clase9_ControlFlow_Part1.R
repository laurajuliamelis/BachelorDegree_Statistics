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
