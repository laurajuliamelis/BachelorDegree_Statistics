### R code from vignette source 'Coef_correlacio_Test_permutacions_17_18.Rnw'
### Encoding: ISO8859-1

###################################################
### code chunk number 1: Coef_correlacio_Test_permutacions_17_18.Rnw:36-44
###################################################
lawSchool <- read.table(file="Law_School.txt", header=T)

x <- lawSchool[,1]
y <- lawSchool[,2]

# coeficiente de correlación para los datos originales:
r <- cor(x,y)
r


###################################################
### code chunk number 2: Coef_correlacio_Test_permutacions_17_18.Rnw:50-51
###################################################
factorial(length(x))


###################################################
### code chunk number 3: Coef_correlacio_Test_permutacions_17_18.Rnw:56-59
###################################################
# Permutació de la segunda columna:
sample(y)



###################################################
### code chunk number 4: Coef_correlacio_Test_permutacions_17_18.Rnw:65-72
###################################################
# 'nperm' permutacions
set.seed(2431)
nperm <- 19999
r.perm <- replicate(nperm, cor(x, sample(y)))

#Veiem les 20 primeres
r.perm[1:20]


###################################################
### code chunk number 5: Coef_correlacio_Test_permutacions_17_18.Rnw:75-90
###################################################
# Significació de la correlació:
p.valor <- (sum(r.perm > r) + 1) / (nperm + 1)
p.valor
# p.valor significatiu per el test H0: estoc. indep. vs. H1: rho > 0

# No importa respecte de quina columna permuteam

set.seed(2431)
r.perm <- replicate(nperm, {
    cor(sample(x),y)
  }
)
# Significació de la correlación:
p.valor <- (sum(r.perm > r) + 1) / (nperm + 1)
p.valor


###################################################
### code chunk number 6: Coef_correlacio_Test_permutacions_17_18.Rnw:96-110
###################################################
# En lugar de la correlación se podría emplear la suma de productos
# Estadístico sobre la muestra original:
sumXY <- sum(x * y)

# Permutaciones aleatorias:
set.seed(2431)
nperm <- 19999
sumXY.perm <- replicate(nperm, {
  sum(x * sample(y))
}
)

p.valor <- (sum(sumXY.perm > sumXY) + 1) / (nperm + 1)
p.valor


###################################################
### code chunk number 7: Coef_correlacio_Test_permutacions_17_18.Rnw:115-134
###################################################
# Per al test bilateral seria millor sumXY centrada:

max.sumXY = sum(sort(x) * sort(y))
min.sumXY = sum(sort(x) * sort(y, decreasing = TRUE))
mean.sumXY = (min.sumXY + max.sumXY) / 2

sumXY <- sum(x * y) - mean.sumXY

# Permutacions aleatories:
set.seed(2431)
nperm <- 19999
sumXY.perm <- replicate(nperm, {
  sum(x * sample(y)) - mean.sumXY
}
)

p.valor <- (sum(abs(sumXY.perm) > abs(sumXY)) + 1) / (nperm + 1)
p.valor



