### R code from vignette source 'Blocs_aleatoritzats_Test_permutacions_17_18v2.Rnw'
### Encoding: ISO8859-1

###################################################
### code chunk number 1: Blocs_aleatoritzats_Test_permutacions_17_18v2.Rnw:45-50
###################################################

# Lectura de les dades 
# Disseny balancejat, d'un total de 7 individus, ASSIGNATS ALEATORIAMENT 
# a cadascuna de les combinacions gasoses (atmofera)
dat = read.table("proves esforç.txt", header = TRUE)


###################################################
### code chunk number 2: Blocs_aleatoritzats_Test_permutacions_17_18v2.Rnw:53-59
###################################################
# Les mateixes dades en una presentació més adequada per la funció 'aov':
dades = data.frame(
  subjecte = rep(factor(1:nrow(dat)), each = 4),
  atmosfera = rep(factor(c("A","B","C","D")), nrow(dat)),
  distancia = as.vector(t(dat[,-1]))
)


###################################################
### code chunk number 3: Blocs_aleatoritzats_Test_permutacions_17_18v2.Rnw:67-68
###################################################
factorial(4)^7


###################################################
### code chunk number 4: Blocs_aleatoritzats_Test_permutacions_17_18v2.Rnw:75-90
###################################################
# Preparatius inicials:
# Les mateixes dades en una presentació més adequada per la funció 'aov': 
dades = data.frame(
  subjecte = rep(factor(1:nrow(dat)), each = 4),
  atmosfera = rep(factor(c("A","B","C","D")), nrow(dat)),
  distancia = as.vector(t(dat[,-1]))
)
taula.ANOVA = anova(aov(distancia ~ subjecte + atmosfera, data = dades)) 
taula.ANOVA
# Estadístic F per "atmosfera": 
taula.ANOVA["atmosfera","F value"]

 # o bé
f.obs = taula.ANOVA[2,4] 
f.obs


###################################################
### code chunk number 5: Blocs_aleatoritzats_Test_permutacions_17_18v2.Rnw:99-100
###################################################
factorial(4)^7


###################################################
### code chunk number 6: Blocs_aleatoritzats_Test_permutacions_17_18v2.Rnw:107-121
###################################################
# data.frame amb la mateixa estructura que les dades originals
# però que emmagatzemarà unes dades permutades:
dades.perm <- dades

# Una permutació aleatòria de la columna 'atmosfera':
dades.perm[1:4,2] <- sample(dades[1:4,2], replace = FALSE)
dades.perm[5:8,2] <- sample(dades[5:8,2], replace = FALSE)
dades.perm[9:12,2] <- sample(dades[9:12,2], replace = FALSE)
dades.perm[13:16,2] <- sample(dades[13:16,2], replace = FALSE)
dades.perm[17:20,2] <- sample(dades[17:20,2], replace = FALSE)
dades.perm[21:24,2] <- sample(dades[21:24,2], replace = FALSE)
dades.perm[25:28,2] <- sample(dades[25:28,2], replace = FALSE)
dades.perm



###################################################
### code chunk number 7: Blocs_aleatoritzats_Test_permutacions_17_18v2.Rnw:126-130
###################################################
dades.perm<-tapply(dades$atmosfera, dades$subjecte, sample)
dades.perm
# o equivalentment: 
# tapply( dades[,2], dades[,1], sample)


###################################################
### code chunk number 8: Blocs_aleatoritzats_Test_permutacions_17_18v2.Rnw:134-142
###################################################
# Estatístic F sobre aquestes dades permutades:
nperm = 9999
set.seed(123)
f.perms = replicate(nperm, {
  atmos.perm = unlist(tapply(dades$atmosfera, dades$subjecte, sample, replace = FALSE))
  anova(aov(dades$distancia ~ dades$subjecte + atmos.perm))[2,4]
}
)


###################################################
### code chunk number 9: Blocs_aleatoritzats_Test_permutacions_17_18v2.Rnw:145-146
###################################################
(sum(f.perms >= f.obs) + 1) / (nperm + 1)


###################################################
### code chunk number 10: Blocs_aleatoritzats_Test_permutacions_17_18v2.Rnw:153-169
###################################################
# En canvi aquest enfoc donaria exactament el mateix resultat que l'estadístic F:

f.simpli = function(x) {
  s = tapply(x$distancia, x$atmosfera, sum)
  sum(s * s)
}

set.seed(123)
dades.perm = dades
f.obs = f.simpli(dades)
f.perms = replicate(nperm, {
  dades.perm$atmosfera = unlist(tapply(dades$atmosfera, dades$subjecte, sample))
  f.simpli(dades.perm)
}
)
(sum(f.perms >= f.obs) + 1) / (nperm + 1)


