### R code from vignette source 'ANOVA_1_factor_Test_permutacions_17_18.Rnw'
### Encoding: ISO8859-1

###################################################
### code chunk number 1: ANOVA_1_factor_Test_permutacions_17_18.Rnw:40-52
###################################################

# Lectura de les dades d'un assaig clínic on es volien comparar
# a = 3 tractaments  per a la hipertensió: 
# un placebo, un diurètic i un beta-bloquejant.
# Disseny balancejat, d'un total de 45 individus, ASSIGNATS ALEATORIAMENT 
assaig <- read.table("BetaBloc.txt", header = TRUE)
n <- length(assaig[,"Tratamiento"])
n
ntreat<- tapply(assaig[,"ReducTension"], assaig[,"Tratamiento"], length)
n1 <- ntreat[1]
n2 <- ntreat[2]
n3<-  ntreat[3]


###################################################
### code chunk number 2: ANOVA_1_factor_Test_permutacions_17_18.Rnw:58-66
###################################################

# Estadístic de test:

# Una tria inicial molt lògica seria utilitzar l'estadístic 
# F de l'ANOVA paramètrica normal:
estad.F = function(dat, testFactor = 1, response = 2) {
  return(oneway.test(dat[,response] ~ dat[,testFactor], var.equal = TRUE)$statistic)
}


###################################################
### code chunk number 3: ANOVA_1_factor_Test_permutacions_17_18.Rnw:70-90
###################################################
# Estadístic de test: Modificació equivalent a l'estadístic F de l'ANOVA paramètrica

estad <- function(dat, testFactor = 1, response = 2) {
  y <- dat[,response]
  fact <- dat[,testFactor]
  yi. <- tapply(y, fact, sum)
  return(sum(yi.*yi. / tapply(y, fact, length)))
}

estad.balanc <- function(dat, testFactor = 1, response = 2) {
  sums <- tapply(dat[,response], dat[,testFactor], sum)
  return(sum(sums*sums))
}

estad.tilde1<-estad.F(assaig)
estad.tilde1
estad.tilde2<-estad(assaig)
estad.tilde2
estad.tilde <- estad.balanc(assaig)
estad.tilde


###################################################
### code chunk number 4: ANOVA_1_factor_Test_permutacions_17_18.Rnw:98-99
###################################################
factorial(n)/(factorial(n1)*factorial(n2)*factorial(n3))


###################################################
### code chunk number 5: ANOVA_1_factor_Test_permutacions_17_18.Rnw:106-125
###################################################
#Nombre de permutacions
nperm <- 19999

assaig.perm <- assaig


# Generació de 'nperm' permutacions aleatòries, i càlcul d'estad.F
# sobre cada permutació:
set.seed(23771)
estad.perms <- replicate(nperm,
{
  assaig.perm[,2] <- sample(assaig[,2], replace = FALSE)
  estad.F(assaig.perm)
}
)
# Triga bastant...

# Visualització dels 20 primers valors F calculats sobre les permutacions:
estad.perms[1:20]


###################################################
### code chunk number 6: ANOVA_1_factor_Test_permutacions_17_18.Rnw:130-132
###################################################
# p-valor:
(sum(estad.perms >= estad.tilde1) + 1) / (nperm + 1)


###################################################
### code chunk number 7: ANOVA_1_factor_Test_permutacions_17_18.Rnw:137-147
###################################################
# El mateix procés, però ara calculant l'estadístic més senzill i equivalent a F:
set.seed(23771)
estad.perms <- replicate(nperm,
  {
    assaig.perm[,2] <- sample(assaig[,2], replace = FALSE)
    estad.balanc(assaig.perm)
  }
)

estad.perms[1:20]


###################################################
### code chunk number 8: ANOVA_1_factor_Test_permutacions_17_18.Rnw:152-153
###################################################
(sum(estad.perms >= estad.tilde) + 1) / (nperm + 1)


