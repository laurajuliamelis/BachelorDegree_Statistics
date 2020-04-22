# Ens caldrà la llibreria 'sm' (per "smooth") que amplia les
# possibilitats d'R per treballar amb estimacions nucli de la
# densitat, en particular permet estimacions per distribucions
# bi i trivariants
require(sm)
# A més d'instal·lar 'sm' manualment, normalment cal
# instal·lar també:
# la llibreria 'rgl'
# la llibreria 'rpanel'

# Tornem a les dades "Law School":
lawSchool <- read.table(file="law_school.txt", header=TRUE)

# Opcions necessàries per a que la llibreria 'sm' funcioni més
# adequadament pels nostres propòsits:
# --> eval.grid = FALSE per a que la densitat estimada s'avaluï sobre
# els valors mostrals i no sobre una graella de valors
# --> display = "none" per a que no mostri gràfics durant càlculs parcials
sm.options(eval.grid = FALSE, display = "none")

# Càlcul de l'estadístic de raó de versemblança (però versió no paramètrica,
# basat en l'estimació nucli de la densitat) per la hipòtesi nul·la
# d'independència estocàstica entre dues variables aleatòries X i Y:
# H0: fXY(x,y) = fX(x) * fY(y), per tot (x,y)
# en front a l'alternativa:
# H1: fXY(x,y) != fX(x) * fY(y), per algun (x,y).

# Estimació nucli de la densitat conjunta:
fLSAT.GPA = sm.density(lawSchool, eval.points = lawSchool)
# Estimació nucli de cada densitat marginal:
fLSAT = sm.density(lawSchool$LSAT, eval.points = lawSchool$LSAT)
fGPA = sm.density(lawSchool$GPA, eval.points = lawSchool$GPA)
# Estadístic de raó de versemblança:
sum( log(fLSAT.GPA$estimate / (fLSAT$estimate * fGPA$estimate))) / nrow(lawSchool)


# En forma de funció:
# L'argument 'xy' correspon a una matriu numèrica amb dues columnes, la
# primera columna s'interpreta com la mostra de valors de la variable X
# i la segona com els valors d'Y.
# Els arguments 'fxy', 'fx' i 'fy' han de ser vectors numèrics amb la
# mateixa llargada que nrow(xy); corresponen als valors de l'estimació de
# la densitat. En concret:
# 'fxy[i]' és l'estimació de la densitat bivariant per la parella xy[i,]
# 'fx[i]' és l'estimació de la densitat pel valor xy[i,1]
# 'fy[i]' és l'estimació de la densitat pel valor xy[i,2]
# Si algun d'aquests tres arguments (fxy, fx o fy) no es proporciona,
# es calcula automàticament amb la funció 'sm.density' de la llibreria 'sm',
# amb nucli normal i amplada de finestra ajustada automàticament per la
# mateixa funció 'sm.density'. Si tots tres es proporcionen, l'argument 'xy'
# és innecessari, no s'utilitza.

likStat = function(xy,
                   fxy = sm.density(xy, eval.points = xy)$estimate,
                   fx = sm.density(xy[,1], eval.points = xy[,1])$estimate,
                   fy = sm.density(xy[,2], eval.points = xy[,2])$estimate) 
{
  sum(log(fxy / (fx * fy))) / length(fxy)
}

# Exemples:
likStat(matrix(rnorm(50), ncol = 2))
likStat(lawSchool)
likStat(fxy = fLSAT.GPA$estimate, fx = fLSAT$estimate, fy = fGPA$estimate)

# La densitat marginal de la variable que no permutarem (p.e. LSAT)
# serà òbviament invariant respecte de permutacions, la podem obtenir 
# d'una vegada per totes, abans de començar a permutar:
fLSAT.estim = fLSAT$estimate

# Valor de l'estadístic de raó de versemblança sobre la mostra original:
lik.obs = likStat(lawSchool, fx = fLSAT.estim)
# fxy i fy es calculen "sobre la marxa" a 'likStat'
lik.obs

# Generarem 9999 permutacions aleatòries dels 82 valors GPA
# (és pràcticament impossible enumerar les 82! permutacions possibles)
nperm = 9999

lawSchool.perm = lawSchool
y = lawSchool[,2]
set.seed(123)
lik.perms = replicate(nperm,
{
  lawSchool.perm[,2] = sample(y)
  likStat(lawSchool.perm, fx = fLSAT.estim)
}
)

lik.perms[1:30]
# p-valor estimat:
(sum(lik.perms >= lik.obs) + 1) / (nperm + 1)
# es pot rebutjar H0...

# Es podrien utilitzar altres estadístics de test, que també podrien
# tenir sentit, però no tenim garantia que obtinguéssin el mateix
# p-valor, no hem demostrat monotonicitat aamb 'likStat' respecte de
# permutacions:
stat2 = function(xy, 
                 fxy = sm.density(xy, eval.points = xy)$estimate,
                 fx = sm.density(xy[,1], eval.points = xy[,1])$estimate,
                 fy = sm.density(xy[,2], eval.points = xy[,2])$estimate) 
{
  sum((fxy - fx * fy)^2)
  # O altres possibilitats:
  # sum(abs(fxy - fx * fy))
  # max(abs(fxy - fx * fy))
} 

stat2.obs = stat2(lawSchool, fx = fLSAT.estim)
stat2.obs

nperm = 9999

lawSchool.perm = lawSchool
y = lawSchool[,2]
set.seed(123)
stat2.perms = replicate(nperm,
{
  lawSchool.perm[,2] = sample(y)
  stat2(lawSchool.perm, fx = fLSAT.estim)
}
)

stat2.perms[1:30]

(sum(stat2.perms >= stat2.obs) + 1) / (nperm + 1)


# ***********************************************************************
#                          TEST BOOTSTRAP
# ***********************************************************************
# Una versió bootstrap del test anterior basat en la raó de versemblances
# per les estimacions nucli de les densitats, generant remostres bootstrap
# a partir d'aquestes estimacions nucli.
# ***********************************************************************

# seria lleugerament diferent a nivell de realització pràctica però molt
# diferent a nivell conceptual.

# Com simular una remostra bootstrap SEMIPARAMÈTRICA de lawSchool?

# Remostra d'LSAT partir de l'estimació nucli fLSAT:
# fLSAT correspon a la densitat de la suma de dues variables,
# la primera amb distribució igual a la empírica associada a lawSchool$LSAT
# i la segona amb distribució normal de mitjana 0 i desviació típica igual a
# la "band width" h amb la qual s'ha obtingut fLSAT.
# Per tant una remostra completa s'obtindria com:
sample(lawSchool$LSAT, replace = TRUE) + rnorm(nrow(lawSchool), sd = fLSAT$h)

# Similarment, per GPA:
sample(lawSchool$GPA, replace = TRUE) + rnorm(nrow(lawSchool), sd = fGPA$h)

# Com que volem simular matrius de dades sota condicions d'independència
# (H0 certa) una remostra bootstrap completa es pot obtenir generant
# independentment cada columna:
cbind(
  sample(lawSchool$LSAT, replace = TRUE) + rnorm(nrow(lawSchool), sd = fLSAT$h),
  sample(lawSchool$GPA, replace = TRUE) + rnorm(nrow(lawSchool), sd = fGPA$h)
)

# (Pregunta: com generaríem remostres bootstrap -matrius de dades- però sense
# forçar independència entre LSAT i GPA? És a dir, simulant la possible 
# dependència entre LSAT i GPA que les pròpies dades reflecteixen)

# Generació de 9999 remostres bootstrap i càlcul de l'estadístic sobre
# cadascuna:
nboot = 9999

x = lawSchool[,1]
y = lawSchool[,2]
n = nrow(lawSchool)
hLSAT = fLSAT$h
hGPA = fGPA$h

set.seed(123)
lik.boots = replicate(nboot,
{
  lawSchool.boot = 
    cbind(sample(x, replace = TRUE) + rnorm(n, sd = hLSAT), 
          sample(y, replace = TRUE) + rnorm(n, sd = hGPA))
  likStat(lawSchool.boot)
}
)

# p-valor del test bootstrap:
(sum(lik.boots >= lik.obs) + 1) / (nboot + 1)
