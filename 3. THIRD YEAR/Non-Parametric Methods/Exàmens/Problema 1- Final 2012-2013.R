# Utilitzarem les mateixes dades del Problema 1 (típica situació de
# dades aparellades) de la prova de síntesi 2012-2013, però per
# respondre la pregunta 1 utilitzarem una prova de permutacions.
# (És semblant al que demana la pregunta 6) del problema de l'examen,
# però allí s'ha de respondre a partir d'uns llistats, aquí farem els
# càlculs amb R)

# Dades:

esmorzars = matrix( c(
4.61, 3.84,
6.42,	5.57,
5.40,	5.85,
4.54,	4.80,
3.98,	3.68,
3.82,	2.96,
5.01,	4.41,
4.34,	3.72,
3.80,	3.49,
4.56,	3.84,
5.35,	5.26,
3.89,	3.73,
2.25,	1.84,
4.24,	4.14),
ncol = 2, byrow = TRUE
)

colnames(esmorzars) = c("CORNFLK", "OATBRAN")

# Hi ha n = 14 dades:
n = nrow(esmorzars)
n

# Diferències entre els valors de cada parella:
dif = esmorzars[,"CORNFLK"] - esmorzars[,"OATBRAN"]
dif

# A l'enunciat del problema demanava estudiar la mediana de les
# diferències. Aquí, primer, estudiarem la mitjana de les
# diferències.
# Valor de la mitjana sobre les diferències originals:
meanDif = mean(dif)
meanDif

# vector amb els valors absoluts de les diferències:
absDif = abs(dif)

# Aquesta funció calcula la mitjana de les diferències sobre una mostra
# permutada per files:
meanPerm = function(signs, absDiff) mean(signs * absDiff)
# 'signs' ha de ser un vector de valors -1 o +1 de la mateixa llargada que 'absDiff'

# Construirem una matriu de n = 14 columnes i 16384 files amb la
# codificació de totes les permutacions possibles (maneres de crear
# un vector de llargada 14 format de valors -1 o +1)
sgn = c(-1, +1)
signsTab = expand.grid(as.data.frame(matrix(rep(sgn, n), ncol = n)))
# Les primeres 10 files de 'signsTab':
signsTab[1:10,]
# Hi ha 16384 = 2^n permutacions possible, un valor totalment assequible.

# Càlcul de la diferència de mitjanes per cada permutació possible:
dMeansPerm = apply(signsTab, 1, meanPerm, absDiff = absDif)

# p-valor exacte del test unilateral mitjana.CORNFLK > mitjana.OATBRAN:
sum(dMeansPerm >= meanDif) / nrow(signsTab)

# L'enfoc de permutacions ens dóna bastanta llibertat per provar
# hipòtesis basades en diferents paràmetres, amb els corresponents
# estimadors. Per exemple, si volem fer una cosa semblant al que
# fa la prova de Wilcoxon dels signes amb rang (és a dir, estudiar
# les hipòtesis tal com es plantegen a l'enunciat original del problema)
# caldria calcular sobre les dades originals l'estimador de la mediana
# de les diferències (mediana de totes les seves semisumes)...
sSums = outer(dif, dif, "+") / 2
medianDif = median(sSums[lower.tri(sSums, diag = TRUE)])
medianDif

# ... i fer el mateix amb tots els conjunts de dades permutades.

# Aquesta funció calcula l'estimació de la diferència de medianes
# per qualsevol conjunt de dades permutades:
medianPerm = function(signs, absDiff) {
  d = signs * absDiff
  sSums = outer(d, d, "+") / 2
  median(sSums[lower.tri(sSums, diag = TRUE)])
}

# Possible versió alternativa (és un altre estimador)
#medianDif = median(dif)
#medianDif
#medianPerm = function(signs, absDiff) median(signs * absDiff)
# la resta d'instruccions valdrien igual.

# Calcul de l'estimador sobre cada permutació:
dMediansPerm = apply(signsTab, 1, medianPerm, absDiff = absDif)

# p-valor exacte del test unilateral CORNFLK > OATBRAN:
sum(dMediansPerm >= medianDif) / nrow(signsTab)

# ****************************************************************************
# Significació del coeficient de correlació de Pearson entre CORNFLK i OATBRAN
# ****************************************************************************
r = cor(esmorzars[,"CORNFLK"], esmorzars[,"OATBRAN"])
r
# El nombre de permutacions possibles és enorme, n! >> 2^n:
factorial(n)
# -------------------------------------------------------
# Enfoc determinista: en general bloquejaria l'ordinador
#require(gtools)
# Enumera totes les permutacions possibles de n elements:
#perms = permutations(n, n)
# etc.
# -------------------------------------------------------

# És més viable un enfoc de Monte Carlo:
x = esmorzars[,"CORNFLK"]
y = esmorzars[,"OATBRAN"]

r = cor(x,y)
r

nperms = 9999

set.seed(1237)
rPerms = replicate( nperms, cor(x, sample(y, replace = FALSE)))

# Alternativa unilateral: rho > 0
(sum(rPerms >= r) + 1) / (nperms + 1)
# Alternativa bilateral: rho != 0
(sum(abs(rPerms) >= abs(r)) + 1) / (nperms + 1)

sort.x = sort(x)
sort.y = sort(y)
min.sum = sum(sort.x * sort.y[n:1])
max.sum = sum(sort.x * sort.y)
centre = (min.sum + max.sum) / 2

sXY = abs(sum(x * y) - centre)
sXY
set.seed(1237)
sXYPerms = replicate( nperms, abs(sum(x * sample(y)) - centre))
(sum(sXYPerms >= sXY) + 1) / (nperms + 1)
