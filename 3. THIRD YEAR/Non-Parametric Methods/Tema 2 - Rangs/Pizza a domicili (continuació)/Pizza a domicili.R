dades = read.table("Pizza a domicili.txt", header = TRUE)
dades

# Imaginem que tenim la idea preconcebuda que A és més lenta en
# servir les pizzes que B, idea per exemple plasmada en que
# la veritable mitjana dels temps A és superior a la mitjana
# dels temps B (hipòtesi alternativa).
# Segurament la manera "paramètrica" de provar de respondre seria:
t.test(Temps ~ Empresa, data = dades, var.equal = TRUE, alternative = "greater")
# ...sembla que no tenim prou evidència

temps = dades$Temps
empresa = dades$Empresa

# Nombre total de dades:
N = nrow(dades)
N
# De les quals són A o B:
n = table(dades$Empresa)
n
nA = n[1]

# Guardem el valor de t per les dades "reals"
t.obs = t.test(Temps ~ Empresa, data = dades, var.equal = TRUE, alternative = "greater")$statistic
t.obs

# El valor observat t.obs és positiu, suggereix A > B, però la prova t no resulta concloent

# Com la prova t de Student ha determinat que el p-valor és 0.1446?

# Utilitzant l'estadístic t com a mesura de "com de més gran és A sobre B", però
# sense suposar normalitat de les dades, hi hauria alguna manera alternativa de calcular
# un p-valor? Ja als anys 1920 Ronald Fisher va imaginar una possibilitat, encara
# que llavors era pràcticament impossible de portar a la pràctica.

# Permutacions possibles (un número enorme):
factorial(N)
# Permutacions de 12 elements amb 7 i 5 repeticions = (12!) / (7! 5!)
# ( = combinacions de 12 elements agafats de 7 en 7)
choose(N, nA)
# ( = combinacions de 12 elements agafats de 12 - 7 = 5 en 5)
choose(N, N - nA)

# Enumeració de totes les permutacions amb repetició possibles:
iperms = combn(N,nA)
# Cada columna és una combinació/permutació amb repetició possible. 
# Mirem les 10 primeres:
iperms[,1:10]

indexos = 1:N
indexos

# Considerem per exemple la combinació número 10 (que és 1  2  3  4  5  7 11)
# A partir de com han quedat permutats els 7 primers, reconstruïm tot
# el vector 1,2,3,...,12 permutat
iperm10 = c(iperms[,10], indexos[-iperms[,10]])
iperm10

# Càlcul de l'estadístic t per aquesta permutació concreta:
t.test(temps[iperm10] ~ empresa, var.equal = TRUE)$statistic

# Convertim aquests càlculs en una funció:
t.testPerm = function(iperm, x, grup, ...) {
  indexos = 1:length(x)
  iperm = c(iperm, indexos[-iperm])
  t.test(x[iperm] ~ grup, ...)$statistic
}

# Per exemple:
t.testPerm(iperms[,10], temps, empresa, var.equal = TRUE)
# En canvi, si volguéssim calcular l'estadístic de Welch:
t.testPerm(iperms[,10], temps, empresa) # equival a posar: var.equal = FALSE


# Apliquem la funció t.testPerm a totes 792 combinacions (columnes d'iperms)
t.perms = apply(iperms, 2, t.testPerm, temps, empresa, var.equal = TRUE)

# P-valor de permutacions (calculant repetidament l'estadístic t de Student):
sum(t.perms >= t.obs) / ncol(iperms)

# Una prova de permutacions ens dóna una gran llibertat a l'hora de triar
# l'estadístic de test. Si per exemple algú considera que seria preferible
# utilitzar l'estadístic de Welch:
welch.perms = apply(iperms, 2, t.testPerm, temps, empresa, var.equal = FALSE)

# P-valor de permutacions, ara amb estadístic de Welch:
welch.obs = t.test(temps ~ empresa, var.equal = FALSE)$statistic
sum(welch.perms >= welch.obs) / ncol(iperms)

# L'ORDRE DELS VALORS ÉS EL QUE IMPORTA!:
# Fixem-nos que en un test de permutacions, l'única cosa que importa és l'ordre
# dels valors de l'estadístic de test que hem calculat sobre totes les permutacions.
# El valor concret que tenien no és important, l'únic que importa és quants d'ells
# són tant o més extrems que el valor que hem calculat (del mateix estadístic!)
# sobre les dades "reals"

# PER TANT, PROCUREM SIMPLIFICAR AL MÀXIM L'ESTADÍSTIC, SEMPRE QUE ES RESPECTI
# L'ORDRE:
# En realitat, l'estadístic t de Student i la simple diferència de les mitjanes
# mostrals, quant a test de permutacions, són equivalents ja que els valors
# queden ordenats exactament igual. Calcular cada vegada el denominador de 
# l'estadístic t només és perdre el temps:

# Funció amb una interfície similar a t.testPerm però que calcula la simple
# diferència de mitjanes:
dMeansPerm = function(indexs1, x, ...) {
  n1 = length(indexs1)
  sum(x[indexs1]) / n1 - sum(x[-indexs1]) / (length(x) - n1)
}

# P-valor de permutacions, ara amb estadístic "diferència de mitjanes":
dMeans.obs = mean(temps[1:nA]) - mean(temps[-(1:nA)])
dMeans.perms = apply(iperms, 2, dMeansPerm, temps)
sum(dMeans.perms >= dMeans.obs) / ncol(iperms)

# Exactament el mateix p-valor que amb l'estadístic t de Student!
# Però més ràpid:
require(microbenchmark)
microbenchmark(
  t.testPerm(iperms[10], temps, empresa, var.equal = TRUE),
  dMeansPerm(iperms[10], temps)
)

# Com a curiositat, és més ràpid fer 'sum(x)/length(x)' que 'mean(x)',
# per aquesta raó s'ha definit dMeansPerm com s'ha fet:
dMeansPerm2 = function(indexs1, x, ...) {
  mean(x[indexs1]) - mean(x[-indexs1])
}

microbenchmark(
  dMeansPerm(iperms[10], temps),
  dMeansPerm2(iperms[10], temps)
)


# ========================================================================
#             TESTS DE PERMUTACIONS "DE MONTE CARLO"
# ========================================================================

# A l'exemple anterior era perfectament realitzable enumerar TOTES les 
# permutacions possibles, de manera que teníem un test de permutacions
# EXACTE, el p-valor es calculava sobre la distribució exacta de tots
# els valors de l'estadístic, avaluat sobre totes les permutacions possibles

# Però fixem-no que només amb una mica més de dades, per exemple el doble,
# N = 24, amb nA = 14, nB = 10, el nombre de permutacions amb repetició
# possibles seria molt més gran (quasi 2 milions):
choose(24, 14)
# Sobre bastants ordinadors, tant per manca de memòria com pel temps que
# requeriria l'execució, seria molt problemàtic fer la prova de permutacions
# exacta. (Si ho voleu provar, feu-ho, però a casa...)

# Una alternativa és fer un test de Monte Carlo: generarem una mostra aleatòria
# de permutacions (gran, però no tant enorme com tota la població de permutacions)
# i a partir d'ella ESTIMAREM el p-valor. No serà el p-valor exacte, però serà
# (esperem) una estimació força precisa, que fiablement ens permetrà arribar a
# una conclusió.

# Generació d'una permutació aleatòria dels valors d'un vector:
sample(temps)

# (Si volguéssim que fos una mostra aletòria "amb reemplaçament" -és a dir,
# amb possible repetició o manca de valors a la mostra aleatòria-: 
# sample(temps, replace = TRUE), però això no ens interessarà fins
# al tema de "bootstrap".)

# O equivalentment dels índexos:
iperm = sample(N) # o bé: sample(1:N)
iperm
temps[iperm]

# Però tractant-se de valors enters és més ràpida la funció 'sample.int':
sample.int(N)
temps[sample.int(N)]

# El procediment és molt similar al cas exacte, però ara generem només una
# mostra (en general gran) de les permutacions possibles (que són moltes més):
nperm = 10000

# Si ho volem enfocar de forma similar a com hem resolt el cas exacte:
# Primer generem 'nperm' permutacions (solament el índexs del 1er grup):
# Gran matriu 7 x 10000, cada columna representa els índexs del primer
# grup, resultants d'una permutació aleatòria:
iperms.aleat = replicate(nperm, sample.int(N, size = nA))
# les 10 primeres de les 10000:
iperms.aleat[,1:10]

# Sobre cada permutació apliquem l'estadístic (en aquest cas fem servir
# el més ràpid, la diferència de mitjanes):
dMeans.perms.aleat = apply(iperms.aleat, 2, dMeansPerm, temps)
sum(dMeans.perms.aleat >= dMeans.obs) / ncol(iperms.aleat)

# Fixem-nos que l'estimació del p-valor, a partir d'una mostra de 10000
# permutacions aleatòries, és molt similar al veritable p-valor de
# permutacions, i ens condueix a la mateixa conclusió. 
# És lògic que així sigui, sobre una mostra molt gran, hem calculat
# la freqüència relativa de l'esdeveniment:
#    {estadístic sobre mostra permutada >= estadístic observat}
# que és un estimador no esbiaixat, eficient, etc.

# Però en realitat és preferible estimar el p-valor com:
(sum(dMeans.perms.aleat >= dMeans.obs) + 1) / (ncol(iperms.aleat) + 1)

# Aquesta estimació sempre és lleugerament més gran que la freqüència
# relativa, és positivament esbiaixada i per tant condueix a un test
# lleugerament conservador (els p-valors seran lleugerament més grans
# i per tant serà més difícil rebutjar H0). Però queda garantit que,
# per qualsevol nombre de permutacions aleatòries, la probabilitat de
# rebutjar H0 quan és certa (error de tipus I) no superarà el nivell 
# de significació que hàgim fixat.

# ---------------------------------------------------------------------
# Comentari final:
# És molt més important entendre la idea del que hem estat fent a tota
# aquesta pràctica que les instruccions R concretes, que poden ser
# diferents en funció del problema concret, de quina forma de l'estadístic
# estem considerant, etc. (O de quin entorn de treball, R, SAS,... fem
# servir.) 

# Pel cas de la prova de Monte Carlo anterior, si féssim servir
# la funció t.test de la forma plantejada inicialment (amb una fórmula)
# També hauríem pogut fer:

# Generem les permutacions completes, de tot el vector 1:N:
iperms.aleat = replicate(nperm, sample.int(N))

# Sobre cada permutació apliquem l'estadístic t:
t.perms.aleat = apply(iperms.aleat, 2, 
  function(iperm, x, grups) { 
    t.test(x[iperm] ~ grups, var.equal = TRUE)$statistic
  }, 
  temps, empresa)

# o més compacte, sense necessitat inicial de crear la matriu d'índexs
# permutats (iperms.aleat):
t.perms.aleat = replicate(nperm, 
  t.test(temps[sample.int(N)] ~ empresa, var.equal = TRUE)$statistic)
# i al final:
(sum(t.perms.aleat >= t.obs) + 1) / (ncol(iperms.aleat) + 1)

# O amb la diferència de mitjanes (sense crear primer la matriu dels 
# índexs permutats):
dMeans.perms.aleat = replicate(nperm, 
  dMeansPerm(sample.int(N, size = nA), temps))
sum(dMeans.perms.aleat >= dMeans.obs) / ncol(iperms.aleat)


# ##########################################################################
#                                   RANGS
#                      Prova de Mann-Whitney-Wilcoxon
# ##########################################################################

# Primer farem una petita trampa: aquestes dades tenen dues parelles de valors
# empatats
temps
# Treballarem amb unes dades 'y' lleugerament manipulades:
y = temps
# Desfarem els empats:
# Valors 5è i 8è: tots dos 20.2, el 5è el convertim en 20.3
y[5] = 20.3
# Valors 6è i 10è: tots dos 18.5, el 10è el convertim en 18.6
y[6] = 18.6

# Rangs dels valors de la mostra (conjunta):
rank.y = rank(y)
rank.y

# Estadístics basats en la suma de rangs:
# Suma de rangs del primer grup, l'empresa "A"
# (estadístic sovint anomenat W en honor a Wilcoxon):
sum(rank.y[1:nA])

# Estadístic W tal com el calcula R:
w = sum(rank.y[1:nA]) - nA * (nA + 1) / 2
w

# Estadístic "U":
nB = N - nA
names(nB) = "B"
u = min(w, sum(rank.y[(nA+1):N]) - nB * (nB + 1) / 2)
u

# Si, tal com hem explicat al principi, tenim la idea preconcebuda que A 
# és més lenta en servir les pizzes que B, per "demostrar" aquesta idea
# preconcebuda haurem d'establir una hipòtesi alternativa unilateral:
#           H1: mediana d'A > mediana de B
# Podem rebutjar H0 i, per tant, en certa manera haurem demostrat la nostra
# idea preconcebuda?

# Amb les taules de la U de Mann-Whitney, prova unilateral per alfa = 0.05,
# el valor crític per mides motrals nA = n1 = 7, nB = n2 = 5 és
# u*0.05(7,5) = 6.
# Com que NO es compleix que u <= 6 (atenció, per l'estadístic U "extrem" sempre
# vol dir "més petit o igual")
u <= 6
# No podem rebutjar H0, no hem demostrat que l'empresa A sigui més lenta que B.

# Amb l'estadístic W de Wilcoxon, tal com el calcula R:
wilcox.test(y ~ empresa, alternative = "greater")
# O bé:
wilcox.test(y[1:nA], y[(nA+1):N], alternative = "greater")

# Test asimptòtic normal (aquí no gaire correcte, ja que la mida mostral
# és molt petita, però fem els càlculs per practicar):
z = (w - nA * nB / 2) / sqrt(nA * nB * (nA + nB + 1) / 12)
z

# p-valor unilateral, cua de la dreta:
pnorm(z, lower.tail = FALSE)
# Evidentment la conclusió és la mateixa, però és molt més fiable el
# p-valor exacte que ens ha donat la funció 'wilcox.test' d'R

#      *********************** EMPATS ***************************
# Les dades reals presentaven empats. Quants exactament?

# Aquesta funció permet determinar quantes sèries d'empats hi ha, 
# i de quina llargada és cada sèrie d'empats:
# Funció que utilitzarem per detectar les sèries d'empats i la seva llargada:
ties = function(x) {
  ti = sapply(lapply(unique(x), function(xi, x) x %in% xi, x),sum)
  result = ti[ti > 1]
  if (length(result) > 0)
    names(result) = paste("t", 1:length(result), sep = "")
  return(result)
}

ti = ties(temps)
ti

# No existeixen taules exactes en presència d'empats, hi hauria massa
# configuracions possibles.
# Per mides petites, el programari acostuma senzillament a avisar que
# el p-valor no s'ha calculat de forma exacta, que en realitat és un valor
# més o menys erroni:
wilcox.test(temps ~ empresa, alternative = "less")
# Ni cas encara que li demanis un test exacte:
wilcox.test(temps ~ empresa, alternative = "less", exact = TRUE)

# Per mides grans, la situació és millor ja que hi ha correccions a la
# variància de l'estadístic W (o U) que fan el càlcul de l'estadístic
# Z més correcte:
z = (w - nA * nB / 2) / 
  sqrt((nA * nB * (nA + nB + 1) / 12) - 
        nA * nB * (sum(ti^3 - ti) / (12 * (nA + nB) * (nA + nB - 1))))
z

# p-valor unilateral, cua de la dreta:
pnorm(z, lower.tail = FALSE)


# TORNEM A LES DADES SENSE EMPATS
# Pseudomediana, estimació de la mediana de les diferències:
# Totes les possibles diferències entre A i B:
outer(y[1:nA], y[(nA+1):N], "-")
# Mediana d'aquestes diferències:
median(outer(y[1:nA], y[(nA+1):N], "-"))

# Interval de confiança per a la pseudomediana:
# Bilateral:
wilcox.test(y ~ empresa, alternative = "two.sided", conf.int = TRUE)
# Unilateral, d'acord amb la hipòtesi alternativa "greater":
wilcox.test(y ~ empresa, alternative = "greater", conf.int = TRUE)

# Càlcul manual de l'interval de confiança:
# Ordenem les diferències de menor a més gran:
d = sort(outer(y[1:nA], y[(nA+1):N], "-"))
d

# Posició dels extrems de l'interval de confiança dins el vector d:
# A la taula: u_0.05(7,5) = 5
lambda = 5 + 1
lambda
# Extrem inferior de l'interval de confiança:
d[lambda]

# Posició de l'extrem superior dins d:
nu = nA * nB - lambda + 1
# Extrem superior:
d[nu]

# Interval de confiança unilateral [extrem inferior, +infinit]
# Valor crític unilateral per 7 i 5 dades i 0.05: 6
# Extrem inferior de l'interval de confiança
d[6 + 1]
