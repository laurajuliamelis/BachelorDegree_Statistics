# N = 22 dones, aleatòriament assignades n1 = 11 a rebre la droga D
# i n2 = 11 un placebo P. Totes 22 van rebre una dosi d'un anticonceptiu
# amb dos components: etinil estradiol (EE) i noretindrona (NET).
# Es volia estudiar si la presència de la droga D influia en en els nivells
# de EE i NET (i, per tant, en l'efectivitat i/o seguretat de l'anticonceptiu).
# El nivell dels components de l'anticonceptiu es mesurava mitjançant la
# variable "àrea sota la corba" (AUC) que designarem EEAUC i NETAUC 
# respectivament per EE i NET (i també amb la concentració màxima Cmax, que 
# no estudiarem aquí)

dat <- read.table("anticonceptive.txt", header = TRUE)

# Indicis de normalitat (o no) de les dades:
hist(dat[dat[,"tratam"]=="P","EEAUC"])
hist(dat[dat[,"tratam"]=="D","EEAUC"])

hist(dat[dat[,"tratam"]=="P","NETAUC"])
hist(dat[dat[,"tratam"]=="D","NETAUC"])

# Indicis d'igualtat de la dispersió (o no) de les dades P vs D
boxplot(EEAUC ~ tratam, data = dat)
boxplot(NETAUC ~ tratam, data = dat)

# És totalment DESACONSELLABLE fer proves prèvies amb les mateixes dades
# que després s'analitzaran (per exemple, prova de Lillieffors o khi-quadrat
# per investigar la possible normalitat, prova F per investigar la possible
# igualtat de variàncies) i en funció del resultat d'aquestes decidir quina
# prova de significació aplicar (per exemple, prova t de Student si prèviament
# no s'ha rebutjat ni la normalitat de les dades ni la igualtat de variàncies).
# Fent això en general tindrem tests no vàlids, que globalment no respectaran
# la probabilitat d'error de tipus I.

# Si tenim garantia que les condicions de normalitat i homogeneïtat de
# variàncies es compleixen (NO a través de pretests, sinó d'experènca prèvia
# i altres arguments), la millor aproximació és la prova t de Student:
t.test(EEAUC ~ tratam, data = dat, var.equal = TRUE)
t.test(NETAUC ~ tratam, data = dat, var.equal = TRUE)

t.test(EEAUC ~ tratam, data = dat, var.equal = TRUE, alternative = "less")
t.test(EEAUC ~ tratam, data = dat, var.equal = TRUE, alternative = "greater")

t.test(NETAUC ~ tratam, data = dat, var.equal = TRUE, alternative = "less")
t.test(NETAUC ~ tratam, data = dat, var.equal = TRUE, alternative = "greater")

# A tot el que segueix ens centrarem en la variable EEAUC. 
# Queda com exercici repetir-ho per les restants variables.

# Per no escriure tant, guardarem en variables separades algunes informacions 
# útils:

# Mides mostrals:
n <- tapply( dat[,"EEAUC"], dat[,"tratam"], length)
n                 
# Ara 'n' conté les mides mostrals de cada grup, n[1] == n[2] == 11
N <- sum(n)       
# 'N' serà el total d'observacions, 22
n1 <- n[1]
n2 <- n[2]

# Vector de valors d'EEAUC, ordenats de manera que els 11 primers 
# corresponguin a "D" i els 11 següents a "P":
auc <- dat[order(dat$tratam), "EEAUC"]

auc
auc[1:n1]         # Valors EEAUC per tots els casos "D"
auc[(n1+1):N]     # i per tots els casos "P"

# La prova t de Student anterior també es podria demanar així, 
# indicant els dos grups de vaors per separat:
t.test(auc[1:n1], auc[(n1+1):N], var.equal = TRUE)

# *******************************************************************************************************
#         ENFOC BASAT EN RANGS: TEST DE MANN-WHITNEY-WILCOXON
# *******************************************************************************************************

# Si no estem segurs de la validesa de les condicions anteriors, probablement 
# millor anàlisi "no paramètrica" o "lliure de la distribució" (denominació 
# més encertada). Cóm fer anàlisi vàlida sense necessitat de les suposicions
# anteriors?

# Un enfoc que principalment es va desenvolupar als anys 40 dels segle passat 
# és el basat en rangs.
# Soluciona bastant bé el problema d'una possible manca de normalitat, 
# NO el d'heterogeneïtat de variàncies.

# rang (número d'ordre) de cada observació de EEAUC:
rangs <- rank(auc)
rangs
# rangs de les observacions "D"
rangs[1:n1]            
# atenció! NO és el mateix que rank(auc[1:n1]) que serien els rangs de les observacions
# D per separat, i que anirien de 1 a n1, ens referim als rangs de les
# observacions del grup D però dins del TOTAL de dades (possibles rangs de 1 a N) !!!

# rangs de les observacions "P"
rangs[(n1+1):N]        
# atenció! NO és el mateix que rank(auc[(n1+1):N])   !!!

# Mann i Whitney (i Wilcoxon amb un any de diferència, fent uns càlculs diferents
# però equivalents) van deduir quina seria la distribució de les sumes de rangs de
# cada grup per separat si fos certa la H0 d'igualtat en la localització (normalment
# es pensa en termes de "medianes": en una variable contínua, aquell valor que deixa
# a banda i banda 1/2 de probabilitat).
# Si esencialment la forma de la distribució d'un grup i de l'altre fos la mateixa
# i tots dos grups estessin centrats en el mateix lloc, totes les possibles
# configuracions de rangs (de les quals n'hi ha N! = N(N-1)(N-2)···2·1), com ara:
#    16  5 10  7 19  3 17  4 11  9 12 21  6 14  8 18  1 22 15  2 20 13
# (que vol dir que, casualment, el valor primer del grup 1 ha quedat amb rang 16,
# el valor segon del grup 1 ha quedat amb rang 5, etc.), o com ara:
#    11 17 15  8  6 16 10 13 21 19 12  2  4  1 14  5 18  7 22  9  3 20, etc.
# serien igualment probables.
# És una simple qüestió de permutacions, totes tindrien probabilitat 1 /(N!).
# En aquest cas (H0 d'igualtat entre poblacions certa), la tendència hauria de ser
# que els valors més probables de la suma dels rangs d'un grup no fossin gaire 
# extrems.
# Un simple càlcul de probabilitat ens porta a que les configuracions més abundants
# han de ser aquelles a les quals els rangs queden bastant repartits dins cada grup,
# ni tots molt grans ni tots molt petits.

# Si no hi haguessin diferències entre la localització dels dos grups, la tendència hauria de ser
# que les sumes dels rangs dins cada grup fossin similars:
sumaRangs <- c(sum(rangs[1:n1]), sum(rangs[(n1+1):N]))
names(sumaRangs) <- c("D", "P")
sumaRangs

# Intuïm que potser sí que hi ha diferències...

# Si la mida mostral fos diferent, podríem dir una cosa semblant de les mitjanes
# dels rangs:
mean(rangs[1:n1])
mean(rangs[(n1+1):N])

# Sembla que tenim un indici que hi ha certa diferència.

# Fixem-nos que no cal considerar tots els possibles valors de la suma de rangs
# d'un grup i de l'altre: en total tots els rangs 1, 2, ..., N sempre sumen
# N(N+1)/2. Per tant, calculada la suma de rangs d'un grup (diguem-ne R1), la 
# suma de rangs de l'altre grup és R2 = N(N+1)/2 - R1. Les sumes de rangs de cada 
# grup són com a dipòsits comunicants, si un és gran, l'altre per força és petit,
# etc.
#
# Una forma diferent de raonar tot l'anterior és la següent: si H0 tal com l'hem
# considerada abans és certa, la probabilitat que un valor qualsevol agafat a
# l'atzar del primer grup sigui més gran que un valor agafat a l'atzar del segon
# grup és 1/2 (i viceversa). 
# Comptarem quantes vegades un valor del primer grup ha superat un valor del segon grup.
# Per exemple amb les nostres dades, la següent instrucció ens dona una taula amb
# TRUE quan és veritat que determinat valor D supera un altre valor P:
outer(auc[1:n1], auc[(n1+1):N], ">")
# i això ens indica quantes vegades ha passat:
s = sum(outer(auc[1:n1], auc[(n1+1):N], ">"))
s

# Novament, si H0 és certa, el valor de "s" no pot ser ni massa gran (màxim
# n1 * n2, que correspondria al cas que tots els valors D fossin superiors als 
# valors P) ni massa petit (mínim 0, tots els valors D inferiors P), i les 
# probabilitats associades a aquests valors sota H0 es poden calcular de forma
# relativament senzilla a partir del fet que la probabilitat
# P[D > P] = 1/2.

# R implementa la versió de Wilcoxon, sovint designada com prova de 
# Mann-Whitney-Wilcoxon
wilcox.test(EEAUC ~ tratam, data = dat)
wilcox.test(EEAUC ~ tratam, data = dat, alternative = "greater")
# o també:
wilcox.test(auc[1:n1], auc[(n1+1):N])
wilcox.test(auc[1:n1], auc[(n1+1):N], alternative = "greater")

# Si volem també una estimació puntual de la mediana de les diferències entre 
# D i P (que NO coincideix amb mediana de D - mediana de P) i el corresponent
# interval de confiança:
wilcox.test(auc[1:n1], auc[(n1+1):N], conf.int = TRUE)
# Pel test unilateral tenim un interval també unilateral:
wilcox.test(auc[1:n1], auc[(n1+1):N], conf.int = TRUE, alternative = "greater")


# Es pot demostrar que el raonament a base de comptar quantes vegades D > P 
# és equivalent al raonament que hem considerat inicialment, basat en rangs. 
# Concretament, es pot deduir la distribució de l'estadístic que s'acostuma a
# anomenar U de Mann-Whitney:
# (És purament una qüestió de reduir al mínim els càlculs, que en aquella època
# eren pràcticament manuals. Es tracta de buscar la taula de valors crítics que
# ocupi el mínim d'espai possible i que sigui més ràpida de calcular.)

u <- min(n1 * n2 + n1 * (n1 + 1) / 2 - sumaRangs[1], n1 * n2 + n2 * (n2 + 1) / 2 - sumaRangs[2])
# (o equivalent i més compacte:)
u <- min(n1 * n2 + n * (n + 1) / 2 - sumaRangs)
u
# (o també, potser la millor expressió equivalent):
u <- min(sumaRangs - n*(n+1)/2)
u

# Aquest valor "u" s'ha de comparar amb un valor crític a les taules:

# Pel test bilateral H1: medianaD != medianaP,
# si n1 = n2 = 11,
# tenim que el valor critic per alfa = 0,05 és 30.
# Criteri: Si u < valor crític rebutjarem H0.
# Decisió: al nostre cas, u = 32 ==> no tenim arguments per rebutjar H0.
# Conclusió: cap, prova inconclusiva. No hem pogut demostrar que hi hagi diferències. No rebutjar H0
# NO vol dir que H0 sigui certa: NO hem arribat a cap conclusió interessant.

# Pel test unilateral H1 medianaD > medianaP,
# el valor crític per alfa = 0,05 és 34.
# Criteri: si u < valor crític i mitjana dels rangs D > mitjana dels rangs P, rebutjarem H0.
# Decisió: u = 32 < 34, mitjana dels rangs D = 14,09 > mitjana dels rangs P = 8,91 ==> rebutgem H0.
# Conclusió: sembla que podem afirmar que la mediana de EEAUC és superior en prendre D.


# Referències: 
# A part de la bibliografia del curs (especialment Hollander i Wolfe), també podeu utilitzar la
# descripció de la Wikipedia, força correcta en la versió anglesa, 
# http://en.wikipedia.org/wiki/Mann%E2%80%93Whitney_U.
# (Però no confongueu allò que explica al final sobre "àrea sota la corba" AUC amb el tema d'aquest
# exemple, és una altra cosa.)
# 

# ******************************************************************************
#                     TESTS DE PERMUTACIONS
# ******************************************************************************

# Una idea intuitiva similar a la empleada en el enfoque basado en rangos
# sería la siguiente:
# Si las dos muestras que comparamos (D y P) son iguales en cuanto a la 
# variable que estamos estudiando (AUC), pertenecer a D o a P es puramente
# una etiqueta, no tiene ninguna relación con que AUC sea sistemáticamente
# grande o pequeño. Como consecuencia, es igualmente probable lo que hemos
# observado (ciertos valors de AUC asociados a D y ciertos valors asociados
# a P) que cualquier permutación de los datos, en la que los valores de AUC
# correspondiesen de otra manera a D y a P. Para establecer la significación
# de la diferencia que hemos observado en nuestros datos reales, comparamos
# este valor con el valor de la diferencia para todas las posibles 
# permutaciones de los datos.
#
# Un método de "pura fuerza bruta" sería enumerar todas las posibles
# permutaciones de los datos, y sobre cada una de ellas calcular un estadístico
# adecuado (por ejemplo, el estadístico t del test paramétrico) y contar cuantas
# veces este valor es más extremo que el valor del mismo estadístico calculado
# sobre los datos reales.
#
# Para enumerar todas las permutaciones se puede emplear la librería 'gtools':
# install.packages('gregmisc')
# require(gregmisc)
# permutations(N,N)
# daría la lista de tales permutaciones, pero habría casi 1.2x10^21, prácticamente
# imposible de realizar, por tiempo y por espacio de memoria

# Si el estadístico que vamos a calcular solamente depende de qué observaciones
# quedan en cada grupo, pero no del orden de las observaciones dentro de cada grupo,
# una posibilidad más razonable es emplear la función 'combn' de la librería
# estándar 'utils' (se carga por defecto) que enumera todas las combinaciones
# posibles. Por ejemplo
combn(5,3)  # equivalente a 'combn(1:5,3)'
# enumera todos los índices 1:5 cogidos en grupos de 3 sin repetición
combn(5,3,sum)
# enumera todos los índices 1:5 en grupos de 3 y a cada una de tales combinaciones
# le aplica la función 'sum'

# Si solamente se quiere contar el número de combinaciones, no enumerarlas:
choose(5,3)

# En nuestro ejemplo, el número de combinaciones de N (= 22) datos elegidos
# de 11 en 11 sería muy grande, pero manejable:
choose(N,n1)

# Del vector de 22 valores de EEAUC, enumeraremos todos los posibles grupos
# de 11 (todas las posibles maneras en que a 11 de ellos se podrían asignar
# a la categoría "D" -se puede suponer que por defecto los restantes reciben
# la etiqueta "P")
# (nótese que cada una de estas combinaciones tiene igual probabilidad, le
# corresponden 11! ordenaciones posibles de estos valores)
#
# Supongamos que hemos decidido basar nuestro test en el estadístico t de student
# (como veremos luego, hay otras opciones equivalentes y más eficientes).
# Vamos a realizar un test de permutaciones paso a paso:
# En primer lugar, vamos también a crear una función que nos devuelva el estadístico
# t de Student. Según la ley del mínimo esfuerzo, aprovecharemos 't.test'
tStat <- function(indexs, vector.dades) {
  t.test(vector.dades[indexs], vector.dades[-indexs], var.equal = TRUE)$statistic
}

# Fijémonos en que esta función, como primer argumento, recibe los índices de los
# valores que constituyen el primer grupo y como segundo argumento el vector con
# todos los valores.

# 1) Cálculo del estadístico t para los datos originales:
tauc <- tStat(1:n1, auc) 
tauc

# 2) Enumeramos todas las posibles combinaciones de los índices 1:22 escogidos en grupos de 11:
cindexs <- combn(1:N, n1)  # o simplemente combn(N, n1)
# Tarda un buen rato...

# Visualizamos las 15 primeras combinaciones:
cindexs[,1:15]
# una cualquiera de estas combinaciones, por ejemplo la número 12235:
cindexs[,12235]
# que corresponde a los índices 1  2  3  4  5 13 14 18 20 21 22.
# Fijémonos en que
tStat(cindexs[,12235], auc)
# correspondería al cálculo del estadístico t de Student si en realidad el grupo "D"
# estuviese formado por los casos 1  2  3  4  5 13 14 18 20 21 22 (y no 1:11 como ocurre
# con los datos reales)

# 3) Cálculo del estadístico t de Student para cada una de estas reordenaciones de los índices:
tPerms <- apply(cindexs, 2, tStat, auc)
# (a lo largo de la segunda dimensión -las columnas- de 'cindexs' aplico 'tStat', que como
# segundo argumento espera recibir el vector completo de valores, 'auc')

# 4) Finalmente, p-valor: proporción de ocasiones en que el estadístico calculado sobre las
# permutaciones es tan o más extremo que el calculado sobre los datos originales:
sum(tPerms >= tauc) / length(tPerms)

# El p-valor se refiere al test unilateral en el que consideramos la hipótesis alternativa
# H1 :  media de D > media de P. Vemos que el resultado es significativo al nivel 0.05.

# Para realizar el test bilateral con H1 :  media de D != media de P
# el estadístico más apropiado sería el valor absoluto de la t de Student, ya que tanto las
# diferencias negativas como las positivas, siempre que estén muy alejadas de cero, son
# evidencias contra la hipótesis nula.
# Por lo tanto, el p-valor en este caso se podría calcular como:
sum( abs(tPerms) >= abs(tauc)) / length(tPerms)
# Sin resultado significativo al 0.05.

# Nota: en un test de permutaciones es mucho más cómodo calcular el p-valor, y decidir en función
# de él. Equivalentemente podriamos haber obtenido una tabla de valores críticos. Por ejemplo, en el test
# unilateral, los valores críticos asociados a varios niveles de significación, PARA ESTOS DATOS,
# serían:
alphas <- c(0.10, 0.05, 0.025, 0.01)
tabla <- quantile( tPerms, probs = 1 - alphas)
names(tabla) <- alphas
tabla
# (se podrían comparar con los valores críticos del test t de Student):
qt(1 - alphas, df = N - 2)

# Hemos resaltado la frase "PARA ESTOS DATOS": 
# En un test de permutaciones, la tabla de valores criticos se obtiene UNA VEZ 
# CONOCEMOS LOS DATOS, es CONDICIONAL A LOS DATOS. Para algunos autores esto 
# constituye un problema filosófico importante: inferencia a nivel de la muestra
# y no de la población.

# En un test de permutaciones lo que realmente importa es el orden en el que
# quedan los valores del estadístico. En este sentido el resultado sería
# exactamente el mismo empleando el estadístico t, o bien la simple diferencia de medias,
# o bien, SOLAMENTE PARA EL CASO BALANCEADO (como el del ejemplo n1 == n2 == 11), simplemente
# la suma de los valores de uno de los grupos, por ejemplo "D":

combs <- combn(auc, n1)
# visualizo las 20 primeras (de 705432) combinaciones de los 22 valores de EEAUC 
# elegidos en grupos de 11:
combs[,1:20]

# Sobre cada una de estas 705432 combinaciones debo calcular el estadístico de test
# (elijo la suma, siendo como es equivalente a otros más complicados, pensemos que
# se tendrá que calcular un número muy grande de veces).
# En una única operación puedo generar las combinaciones y aplicar el estadístico sobre
# cada una de ellas:
sums <- combn(auc, n1, sum)

# Valor de la suma de los 11 datos que realmente son "D" (test unilateral):
suma.D <- sum(auc[1:n1])
# p.valor exacto:
sum(sums >= suma.D) / length(sums)

# En un caso no balanceado tendríamos que emplear un estadístico como la
# diferencia de medias.
# Sería más rápido que calcular cada vez la t:
diff.means <- function(indexs, vector.dades) {
  mean(vector.dades[indexs]) - mean(vector.dades[-indexs])
}

# Cálculo de la diferencia de medias entre D y P para los datos reales:
dmeansReal <- diff.means( 1:n1, auc)
# y para todas las permutaciones:
dmeansPerm <- apply(cindexs, 2, diff.means, vector.dades = auc)

# Test unilateral: D > P ?
sum(dmeansPerm >= dmeansReal) / length(dmeansPerm)

# Test bilateral: D != P ?
sum(abs(dmeansPerm) >= abs(dmeansReal)) / length(dmeansPerm)

# Fijémonos en que los p-valores son exactamente los mismos que empleando el
# estadístico t

# *************************************************************************************************************
#                                      EXERCICI:
#       Realitza un test de permutacions amb ELS RANGS de les dades anteriors 
#       i compara el resultat amb el del test de Mann-Whitney. Conclusió?
# *************************************************************************************************************

rangs <- rank(auc)

# Cálculo de la diferencia de medias entre D y P para los datos reales:
dmeansReal <- diff.means( 1:n1, rangs)
# y para todas las permutaciones:
dmeansPerm <- apply(cindexs, 2, diff.means, vector.dades = rangs)

# Test unilateral: D > P ?
sum(dmeansPerm >= dmeansReal) / length(dmeansPerm)

# Test bilateral: D != P ?
sum(abs(dmeansPerm) >= abs(dmeansReal)) / length(dmeansPerm)


# -------------------------------------------------------------------------------------------------------
#                 Test de permutacions "de Montecarlo" o aproximat
# -------------------------------------------------------------------------------------------------------

# Per mides mostrals mitjanes o grans els tests de permutacions exactes, com els anteriors, són
# impossibles de realitzar. La solució més habitual és fer un test de permutacions aproximat o de
# Montecarlo. En aquest enfoc, es simula una mostra aleatòria de permutacions, normalment molt 
# gran però de mida inferior a tota la "població" de possibles permutacions, que pot ser enorme
# (per exemple, si tinguèssim unes mostres de mida doble a les de l'exemple, el nombre de combinacions
# possibles -el de permutacions encara seria molt més gran- seria de més de 2.1 x 10^12).
# Procedint d'aquesta manera s'obté una estimació del p-valor, que es pot suposar molt precisa.

# Versió de Montecarlo del test anterior (farem servir l'estadístic "diferència de mitjanes"):

# Fixem el nombre de permutacions aleatòries a generar, per exemple 100000
nperm <- 100000

# una permutació aleatòria dels índexs 1:N
sample(1:N, replace = FALSE)  # replace = FALSE és el valor per defecte, no cal indicar-ho
# tal com hem definit la funció 'diff.means', només espera rebre els índexos, permutats, corresponents
# al grup "D", i el vector amb les dades originals.
sample(1:N, size = n1)

# Generem 'nperm' permutacions aletòries i sobre cada una d'elles calculem 'diff.means':
set.seed(1324)
dmeansPerm <- replicate( nperm, diff.means(sample(1:N, size = n1), auc))
dmeansPerm[1:10]

# Valor de diferència de mitjanes sobre la mostra real
dmeansReal <- diff.means( 1:n1, auc)

# un estimador no esbiaixat del p-valor seria la freqüència relativa:
# Test unilateral: D > P ?
sum(dmeansPerm >= dmeansReal) / nperm

# Test bilateral: D != P ?
sum(abs(dmeansPerm) >= abs(dmeansReal)) / nperm

# De tota manera és preferible l'estimador proposat per Dwass(1957), que és esbiaixat negativament però
# assegura que es respectarà el nivell de significació nominal (de tota forma, si el nombre de permutacions
# aleatòries és gran, la diferència és imperceptible)
# Test unilateral: D > P ?
(sum(dmeansPerm >= dmeansReal) + 1) / (nperm + 1)

# Test bilateral: D != P ?
(sum(abs(dmeansPerm) >= abs(dmeansReal)) + 1) / (nperm + 1)



# Una de las ventajas de los tests de permutaciones es que podemos emplear el estadístico que
# mejor exprese el tipo de diferencias que deseamos poner de manifiesto, no nos vemos obligados
# a emplear un estadístico cuya distribución bajo H0 sea matemáticamente fácil de determinar,
# con buenas propiedades estadísticas. El mecanismo de permutación nos permite ignorar la
# necesidad de buen comportamiento matemático.

# Per exemple, podria passar perfectament que sospitèssim de la presència de dades extremes,
# outliers. Un inconvenient del test de permutacions que hem discutit anteriorment respecte
# del test de Mann-Whithey és que es considera que és més sensible a la presència d'outliers.
# Potser, si en lloc de calcular les mitjanes calculèssim les medianes seria més robutst.
# Com funcionaria un test basat en la diferència de medianes? Sota un enfoc de permutacions
# seria perfectament raonable:

diff.medians <- function(indexs, vector.dades) {
  median(vector.dades[indexs]) - median(vector.dades[-indexs])
}

# Diferencia de medianes entre D y P per a les dades originals:
dmediansReal <- diff.medians( 1:n1, auc)
# y para todas las permutaciones:
dmediansPerm <- apply(cindexs, 2, diff.medians, vector.dades = auc)

# Test unilateral: D > P ?
sum(dmediansPerm >= dmediansReal) / length(dmediansPerm)

# Test bilateral: D != P ?
sum(abs(dmediansPerm) >= abs(dmediansReal)) / length(dmediansPerm)

# No hi ha cap resultat significatiu, ja era d'esperar degut a que, en aquest cas,
# les medianes no són tant diferents com les mitjanes:
median(auc[1:n1])
median(auc[(n1+1):N])
# (però també podria ser que la major diferència entre mitjanes fos deguda a algun outlier...)



# ******************************************************************************
#                   TEST DE KOLMOGORV-SMIRNOV
# ******************************************************************************

# Distribució empírica o mostral de les dades "D":
ecdfD <- ecdf(auc[1:n1])
# Distribució empírica o mostral de les dades "P":
ecdfP <- ecdf(auc[(n1+1):N])

# 'ecdfD' i 'ecdfP' són funcions, són funcions escalonades.
# Per exemple, valor d'ecdfD pel valor x = 500
ecdfD(500)
# o pel valor x = 4000
ecdfD(4000)

# Podem calcular el seu valor sobre tots els valors auc:
ecdfD(auc)
# o millor:
auc.ord <- sort(auc)
ecdfD(auc.ord)
ecdfP(auc.ord)

windows(14, 14)

# Els objectes de classe "ecdf" són funcions, una versió especialitzada 
# de la classe "stepfun", de qui hereden el mètode "plot".
# Dibuixem les distribucions empíriques de D (en negre) i P (en blau), 
# superposades:
plot(ecdfD, auc, main = "")
lines(ecdfP, auc, col = "blue")

# Sembla que són força diferents, amb P estocàsticament inferior a D,
# intuïm: P(P > D) > 1/2

# Calculem l'estadístic de Kolmogorov-Smirnov (contrast bilateral):
Dn = max(abs(ecdfD(auc) - ecdfP(auc)))
Dn

# Valor crític per H1 : FD != FP: 0.636 a la taula que hi ha al Campus virtual.
# Com que Dn < 0.636 no podem rebutjar H0

# De vegades s'utilitza l'estadístic:
n1 * n2 * Dn

# Estadístic per contrast unilateral H1 :  FD < FP (que correspon a la 
# tendència D > P !)
DMinus = max(ecdfP(auc) - ecdfD(auc))
DMinus

# (l'estadístic pel contrast unilateral H1 :  FD > FP seria (D < P):
# DPlus = max(ecdfD(auc) - ecdfP(auc))
# )

# Implementació R del test de K-S:
# Bilateral, el resulat concorda amb quan ho hem fet pas a pas, encara que
# aquí ens basarem en el p-valor
ks.test(auc[1:n1], auc[(n1+1):N]) 
# Unilateral, H1 :  FD < FP
ks.test(auc[1:n1], auc[(n1+1):N], alternative = "less")



# ******************************************************************************
#                   METODOLOGIA BOOTSTRAP
# ******************************************************************************

# L'enfoc bootstrap és més general que els anteriors. Ens proporciona una metodologia
# general per intentar determinar la distribució mostral d'un estadístic d'interés.
# Un cop coneguda aquesta distribució (o alguns aspectes rellevants d'ella, com les
# probabilitats o els quantils a les cues), es poden intentar realitzar els processos
# habituals en inferència estadística, com ara obtenir un interval de confiança o
# resoldre un contrast d'hipòtesis.

# Pensem novament en l'exemple que és fil conductor de tot aquest tema. L'objectiu
# és comparar, respecte de la variable EEAUC, el grup de dones D amb el grup P. El
# paràmetre principal d'interés és la diferència entre les mitjanes poblacionals
# (o qualsevol altra mesura de localització adequada) d'ambdos grups. Diguem-ne
# 'delta' d'aquest paràmetre. Suposem que, de moment, hem decidit basar la nostra
# inferència en l'estadístic t per dues mostres:
# t = (('mitjana mostral D' - 'mitjana mostral P') - delta) / (s * sqrt(1/n1 + 1/n2))
# on 's' representa l'estimació global (pooled) de la desviació estàndard.

# Sigui 'G' a la distribució de 't'. Fixem-nos que G depèn o pot dependre de
# diverses coses com ara la forma concreta de t, les mides mostrals, n1 i n2, i,
# especialment, de la distribució de la qual podem assumir que procedeixen les dades,
# diguem-ne F. La forma de l'estadístic i les mides mostrals són conegudes, però F no
# (o com a mínim no completament, potser sabem p.e. que és normal, però en desconeixem
# el valor dels paràmetres). En condicions generals F pot ser una parella de distribucions, 
# FD i FP, ja que pot donar-se el cas que sigui diferent per cada grup.
# Si assumim que tant FD com FP corresponen a la normal i que, a més, tenen la mateixa
# variància, és ben conegut que G = G(t, n1, n2, FD, FP) és una t de Student amb
# n1 + n2 - 2 graus de llibertat i paràmetre de no centralitat 'delta'. Sota la típica
# hipòtesi nul·la que delta = 0, tenim la distribució t centrada en el valor 0.
# Però quina és la distribució de t sota altres formes de FD i FP, o senzillament si
# no volem suposar cap forma concreta per aquestes distribucions?

# La idea central del mètode bootstrap és substituir a G = G(F) la distribució de les dades,
# F, desconeguda, per una estimació obtinguda a partir de les pròpies dades. Encara que
# la idea és d'aplicabilitat molt general, la il·lustrarem pel cas del nostre exemple
# concret: a G(t, n1, n2, FD, FP) podríem substituir FD i FP per les corresponents
# distribucions empíriques o mostrals, FDn1 i FPn2, que en són una bona estimació no
# paramètrica. Teòricament, per diversos teoremes de convergència bastant generals, 
# atès que FDn1 i FPn2 convergeixen cap a FD i FP, G(t, n1, n2, FDn1, FPn2) és una bastant
# bona aproximació de G. Aquesta idea és senzilla, el seu problema principal és que no
# hi ha manera analítica de deduir la fórmula de G(t, n1, n2, FDn1, FPn2). 
# Als anys 80 del segle XX, Bradley Efron va proposar que, de la mateixa manera que la
# distribució de t la podem aproximar molt bé per simulació si especifiquem completament
# els models teòrics FD i FP, G(t, n1, n2, FDn1, FPn2) es pot aproximar simulant un nombre
# gran, B, de mostres obtingudes a partir de FDn1 i FPn2 (ara completament conegudes), 
# sobre cada una d'aquestes mostres calculant l'estadístic (t) i aproximant
# G(t, n1, n2, FDn1, FPn2) (que al seu torn aproxima G(t, n1, n2, FD, FP)) a partir
# d'aquesta gran mostra de B valors de t.

# ..............................................................................
# Bootstrap en acció:

# Obtenció d'una mostra a partir de FDn1:
# Cal obtenir una nova mostra extraient amb probabilitat 1/n1 els valors de la mostra
# original de D. Això és equivalent a agafar a l'atzar i amb reemplaçament n1 valors
# de la mostra original (es parla d'una "remostra" de la mostra original):

# Mostra original:
auc

# Inicialitzem la seqüència aleatòria de R en un punt donat, per a que sigui repetible
# la simulació que farem:
set.seed(123)

# Les n1 = 11 primeres dades de 'auc' provenen d'una determinada situació experimental
# (administrar "D"), per tant les simulem per separat:
# Remostra dins D:
aucD.boot = sample(auc[1:n1], replace = TRUE)
aucD.boot

# Si tot just abans d'aquest 'sample' hem fet 'set.seed(123)' veiem que la remostra
# s'ha format agafant a l'atzar, en primer lloc, l'element quart de la mostra original,
# a continuació el novè, etc. Veiem també que estem agafant elements a l'atzar amb
# reemplaçament, per exemple a les posicions quarta i vuitena de la remostra observem
# que s'ha repetit l'element desè de la mostra original, mentre que alguna dada de la
# mostra original, com la segona, no apareix a la remostra

# Les n2 = 11 darreres dades de 'auc' foren obtingudes administrant "P", també les
# simulem per separat:
# Remostra dins P:
aucP.boot = sample(auc[(n1+1):N], replace = TRUE)
aucP.boot

# Remostra global:
c(aucD.boot, aucP.boot)

# Es poden generar directament remostres bootstrap de la mostra com abans,
# o bé generar els índexos dels elements que participaran a les remostres:
set.seed(123)
indx1 = sample(n1, replace = TRUE)
indx1
indx2 = sample((n1+1):N, replace = TRUE)
indx2
# O igual però més ràpid:
set.seed(123)
indx1 = sample.int(n1, replace = TRUE)
indx2 = sample.int(n2, replace = TRUE) + n1

# remostra completa:
c(auc[indx1], auc[indx2])
# o bé:
auc[c(indx1, indx2)]

# Càlcul de l'estadístic t sobre cada remostra:

# Primer necessitem una versió més general de la funció 'tStat'
# (serveix tant pel test de permutacions com pel bootstrap):
tStat <- function(indexs1, indexs2 = -indexs1, vector.dades, 
  mu = 0, var.equal = TRUE) 
{
  t.test(vector.dades[indexs1], vector.dades[indexs2], 
         mu = mu, var.equal = var.equal)$statistic
}

# prova:
deltaEstim <- mean(auc[1:n1]) - mean (auc[(n1+1):N])
tStat(indx1, indx2, auc, mu = deltaEstim)


B = 10000

# Generació de B remostres i càlcul de t* sobre cada una:
set.seed(123)

tBoots = replicate(B,
  tStat(
    sample.int(n1, replace = TRUE), sample.int(n2, replace = TRUE) + n1,
    auc,
    mu = deltaEstim
  )
)

# Els primers 20 dels B valors bootstrap de t:
tBoots[1:20]

# Histograma dels B valors bootstrap,
# Obrim una nova finestra gràfica:
windows(14, 14)
hist(tBoots, breaks=70, freq=F, xlim=c(-5,5), ylim=c(0,0.50))

# El comparem amb la t(n1 + n2 - 2):
rang.t <- seq(from=-5, to=+5, by=0.1)
dens.t <- dt(rang.t, df = N - 2)
lines(rang.t, dens.t, type="l", col="red", ylim=c(0,0.50))

# La forma de l'aproximació bootstrap a la distribució de l'estadístic t
# és molt similar a la t de Student amb N - 2 = 20 g.d.ll.
# Probablement la distribució de la qual procedeix la mostra 'auc' és
# en gran mesura assimilable a una normal.
# Per altres formes de distribució de les dades,
# previsiblement la distribució dels valors bootstrap seria una altra
# (i segurament més correcta que considerar que la distribució mostral és
# la t de Student)


# Taula de valors crítics t obtinguda a partir de la mostra i el mètode bootstrap:
taulaBoot = quantile(tBoots, 
  probs = c(0.01, 0.025, 0.05, 0.1, 0.15, 0.20, 0.80, 0.85, 0.90, 0.95, 0.975, 0.99)
)
taulaBoot

# Els comparem amb els valors crítics "teòrics" si fos cert que la distribució mostral
# és la t:
taula.t = qt(c(0.01, 0.025, 0.05, 0.1, 0.15, 0.20, 0.80, 0.85, 0.90, 0.95, 0.975, 0.99), df = N - 2)
names(taula.t) = names(taulaBoot)
taula.t

# Interval de confiança "bootstrap-t" (o "bootstrap estudentitzat"):

# L'interval de confiança 95% paramètric sota normalitat i homogeneïtat de variàncies que
# obtindríem a partir del mètode del pivot tindria la forma:
#      [delta estimada - t0.975(20) * s * sqrt(1/n1 + 1/n2), 
#       delta estimada - t0.025(20) * s * sqrt(1/n1 + 1/n2)]
# on 's' és l'estima comú de la desviació estàndard de les dades.
t.test(auc[1:n1], auc[(n1+1):N], var.equal = TRUE)$conf.int

# Estimació de la sigma comú:
s = sqrt(
  (sum((auc[1:n1] - mean(auc[1:n1]))^2) + sum((auc[(n1+1):N] - mean(auc[(n1+1):N]))^2)) / 
  (N - 2)
)

# Interval de confiança "bootstrap-t" (o "bootstrap estudentitzat"):
pConf = 0.95
pCua = (1 - pConf) / 2  # probabilitat a cada cua de la distribució

#      [delta estimada - taulaBoot(0.975) * s * sqrt(1/n1 + 1/n2), 
#       delta estimada - taulaBoot(0.025) * s * sqrt(1/n1 + 1/n2)]

icBoot = deltaEstim - quantile(tBoots, probs = c(1-pCua, pCua)) * s * sqrt(1/n1 + 1/n2)
names(icBoot) <- NULL
icBoot

# Comparem-lo amb l'interval t de Student paramètric-normal:
icStudent = deltaEstim - qt(c(1-pCua, pCua), N - 2) * s * sqrt(1/n1 + 1/n2)
icStudent
# o, equivalentment:
t.test(auc[1:n1], auc[(n1+1):N], var.equal = TRUE)$conf.int

# Fixem-nos que l'única diferència està en els valors crítics utilitzats:
# taula teòrica de la t de Student pel mètode paramètric:
qt(c(1-pCua, pCua), N - 2)
# taula empírica obtinguda per simulació bootstrap:
quantile(tBoots, probs = c(1-pCua, pCua))


# Tant a un interval com a l'altre hem deixat la mateixa probabilitat a cada
# cua de la distribució: 0.025. Així el nivell de confiança global és (com a
# mínim teòricament) de 1 - 2 * 0.025 = 0.95.
# Això té plenament sentit per l'interval paramètric basat en la distribució
# t de Student, que és simètrica. Fixem-nos que així l'interval t de Student
# és perfectament simètric al voltant de l'estimació puntual
deltaEstim
deltaEstim - qt(c(1-pCua, pCua), N - 2) * s * sqrt(1/n1 + 1/n2)
# Això fa que sigui un interval de llargada mitjana mínima.

# En canvi, l'interval bootstrap no és simètric al voltant de l'estimació
# puntual:
deltaEstim
icBoot

# Potser en realitat la veritable distribució mostral de l'estadístic t que ens
# està estimant la distribució bootstrap no és simètrica. En aquest cas,
# possiblement, no hi ha cap raó per deixar la mateixa probabilitat a cada cua.
#
# Un interval bootstrap que pot resultar més òptim pot ser un interval que sigui
# realment simètric al voltant de l'estimació puntual, encara que no deixi la
# mateixa probabilitat a cada cua.

# Interval de confiança "boostrap-t simetritzat":

# Valors crítics -t, +t simètrics al voltant de zero, que abarquin un 95% dels
# valors bootstrap:
tBoot = quantile(abs(tBoots), probs = pConf)
# Interval de confiança bootstrap simetritzat (simètric al voltant de l'estimació):
icBoot.sim = deltaEstim - c(tBoot, -tBoot) * s * sqrt(1/n1 + 1/n2)
names(icBoot.sim) <- NULL
icBoot.sim

# Com podeu comprovar, aquest interval "simetritzat" resulta més similar a
# l'interval paramètric t de Student:
icStudent

# Intervals de confiança i tests d'hipòtesis
# ==========================================
# Recordeu la dualitat entre intervals de confiança i tests d'hipòtesis: per una
# hipòtesi nul·la que afirmi que determinat paràmetre 'theta' val 'theta0' 
# (enfrontada a la hipòtesi alternativa que afirma que és diferent de 'theta0')
# si podem construir un interval de confiança bilateral per 'theta' amb nivell
# de confiança 'pConf', el fet de rebutjar la hipòtesi nul·la si l'interval de
# confiança NO abarca 'theta0' defineix un test de nivell 1 - pConf.
# La implicació contrària també és certa (de test a interval de confiança) però
# la considerarem més endavant.

# Tant l'interval paramètric t de Student com el bootstrap-t simetritzat
# (de la mateixa manera que la prova de Mann-Whitney, el test de permutacions
# i la prova de Kolmogorov-Smirnov) concorden en la seva decisió en un test
# bilateral: cap d'ells rebutja H0 : muD = muP (equivalent a H0 :  delta = 0)
# ja que el valor 0 cau dins l'interval de confiança. En canvi, l'interval
# bootstrap-t (quan no s'especifica, s'acostuma a presuposar que és el "no
# simetritzat", el primer que hem considerat) rebutjaria H0 ja que no inclou
# el valor delta = 0.
# Què seria més correcte? Possiblement la decisió a partir del bootstrap-t
# simetritzat. Consideracions teòriques porten a que, en general, el nivell de
# confiança de l'interval bootstrap-t simetritzat és més correcte (el seu nivell
# de confiança real s'assembla més al nominal, el que en teoria hem fixat, per
# exemple 0.95) de manera que també tenim garantia que el nivell de significació
# real del test corresponent és més correcte (més semblant al que teòricament
# hem fixat, p.e. 0.05).

# Intervals de confiança bootstrap unilaterals
# ============================================
# La relació entre contrastos d'hipòtesis i intervals de confiança també s'estén
# al cas que la hipòtesi alternativa és unilateral.
# Per exemple si H1 :  muD > muP (o bé delta > 0) hauríem de construir un
# interval de confiança unilateral [límit1, +infinit). Si el valor 0
# no està inclòs en aquest interval, podem rebutjar H0. (Intuïtivament, hauríem
# establert alguna cosa com: "amb gran seguretat (95%) el valor de delta no pot
# ser inferior a 'limit1'"; atès que 0 encara és més petit que 'limit1',
# descartem 0 com a valor admissible de 'delta'.)

# A partir de la distribució bootstrap, aquest interval seria (ara, tota la
# probabilitat la tenim concentrada en una sola cua):
icBoot.unilat = c(deltaEstim - quantile(tBoots, probs = pConf) * s * sqrt(1/n1 + 1/n2), Inf)
icBoot.unilat
# de manera que rebutjaríem H0, de la mateixa manera que ho faríem amb el test
# i l'interval de confiança paramètric t de Student (i amb la prova de
# Mann-Whitney, el test de permutacions i la prova de Kolmogorov-Smirnov) 
t.test(auc[1:n1], auc[(n1+1):N], var.equal = TRUE, alternative = "greater")$conf.int

# El corresponent interval de confiança bootstrap-t unilateral
# (-infinit, límit superior] seria:
c(-Inf, deltaEstim - quantile(tBoots, probs = 1 - pConf) * s * sqrt(1/n1 + 1/n2))

# Comparem-lo amb el corresponent interval paramètric t de Student:
t.test(auc[1:n1], auc[(n1+1):N], var.equal = TRUE, alternative = "less")$conf.int

# ******************************************************************************
#                 Construcció directa de tests bootstrap
# ******************************************************************************
# Tal com hem vist, una possibilitat per fer proves d'hipòtesis bootstrap és
# a través dels intervals de confiança. Una altra possibilitat és calcular
# directament un p-valor.
# Recordem que:
# p-valor = P{estadístic més extrem que el valor observat, si H0 certa}
# Si fem servir l'estadístic t, pel cas bilateral seria:
# P{|t| >= |t.observat| | H0 certa}. 
# Es podria fer una simulació bootstrap i observar la freqüència de vegades
# que |t.bootstrap| >= |t.observat| generant les remostres bootstrap de manera
# que reflecteixin la situació de H0.

# Hi ha diverses maneres possibles de realitzar la simulació bootstrap,
# tot generant les remostres sota el supòsit "H0 certa"

# Hipòtesi nul·la de total igualtat de les distribucions:
# =======================================================
# Si suposem que H0 és certa (FD == FP), fixem-nos que una estima adequada de
# la distribució de les dades és la distribució empírica comú, de totes les 
# N = n1 + n2 dades.
# Per tant, una remostra bootstrap (no parmètric) sota H0 vindria donada per:
sample(auc)
# O bé:
indxs.boot = sample.int(N)
auc[indxs.boot]
# O si volem aprofitar la funció tTest anterior:
indxs1 = 1:n1
indxs2 = (n1+1):N
tStat( indxs.boot[indxs1], indxs.boot[indxs2], auc, mu = 0)

# B valors de t* sota H0:
set.seed(123)
t0 = replicate(B,
  {
    indxs.boot = sample.int(N)
    tStat(indxs.boot[indxs1], indxs.boot[indxs2], auc, mu = 0)
  }
)

# P-valor per la hipòtesi alternativa bilateral:
(sum(abs(t0) >= abs(t.obs)) + 1) / (B + 1)
# No rebutgem la hipòtesi nul·la

# P-valor per la hipòtesi alternativa muD > muP:
(sum(t0 >= t.obs) + 1) / (B + 1)

# Adaptació de la distribució mostral de l'estadístic de test a H0
# ================================================================
# De fet, tal com hem fet el remostratge bootstrap abans, es podria considerar
# que ja simulàvem d'acord amb H0 certa ja que recordeu que calculàvem 
# t* = ("mitjana de remostra*" - "mitjana de mostra") / 
#      "error estàndard de mitjana de remostra*"
# és a dir, les mitjanes de les remostres les centràvem, el numerador té
# hipotèticament mitjana nul·la

# Per tant, una estima bootstrap raonable del p-valor bilateral seria:
# Estadístic t observat sobre la mostra real:
t.obs = t.test(auc[1:n1], auc[(n1+1):N], var.equal = TRUE)$statistic
# Estima bootstrap del p-valor associat:
(sum(abs(tBoots) >= abs(t.obs)) + 1) / (B + 1) # bilateral
(sum(tBoots >= t.obs) + 1) / (B + 1)           # unilateral

# De vegades, el remostratge bootstrap no reflecteix tant bé la distribució
# de l'estadístic sota H0 i cal recòrrer a diverses estratègies, com
# modificar d'alguna manera el procés de remostratge

# Remostreig sobre una mostra "adaptada" a  H0:
# =============================================
# Una possibilitat: si muD = muP, a nivell de la mostra s'hauria de reflectir
# en que la mitjana mostral de D i la de P són iguals. "Recol·loquem" les
# mostres de manera que tinguin una mitjana comú.

# Mitjana comú:
mComu = mean(auc)
# Cada submostra "recol·locada"
auc0 = auc
auc0[1:n1] = auc0[1:n1] - mean(auc0[1:n1]) + mComu
auc0[(n1+1):N] = auc0[(n1+1):N] - mean(auc0[(n1+1):N]) + mComu

# (Ara mean(auc0[1:n1]) == mean(auc0[(n1+1):N]) == mComu)

set.seed(123)
t0 = replicate(B,
  tStat(
    sample.int(n1, replace = TRUE), sample.int(n2, replace = TRUE) + n1, auc0,
    mu = 0
  )
)

# P-valor per la hipòtesi alternativa bilateral:
(sum(abs(t0) >= abs(t.obs)) + 1) / (B + 1)
# No rebutgem la hipòtesi nul·la

# P-valor per la hipòtesi alternativa muD > muP:
(sum(t0 >= t.obs) + 1) / (B + 1)
# Rebutgem la hipòtesi nul·la

# Veiem que en realitat els p-valors són exactament iguals,
# al generar 'tBoots' ja centràvem els valors t, en realitat és equivalent

# El que decididament NO simularia H0 seria:
set.seed(123)
tBoots0 = replicate(B,
  tStat(
    sample.int(n1, replace = TRUE), sample.int(n2, replace = TRUE) + n1, auc,
    mu = 0
  )
)

(sum(tBoots0 >= t.obs) + 1) / (B + 1)

# El punt clau és que a la simulació bootstrap inicial posàvem 'mu = deltaEstim'
# la qual cosa permetia centrar les remostres, mentre que ara hem posat 'mu = 0'

# Si suposem que "H0 certa" solament modifica la localització de
# la distribució anterior, i no la seva forma, té sentit fer:
(sum((tBoots0 - mean(tBoots0)) >= t.obs) + 1) / (B + 1)
# ja que sota H0 hem d'esperar que la distribució mostral de tBoots0 ha de
# tenir mitjana 0.
# Veiem però que el p-valor estimat no és exactament l'obtingut en aproximacions
# bootstrap anteriors



