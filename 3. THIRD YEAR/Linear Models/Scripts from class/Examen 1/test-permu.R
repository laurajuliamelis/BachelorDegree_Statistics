longitud <- c(0.92,1.29,1.0,1.5,1.25,1.54,1.26,1.71,1.28,1.16,1.43,1.8,1.5,1.57,1.75)
poblacio <- c(rep("a",9),rep("f",6))  # = rep(c("a","f"),c(9,6))

# a:poblacio especie actual, f:poblacio especie fossil
# es tracta de comparar la longitud d'aquestes dues poblacions
# normalment per comparar mitjanes fariem servir la t-student
# problemes t-student: pob ha d provenir de la normal, les vars han de ser iguals

# modificacio de welch (alternativa t-student quan no hi ha igualtat de variance pero si normalitat)

# comprobar si la longitud es normal es complicat perque n=9. Quan n es petita s'aconsella 
# utilitzar el test kolmorov spirlov (tot i que tampoc es pot estar segur)

# APLICAREM EL TEST DE PERMUTACIONS (dubten de la igualtat de variancies, sobretot, i també de la normalitat)
# aquest test no necessita cap hipotesis
# aquest test serà la última opció

# en cas de test no parametric -> ALTERNATIVA: test no paramètric com wilcoxon o Mann-witney
# (també requereix la igualtat de variancies)

siurana <- data.frame(longitud,poblacio)
rm(longitud,poblacio)

siurana
summary(siurana)

attach(siurana)

tapply(longitud, poblacio, mean) #el primer que hem de fer és calcular les mitjanes mostrals

mean(longitud[poblacio=="a"])
mean(longitud[poblacio=="f"])

dif.mostra <- mean(longitud[poblacio=="f"]) - mean(longitud[poblacio=="a"]) #diferència mostral de les mitjanes
dif.mostra

# Per contrastar necessitem una h0 i una h1
# h0: les mitjanes són iguals (en realitat no hi ha dues poblacions)
# h1: la mitjana de f és més gran (en realitat provenen de dues poblacions diferents)

## Permutacions de la poblacio
# -> consisteix en permutar els 15 elements i veure que passa (suposant h0 certa)
# -> de quantes maneres es pot permutar els 15 elements? 15 factorial
# -> en realitat no permuto 15, sino 9 i 5 -> llavors hi ha menys combinacions

factorial(15) # moltes mostren el mateix resultat

choose(15,6)  # = factorial(15)/(factorial(9)*factorial(6)) #choose forma en R de calcular combinacions
# 15 elements agafats de 6 en 6 (o de 9 en 9)

# Monte-Carlo -> no fa totes les permutacions possibles sino unes quantes al atzar
# permutar a l'atzar: potser que hi hagi repeticions

sample(poblacio,15,replace=F) # proveu varies vegades: permutacio al atzar

## Hipotesi nul.la: les mitjanes son iguals
## Hipotesi alternativa: la mitjana dels fossils es mes gran (és unilateral)

# Test de permutacions (Monte-Carlo) -> només fa 1000 combinacions
n<-1000
dif <- numeric(n) #vector que omplirem amb les diferències de mitjanes

for(i in 1:n){ # mil vegades calcular una permutacio i fer la diferencia de mitjanes per aquesta permutacio
permu <- sample(poblacio)
dif[i] <- mean(longitud[permu=="f"]) - mean(longitud[permu=="a"])
}
hist(dif) # forma acampanada, poblacio es comporta d'una manera força normal
          # distribucio empirica de l'estadístic diferencia de mitjanes en general
          # aquest histograma versió empírica de la distribució teòrica que jo desconec
abline(v=dif.mostra, col="blue")
# linia blava= diferencia de mitjanes que jo he trobat a la meva mostra
# Aquest valor és un valor normal o és extrem? REbutgem o no h0??
# Per decidir-ho: fem servir quantil o p-valor

quantile(dif, 0.95) # valor critic aproximat
abline(v=quantile(dif, 0.95), col="red")

### Una forma mes sofisticada de calcular el vector de diferencies
##
## un.test <- function(x) {
## permu <- sample(poblacio)
## mean(x[permu=="f"]) - mean(x[permu=="a"])
## }
##
## dif <- replicate(1000, un.test(longitud))



# dif.mostra >= quantile(dif,0.95) # TRUE = rebutjem H_0

p.valor <- sum(dif >= dif.mostra) / n
p.valor <- ( sum(dif >= dif.mostra) +1 ) / (n+1) # Aixi mai es zero (truc)
p.valor

# Test t de Student

t.test(longitud~poblacio, alt="less", var.eq=T)$p.value

# Test de permutacions (fent les 5005 combinacions possibles) -> RESULTAT MOLT MÉS EXACTE QUE MONTE-CARLO

idx <- 1:15 # = seq(along=longitud)
idxA <- combn(idx, 9) # matriu amb totes les combinacions

DifM <- function(x) {mean(longitud[!(idx %in% x)]) - mean(longitud[x])}
dif <- apply(idxA, 2, DifM)

quantile(dif, 0.95) # valor critic aproximat

# dif.mostra >= quantile(dif, 0.95) # TRUE = rebutjem H_0

p.valor <- sum(dif >= dif.mostra) / 5005
p.valor # > 0.05

# paquets amb funcions que calculen tests de permutacions:
# perm, coin, exactRankTests

library(perm) # paquet que calula alguns test de permutació
# permTS permet calcular test de permutacions per variable resposta, variable facroe

permTS(longitud ~ poblacio, alternative="less", method="exact.mc") #exact.mc metode de monte-carlo (fa 999 combinacions)
permTS(longitud ~ poblacio, alternative="less", exact=TRUE) #TRUE fa totes les combinacions possibles

detach(siurana)