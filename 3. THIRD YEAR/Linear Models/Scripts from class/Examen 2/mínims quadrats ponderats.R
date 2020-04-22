# MÍNIMS QUUADRATS PONDERATS
# matriu sigma diagonal: els erros no estan correlacionats
# podriem dir que els elements de la diagonal son els pesos, pero en realitat els pessos son els inversos del que hagi a la diagonal
# diagonal: 1/w1, 1/w2,...,1/wn els pesos son els wi's i=1,2,...,n
# la matriu de cholesqui sera la mateixa diagonal pero l'arrel cuadrada. matriu de cholesqui=T(S)S

# Com es calculen aquests pesos?
# Situació 1: Si veiemn que la variablitat dels error es comporta proporcionalment a una variable regressora (x)... wi=1/xi
# Situació 2: si la yi és una mitjana, llavors sabem que var(mitjanayi)=sigma2/ni ... justament agfem com a pes ni
# Situació 3: Si coneixem la variabilitat...

install.packages("faraway")
library(faraway)
data(fpe, package="faraway")
head(fpe)

#EI: registered voters...n
#A2 B2: vots de la segona ronda
#N: diferencia entre els que van votar la primera ronda i els que voten la segona
#A2 Voters for Mitterand in the second round...variable resposta del model
#B2 Voters for party Giscard in the second round
# El model intentar esbrinar com va ser la transparencia d'error entre els vots de la primera i segona ronda

#DIFICULTATS DEL MODEL
# La variancia del vot depen molt de la mida mostral (tamany del departament) -> utilitzar com a ponderacio l'invers del numero de votants...situació 1
  #1/numero d'electors és la ponderació
# Suposar que tot aixo quadrara amb el que esperem...es suggereix no posar intercecció (beta0)
  #El beta0 no tindria interpretació
# Els beta s'han d'interpretar com proporcio de votants que passen de un candidat a un altre, els beta han de estar entre 0 i 1
  #aquesta restricció no forma part del model lineal...ja veurem si es verifica o no (un altre món Model generalitzat restringit)

lmod <- lm(A2 ~ A+B+C+D+F+G+H+J+K+N-1, fpe, weights=1/EI)
coef(lmod)
# la variancia es proporcional al tamany, pertant el pes és 1/EI
# que el betaA sigui 1.07 és logic: els que voten Mitterand a la primera també voten Mitterand a la segona
# que el betaB sigui negatiu també és lògic: els que van votar Giscard a la primera no voten Mitterand a la segona

# SIMPLIFICACIÓ DEL MODEL INICIAL
# Fem una agrupació de partits que van declarar que els seus votants havien de votar Mitterand
# G i K corroboren que tota la gent que va votar a la primera ronda G i K a la primera, van votar Mitterand a la segona
#pas 1
lmod <- lm(A2 ~ offset(A+G+K)+C+D+E+F+J+N-1, fpe, weights = 1/EI) 
coef(lmod) # com que el coeficient de J és negatiu el treiem
#pas 2
lmod <- lm(A2 ~ offset(A+G+K)+C+D+E+F+N-1, fpe, weights = 1/EI) #offset el coeficient serà 1 (no es calcula el coeficient)
coef(lmod) #el 22.58% dels votants de C van votar Mitterand 
#se suposa que ningú que va votar Giscard a la primera, va votar per Miterand a la segona, per això no posa ni B ni H

# ARRIBA EL MOMENT DE TREURE CONCLUSIONS ELECTORALS
#el 22.58% dels votants de C van votar Mitterand -> aquest va ser el que va marcar els resultats d'aquestes eleccions..
#ja que C és un partit tradicionalment de dretes

#packet mgcv
#pcls() -> per restringir que els coeficients estiguin entre 0 i 1