vendedores <- c(5,6,5,4,3,4,7,6,5,8)
coches <- c(10,20,18,10,7,14,21,15,13,22)

plot(vendedores,coches)

g <- lm(coches ~ vendedores)
summary(g)
confint(g)

# per cada unitat de la variable explicativa el coef (2.9851) representa les unitats de la var resposta que tinc

# intercept = -0.8209 no te cap explicacio
# quan els valors de la var explicativa siguin posistius i negatius -> el beta0 és la resposta al 0

# predicció per 7 venedors

coef(g)[1] + coef(g)[2] * 7
# un concseccionari amb 7 venedors ha de vendre 20.07 cotxes

# Com es fa directament? -> funcio predict calcula prediccions

predict(g)

predict(g, newdata = data.frame(vendedores=7), interval="none")
predict(g, newdata = data.frame(vendedores=7), interval="confidence") #interval reduit calculat sobre una mitjana
  #prediccio mitjana sobre la poblacio de concessionaris que tenen 7 venedors
predict(g, newdata = data.frame(vendedores=7), interval="prediction") #és més ample, interval sobre una prediccio concreta
  #

# diferencia entre
# el primer corrrespon al interval de prediccio quan es calcula la mitjana de les possibles prediccions 
# (un interval sobre la mitjana sempre és més estret que no si es fa sobre una predicció)
# intervé no nomes la prediccio sino també el fet que la variabilitat aumenta

install.packages("UsingR")
library(UsingR)
simple.lm(vendedores,coches,show.ci=T) #s'interpreta en vertical. Per cada valor de x tinc un interval de confianza
# a mesura que ens apropem a la mitjana de la x, el interval és més estret