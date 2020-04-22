### Àlex Penina Aguilera ###

###Exercici 9

##Sèrie A

#Indiquem quin és el nostre directori de treball:
setwd("W:/Anàlisi de Sèries Temporals")

#Introduim el fitxer amb les dades:
serieA<-ts(read.table("serie_a.txt")) 

#Fem un gràfic de la sèrie per observar com es comporta:
par(mfrow=c(1,1))
plot.ts(serieA,col=c(4))
#Es tracta d'una sèrie estacionària, tant en mitjana com en variància. 

#Ho comprovem calculant la FAS i la FAP:
par(mfrow=c(2,1))
acf(serieA,ylim=c(-1,1))
pacf(serieA,ylim=c(-1,1))
#Efectivament, observem com el comportament de la FAS i la FAP corresponen al d'una serie estacionària.
#Ens decantem per un MA(1), ja que la FAP tendeix exponencialment cap a 0, mentre que la FAS tan sols té
#un coeficient significatiu, tret del primer (no es conta perquè indica la correlació del coeficient amb 
#ell mateix, i per aixo és 1). 

#Així doncs, atès a que no hem près cap diferencia, ni hem observat cap component estacional, el model que 
#millor s'ajusta a la sèrie A és el model MA(1).





###Sèrie B

#Introduim el fitxer amb les dades:
serieB<-ts(read.table("serie_b.txt")) 

#Fem un gràfic de la sèrie per observar com es comporta:
par(mfrow=c(1,1))
plot.ts(serieB,col=c(4))
#Sembla que la sèrie sigui estacionària en mitjana, però no del tot en variància. 

#Sortirem de dubtes calculant la FAS i la FAP:
par(mfrow=c(2,1))
acf(serieB,ylim=c(-1,1))
pacf(serieB,ylim=c(-1,1))
#Observem un patró oscil·latori en la FAS, fet que ens confirma que la serie no és del tot estacionària.

#Per a arreglar-ho, prenem una diferència regular (ordre 1):
dserieB<-diff(serieB)
par(mfrow=c(1,1))
plot.ts(dserieB,col=4)
#Sembla que la sèrie resultant és estacionària tant en mitjana com en variància.

#Tornem a calcular la FAS i la FAP, però aquest cop de la sèrie diferenciada:
par(mfrow=c(2,1))
acf(dserieB,ylim=c(-1,1))
pacf(dserieB,ylim=c(-1,1))
#Observem com els coeficients no s'acaben de netejar del tot. 

#Prenem una altra diferència, aviam si ho solucionem:
ddserieB<-diff(dserieB)
par(mfrow=c(1,1))
plot.ts(ddserieB,col=4)

#Tornem a calcular la FAS i la FAP:
par(mfrow=c(2,1))
acf(ddserieB,ylim=c(-1,1))
pacf(ddserieB,ylim=c(-1,1))
#Ara sí s'observa clarament el comportament de la nostra sèrie temporal. Tant la FAS com la FAP tendeixen
#exponencialment cap a 0, fet que indica que el model adequat per a la nostra serie temporal és un ARMA(1,1).
#Donat que hem aplicat 2 diferències regulars a la sèrie original, tenim d=2. Per últim, cal recordar que 
#no hem observat cap component estacional en les dades.

#Així, el model adequat per a modelitzar la sèrie B és un ARIMA(1,2,1).




