### Àlex Penina Aguilera ###

###Exercici 10

#Introduïm el valor dels paràmetres:
muA<-1
varA<-1
muB<-2
varB<-1
c<-0.2

#Modelitzem les variables A i B, tot simulant 100 observacions:
par(mfrow=c(1,1))
A<-ts(rnorm(100, mean=muA, sd=sqrt(varA)))
plot.ts(A,col=c(4))
B<-ts(rnorm(100, mean=muB, sd=sqrt(varB)))
plot.ts(B,col=c(4))

#Creem el procés estocàstic a partir de A i B:
X<-ts(A*cos(c(1:100)*c)+B*sin(c(1:100)*c))
plot.ts(X,col=c(4))
#Observant el gràfic del procés estocàstic, observem una espècie de cicles, que es van repetint al llarg 
#de la serie (aproximadament cada 30 observacions). Són deguts a la introducció dels elements cosinus i sinus. 

#Observem també el procés no és estacionari de segon ordre. La mitjana no es manté constant en cap moment, ja que 
#els cicles fan que aquesta augmenti i disminueixi periòdicament. La variància tampoc es manté constant, ja 
#que hi ha valors que estan situats molt distants, mentre que altres els observem relativament junts.

#Calculem la FAS i la FAP per a corroborar-ho:
par(mfrow=c(2,1))
acf(X,ylim=c(-1,1), lag=100)
pacf(X,ylim=c(-1,1), lag=100)
#Clarament observem com el procés no és estacionari. Observem una FAS que oscil·la constantment, i una 
#FAP amb molts coeficients significatius.

