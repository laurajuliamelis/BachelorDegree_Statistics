# Taller 0

# PASSATGERS L???NIES A???RIES

# Carregar i representar dades

pasa<-ts(read.table("passatgers.txt"),start=1978.1,frequency=12) #frequency=12 (dades mensuals)

#1r pas per tenir evidència si la serie es estacionaria
#clarament té tendendncia i la varialitat no és constant --> la series no és estacionaria
#per tant, no podem calcular FAS i FAP (ho calculem igualment)

plot.ts(pasa,col=4)

# Funci??? d'autocorrelaci??? i autocorrelaci??? parcial

#aquest grafics son les fotografies --> tenim que comprovar si s'assemblen a les teòriques
par(mfrow=c(2,1))
acf(pasa,ylim=c(-1,1),lag=100) #FAS lag=100 pk estem calculant 100 valors
pacf(pasa,ylim=c(-1,1),lag=100) #FAP

#hem de fer transformacions per que la serie es comporti com una sèrie estacionaria

# Difer???ncia regular

dpasa<-diff(pasa) #per solucionar que té tendència (dif de cada una amb l'anterior)
par(mfrow=c(1,1))
plot.ts(dpasa,col=4)

#ara tenim aquestes fotografies (encara no les podem comparar amb el teòric)
par(mfrow=c(2,1))
acf(dpasa,ylim=c(-1,1),lag=100)
pacf(dpasa,ylim=c(-1,1),lag=100)

# Difer???ncia estacional

d12pasa<-diff(pasa,lag=12) #diferencia d'un agost amb un agost, d'un juliol amb un juliol...

par(mfrow=c(1,1))
plot.ts(d12pasa,col=4) #no sembla que haguem eliminat la tendencia i la varibalitat ens continua aefectant

par(mfrow=c(2,1))
acf(d12pasa,ylim=c(-1,1))
pacf(d12pasa,ylim=c(-1,1))

#funcio d'autlocorrelacio decreixi de forma rapida símptoma de que la serie s'està comportant de forma estacionaria

lpassa <- log(pasa)
par(mfrow=c(1,1))
plot.ts(lpassa, col=4)

#aplicant logaritme corregim els problemes de variancia
#ara per solucionar la tendencia agafem diferenecies sobre el logaritme de la variable

# Difer???ncia regular

dlpasa<-diff(lpassa) #per solucionar que té tendència (dif de cada una amb l'anterior)
par(mfrow=c(1,1))
plot.ts(dlpasa,col=4)

par(mfrow=c(2,1))
acf(dlpasa,ylim=c(-1,1))
pacf(dlpasa,ylim=c(-1,1))

# Difer???ncia estacional

dl12pasa<-diff(dlpassa,lag=12) #no sabem si es de lpasa o dlpassa

par(mfrow=c(1,1))
plot.ts(dl12pasa,col=4) #no sembla que haguem eliminat la tendencia i la varibalitat ens continua aefectant

par(mfrow=c(2,1))
acf(dl12pasa,ylim=c(-1,1))
pacf(dl12pasa,ylim=c(-1,1))

#amb la transformació logaritmica ens hem passat de frenada, hauriem de aplicar altres com la box-cox
