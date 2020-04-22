# ANÀLISI DE SÈRIES TEMPORALS


# IPI

ipi<-ts(read.table("E:/ipi.txt"),start=1960,frequency=12)
par(mfrow=c(1,1))
plot.ts(ipi,col=c(4))

par(mfrow=c(2,1))
acf(ipi,ylim=c(-1,1),lag=100)
pacf(ipi,ylim=c(-1,1),lag=100)

# Té tendència: no estacionari

dipi<-diff(ipi)
par(mfrow=c(1,1))
plot.ts(dipi,col=4)

# Hem eliminat tendència... però variabilitat? Agafar log?

lipi<-log(ipi)
par(mfrow=c(1,1))
plot.ts(lipi,col=4)

dlipi<-diff(lipi)
par(mfrow=c(1,1))
plot.ts(dlipi,col=4)

# Hem solucionat el problema d'estacionarietat?

par(mfrow=c(2,1))
acf(dlipi,ylim=c(-1,1),lag=24)
pacf(dlipi,ylim=c(-1,1),lag=24)

# Provem models: ARIMA(1,1,0), ARIMA(0,1,1) i ARIMA(1,1,1)

model<-arima(ipi,c(1,1,1))
model

# Comencem la validació dels models: Anàlisi residus

par(mfrow=c(2,1))
acf(model$residuals,ylim=c(-1,1),lag=100)
pacf(model$residuals,ylim=c(-1,1),lag=100)



# PIB

pib<-ts(read.table("E:/pib.txt"),start=1980,frequency=4)
plot.ts(pib,col=c(4))

# No estacionari: té tendència

dpib<-diff(pib)
par(mfrow=c(1,1))
plot.ts(dpib,col=4)

# Encara té tendència

ddpib<-diff(dpib)
par(mfrow=c(1,1))
plot.ts(ddpib,col=4)

# Problemes de variabilitat al mig?

par(mfrow=c(2,1))
acf(ddpib,ylim=c(-1,1),lag=100)
pacf(ddpib,ylim=c(-1,1),lag=100)

# Sembla que hi ha estacionalitat!!!! No ho resolem perquè a l'enunciat ens diuen que s'ha eliminat...

# Per tant, mirem només la part regular:

par(mfrow=c(2,1))
acf(ddpib,ylim=c(-1,1),lag=8)
pacf(ddpib,ylim=c(-1,1),lag=8)

# A la part regular sembla que hi ha un decreixement ràpid de la FAS, per tant no cal agafar logaritmes per solucionar els possibles problemes de variabilitat "al mig".

# Sembla un MA(1)

model<-arima(dpib,c(0,2,1))
model

# Anàlisi residus

par(mfrow=c(2,1))
acf(model$residuals,ylim=c(-1,1),lag=100)
pacf(model$residuals,ylim=c(-1,1),lag=100)



# PASSATGERS LÍNIES AÈRIES

pasa<-ts(read.table("E:/passatgers.txt"),start=1978.1,frequency=12)
plot.ts(pasa,col=4)

# Hi ha diferència de variabilitat: apliquem logaritme

lpasa<-log(pasa)
par(mfrow=c(1,1))
plot.ts(lpasa,col=4)

# Solucionem la tendència agafant diferències

dlpasa<-diff(lpasa)
par(mfrow=c(1,1))
plot.ts(dlpasa,col=4)

par(mfrow=c(2,1))
acf(dlpasa,ylim=c(-1,1),lag=100)
pacf(dlpasa,ylim=c(-1,1),lag=100)

# Clarament hi ha estacionalitat!!! A diferència de l'apartat anterior, ara sí que ho resoldrem: agafem diferències estacionals?

d12dlpasa<-diff(dlpasa,lag=12)
par(mfrow=c(1,1))
plot.ts(d12dlpasa,col=4)

par(mfrow=c(2,1))
acf(d12dlpasa,ylim=c(-1,1),lag=100)
pacf(d12dlpasa,ylim=c(-1,1),lag=100)

# Fixem-nos només en la part regular (tal i com demana l'exercici)

par(mfrow=c(2,1))
acf(d12dlpasa,ylim=c(-1,1),lag=12)
pacf(d12dlpasa,ylim=c(-1,1),lag=12)

# Clarament és un MA(1)

# Com que hi havia estacionalitat...

model<-arima(lpasa,order = c(0,1,1),seasonal = list (order = c(0,1,0)))
model

# Anàlisi residus

par(mfrow=c(2,1))
acf(model$residuals,ylim=c(-1,1),lag=100)
pacf(model$residuals,ylim=c(-1,1),lag=100)

# A part de les diferències estacionals hem de fer més coses amb la part estacional...

# Tornem a mirar la FAS i la FAP

par(mfrow=c(2,1))
acf(d12dlpasa,ylim=c(-1,1),lag=100)
pacf(d12dlpasa,ylim=c(-1,1),lag=100)

# La part estacional sembla un altre MA(1)

# Tornem a estimar el model

model<-arima(lpasa,order = c(0,1,1),seasonal = list (order = c(0,1,1)))
model

# ... i tornem a mirar els residus

par(mfrow=c(2,1))
acf(model$residuals,ylim=c(-1,1),lag=100)
pacf(model$residuals,ylim=c(-1,1),lag=100)


