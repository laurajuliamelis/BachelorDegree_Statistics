# ANÀLISI DE SÈRIES TEMPORALS


# IPI

ipi<-ts(read.table("E:/ipi.txt"),start=1960,frequency=4)
plot.ts(ipi,col=c(4))

par(mfrow=c(2,1))
acf(ipi,ylim=c(-1,1),lag=100)
pacf(ipi,ylim=c(-1,1),lag=100)


dipi<-diff(ipi)
par(mfrow=c(1,1))
plot.ts(dipi,col=4)

par(mfrow=c(2,1))
acf(dipi,ylim=c(-1,1),lag=100)
pacf(dipi,ylim=c(-1,1),lag=100)

model<-arima(dipi,c(2,0,0))
model

# Anàlisi residus

par(mfrow=c(2,1))
acf(model$residuals,ylim=c(-1,1),lag=100)
pacf(model$residuals,ylim=c(-1,1),lag=100)

# PIB

pib<-ts(read.table("E:/pib.txt"),start=1980,frequency=4)
plot.ts(pib,col=c(4))

dpib<-diff(pib)
par(mfrow=c(1,1))
plot.ts(dpib,col=4)

par(mfrow=c(2,1))
acf(dpib,ylim=c(-1,1),lag=100)
pacf(dpib,ylim=c(-1,1),lag=100)

model<-arima(dpib,c(3,0,0))
model

# Anàlisi residus

par(mfrow=c(2,1))
acf(model$residuals,ylim=c(-1,1),lag=100)
pacf(model$residuals,ylim=c(-1,1),lag=100)


# PASSATGERS LÍNIES AÈRIES

pasa<-ts(read.table("E:/passatgers.txt"),start=1978.1,frequency=12)

plot.ts(pasa,col=4)

lpasa<-log(pasa)
par(mfrow=c(1,1))
plot.ts(lpasa,col=4)

par(mfrow=c(2,1))
acf(lpasa,ylim=c(-1,1),lag=100)
pacf(lpasa,ylim=c(-1,1),lag=100)

dlpasa<-diff(lpasa)
par(mfrow=c(1,1))
plot.ts(dlpasa,col=4)


par(mfrow=c(2,1))
acf(dlpasa,ylim=c(-1,1),lag=100)
pacf(dlpasa,ylim=c(-1,1),lag=100)

d12dlpasa<-diff(dlpasa,lag=12)

par(mfrow=c(1,1))
plot.ts(d12dlpasa,col=4)

par(mfrow=c(2,1))
acf(d12dlpasa,ylim=c(-1,1))
pacf(d12dlpasa,ylim=c(-1,1))


model<-arima(d12dlpasa,c(2,0,0))
model

# Anàlisi residus

par(mfrow=c(2,1))
acf(model$residuals,ylim=c(-1,1),lag=100)
pacf(model$residuals,ylim=c(-1,1),lag=100)


