# Taller 0

# PASSATGERS LÍNIES AÈRIES

# Carregar i representar dades

pasa<-ts(read.table("E:/TEMA4/passatgers.txt"),start=1978.1,frequency=12)

plot.ts(pasa,col=4)

# Funció d'autocorrelació i autocorrelació parcial

par(mfrow=c(2,1))
acf(pasa,ylim=c(-1,1),lag=100)
pacf(pasa,ylim=c(-1,1),lag=100)

# Diferència regular

dpasa<-diff(pasa)
par(mfrow=c(1,1))
plot.ts(dpasa,col=4)

par(mfrow=c(2,1))
acf(dpasa,ylim=c(-1,1),lag=100)
pacf(dpasa,ylim=c(-1,1),lag=100)

# Diferència estacional

d12pasa<-diff(pasa,lag=12)

par(mfrow=c(1,1))
plot.ts(d12pasa,col=4)

par(mfrow=c(2,1))
acf(d12pasa,ylim=c(-1,1))
pacf(d12pasa,ylim=c(-1,1))

