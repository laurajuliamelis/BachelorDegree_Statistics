# ANÀLISI DE SÈRIES TEMPORALS


# IPI

ipi<-ts(read.table("ipi.dat"),start=1960,frequency=4)
plot.ts(ipi,col=c(4))
acf(ipi)
pacf(ipi)

model<-lm(ipi~c(1:length(ipi)))
residus<-ts(residuals(model),start=1960,frequency=4)
plot.ts(residus)

acf(residus)
pacf(residus)

# PIB

pib<-ts(read.table("pib.dat"),start=1980,frequency=4)
plot.ts(pib,col=c(4))

model<-lm(pib~c(1:length(pib)))
residus<-ts(residuals(model),start=1980,frequency=4)
plot.ts(residus)

acf(residus)
pacf(residus)

dpib<-diff(pib)
plot(dpib)

acf(dpib)
pacf(dpib)

# passatgers

pasa<-ts(read.table("passatgers.dat"),start=1978,frequency=12)
plot.ts(pasa,col=c(4))
lpasa<-log(pasa)
plot(lpasa)

dlpasa<-diff(lpasa)
plot(dlpasa)
acf(dlpasa)
pacf(dlpasa)

d12lpasa<-diff(lpasa,lag=12)
plot(d12lpasa)
acf(d12lpasa)
pacf(d12lpasa)



# EXERCICI 2

epsilon<-ts(rnorm(120),start=1900,frequency=1)

plot(epsilon)
acf(epsilon,ylim=c(-1,1))
pacf(epsilon,ylim=c(-1,1))

x<-filter(epsilon,0.5,method="recursive")
plot.ts(x,type="l")

acf(x,ylim=c(-1,1))
pacf(x,ylim=c(-1,1))

