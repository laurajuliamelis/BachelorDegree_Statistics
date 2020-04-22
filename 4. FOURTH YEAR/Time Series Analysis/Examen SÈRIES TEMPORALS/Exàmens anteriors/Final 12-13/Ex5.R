setwd("E:/Estadistica UB/4t Curs/1r semestre/Anàlisi de Sèries Temporals/Examen Final 12-13")

#Serie 1
par(mfrow=c(1,1))
s1<-ts(read.table("serie1.txt")) #Serie mensual
plot.ts(s1,col=c(4))

par(mfrow=c(2,1))
acf(s1,ylim=c(-1,1))
pacf(s1,ylim=c(-1,1))


#Serie 2
par(mfrow=c(1,1))
s2<-ts(read.table("serie2.txt")) #Serie mensual
plot.ts(s2,col=c(4))

par(mfrow=c(2,1))
acf(s2,ylim=c(-1,1))
pacf(s2,ylim=c(-1,1))

ds2<-diff(s2)
par(mfrow=c(1,1))
plot.ts(ds2,col=4)

par(mfrow=c(2,1))
acf(ds2,ylim=c(-1,1))
pacf(ds2,ylim=c(-1,1))


#Serie 3
par(mfrow=c(1,1))
s3<-ts(read.table("serie3.txt")) #Serie mensual
plot.ts(s3,col=c(4))

par(mfrow=c(2,1))
acf(s3,ylim=c(-1,1))
pacf(s3,ylim=c(-1,1))

#Tenemos un modelo SARIMA(0,0,0)(1,0,0)4, porque tenemos una serie 
#estacionaria (no hay que aplicar ninguna transformación a la serie original),
#y observamos como la FAS tiende exponencialmente a 0 cada 4 periodos,
#mientras que la FAP tiene un coeficiente significativo en el periodo 4.
#Por lo tanto, d=0, D=0 y P=1.

