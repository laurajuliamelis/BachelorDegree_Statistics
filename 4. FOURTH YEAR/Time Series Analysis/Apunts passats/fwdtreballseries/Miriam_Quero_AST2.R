library(MASS)
library(forecast)
library(TSA)

s<-ts(read.table("F:/A. Series Temporals/Base monetaria.txt"),start=2007,frequency=12)
#Análisis descriptivo
summary(s)

#Peridoo muestral:
sm<-s[-c(61:72)]
sme<-s[c(61:72)]

#Primer análisis gráfico
plot.ts(sm)
#Hay tendencia

par(mfrow=c(2,1))
#Función de autocorrelación
acf(sm,lag=100)
#Función de autocorrelación parcial
pacf(sm, lag=100)
#Shapiro-Wil para comprobar normalidad
shapiro.test(sm)


#Hacemos diferenciación para arreglar la tendencia

dsm<-diff(sm)
dsm

#Miramos de nuevo los gráficos para ver que problemas se nos han solucionado
par(mfrow=c(1,1))
plot.ts(dsm)
#ahora son hortizontales

par(mfrow=c(2,1))
acf(dsm,lag=100)
#no hay patrones de conducta
pacf(dsm, lag=100)
#tiende a 0 rapido
shapiro.test(dsm)
#Normalidad Bien

#Comprobamos estacionariedad
adf.test(dsm)
#Estacionariedad Bien

auto.arima(sm,d=1,D=1)#Pasamos la serie original e indicamos que 
#nos la diferencia (realizado a mano anteriormente) (d=1), y 
#D=1 para indicar que si que hay estacionariedad


#Búsqueda de mejor modelo 
m1<-arima(dsm,order=c(1,0,1))   
m1
pnorm(c(abs(m1$coef)/sqrt(diag(m1$var.coef))), mean = 0,sd=1,lower.tail = F)

m2<-arima(dsm,order=c(1,0,2)) 
m2
pnorm(c(abs(m2$coef)/sqrt(diag(m2$var.coef))), mean = 0,sd=1,lower.tail = F)

m3<-arima(dsm,order=c(2,0,1)) 
m3
pnorm(c(abs(m3$coef)/sqrt(diag(m3$var.coef))), mean = 0,sd=1,lower.tail = F)

m4<-arima(dsm,order=c(0,0,1))
m4
pnorm(c(abs(m4$coef)/sqrt(diag(m4$var.coef))), mean = 0,sd=1,lower.tail = F)

m5<-arima(dsm,order=c(0,0,2))
m5
pnorm(c(abs(m5$coef)/sqrt(diag(m5$var.coef))), mean = 0,sd=1,lower.tail = F)

m6<-arima(dsm,order=c(1,0,0))
m6
pnorm(c(abs(m6$coef)/sqrt(diag(m6$var.coef))), mean = 0,sd=1,lower.tail = F)

m7<-arima(dsm,order=c(2,0,0))
m7
pnorm(c(abs(m7$coef)/sqrt(diag(m7$var.coef))), mean = 0,sd=1,lower.tail = F)


#validacion modeo
#normalidad residuos
shapiro.test(m2$residuals)
#independencia residuos
Box.test(m2$residuals)

#Predicciones
pr<-predict(m2,n.ahead=12)$pred
pr

#Gráfico de las predicciones
plot(pr)
par(mfrow=c(1,1))

#Predicciones y valores diferencia
ts.plot(dsm,m2$residuals,col=c(1,2))

#Periodo muestral y expremuestral con predicciones
prd<-tail(sm,12)-pr
prd

plot(prd, type="l", xlab="meses", ylab="predicción")

plot(c(sm,sme),type="l", ylab=" ", main=" ")
lines(prd,type="l",x=c(61:72),col="red")

# Capacitat predictiva
errors<-sme[1:12]-prd
ts.plot(errors,col=2,main="Errores",xlab="Mes")
#Error cuadratico medio
eqm<-sum(errors*errors)/12 

#Error absoluto medio
eam<-sum(abs(errors))/12 
#EPAM
epam<-sum(abs(errors)/sme[1:12])/12 

