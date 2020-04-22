####################################
###  ANÀLISI DE SÈRIES TEMPORALS ###
###          PRÀCTICA 2          ###
### Laura Julià Melis - 16810883 ###
####################################
library(MASS)
library(forecast)
library(TSA)
library(tseries)

## IMPORTACIÓ DE LA BASE DE DADES.
dades<- ts(read.table("dades.txt"))
data<-dades[-c(73:80)] #Període mostral
dataExtra<-dades[c(73:80)] #Període extra-mostral


## 2.1. TIPOLOGIA DE LA SÈRIE.
plot.ts(data,col=c(4)) # Sèrie amb tendència negativa i sense estacionalitat.

par(mfrow=c(2,1))
plot(acf(data,lag=100, plot=F), ylim=c(-1,1))
pacf(data,ylim=c(-1,1),lag=100)

## 2.2.1 OBTENCIÓ D'UN PROCÉS ESTOCÀSTIC ESTACIONARI.
## Té tendència: no és estacionari. Agafem diferències (regulars) per a solucionar-ho.
difdata<-diff(data)
par(mfrow=c(1,1))
plot.ts(difdata,col=4) # Hem eliminat tendència

# S'ha solucionat el problema d'estacionarietat?
adf.test(difdata) # Test procés estacionari: sí és estacionari.

## 2.2.2 IDENTIFICACIÓ DEL MODEL.
par(mfrow=c(2,1))
plot(acf(difdata,lag=100, plot=F), ylim=c(-1,1))
pacf(difdata,ylim=c(-1,1),lag=100)

fit <-auto.arima(difdata) # millor model ARIMA

## 2.2.3. ESTIMACIÓ I VALIDACIÓ DEL MODEL.
# Estimació del model
model<-arima(difdata,order = c(2,0,2))
model

# Anàlisi dels residus.
shapiro.test(model$residuals) # normalitat residus
Box.test(model$residuals) # independencia residus

par(mfrow=c(2,1))
plot(acf(model$residuals,lag=100), ylim=c(-1,1))
pacf(model$residuals,ylim=c(-1,1),lag=100)

# Significació dels coeficients
pnorm(c(abs(model$coef)/sqrt(diag(model$var.coef))), mean=0, sd=1, lower.tail=FALSE)

## 2.2.4. PREDICCIONS.
par(mfrow=c(1,1))
ts.plot(difdata,model$residuals,col=c(4,2))

pred<-predict(model,n.ahead=8)$pred
prediccions<-tail(dades,12)-pred
prediccions

plot(dades,type="l",xlab = "Mes",ylab ="Atur", col=4)
lines(prediccions,type="l",x=c(73:80),col="red")

# Capacitat predictiva
e <- dataExtra-prediccions
ts.plot(e,col=2,main="Error",xlab="Mes")

EQM  <- sum(e^2)/8 # Error quadràtic mitjà
EAM  <- sum(abs(e))/8  # Error absolut mitjà
EPAM <- (sum(abs(e)/dataExtra)/8)*100

cbind(DadesObservades= dataExtra, Prediccions=prediccions, error= e)

