###  ANÀLISI DE SÈRIES TEMPORALS 
###        EXAMEN FINAL         
### Laura Julià Melis - 16810883


## EXERCICI 6 ##

## Apartat 1
# 1. Introduim el fitxer amb les dades:
serie <-ts(read.table("exercici6_Dades.txt")) 
plot.ts(serie,col=c(4))

par(mfrow=c(2,1))
acf(serie,ylim=c(-1,1))
pacf(serie,ylim=c(-1,1))

# 2. Apliquem diferències regulars per solucionar el problema de la tendència. 
dserie<-diff(serie)
par(mfrow=c(1,1))
plot.ts(dserie,col=4)

par(mfrow=c(2,1))
acf(dserie,ylim=c(-1,1))
pacf(dserie,ylim=c(-1,1)) # ARMA(1,1)


## Apartat 2
# 1. Estimació del model
model<-arima(dserie,order = c(1,0,1))
model

# 2. Significació dels coeficients
pnorm(c(abs(model$coef)/sqrt(diag(model$var.coef))), mean=0, sd=1, lower.tail=FALSE)
