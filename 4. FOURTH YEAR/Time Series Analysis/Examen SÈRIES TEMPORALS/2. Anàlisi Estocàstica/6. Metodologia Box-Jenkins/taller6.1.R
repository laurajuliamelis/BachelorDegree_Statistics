# SÈRIES TEMPORALS
# TALLER 6.1

# EXERCICI 1

serie<-ts(read.table("E:/ast_exemple2.txt"),start=1970,frequency=12)
plot.ts(serie,col=4)

par(mfrow=c(2,1))
acf(serie,ylim=c(-1,1),lag=150)
pacf(serie,ylim=c(-1,1),lag=150)

# Transformació logarítmica

lserie<-log(serie)
par(mfrow=c(1,1))
plot(lserie)

par(mfrow=c(2,1))
acf(lserie,ylim=c(-1,1))
pacf(lserie, ylim=c(-1,1))

# Diferència regular

dlserie<-diff(lserie)
par(mfrow=c(1,1))
plot(dlserie)

par(mfrow=c(2,1))
acf(dlserie,ylim=c(-1,1),lag=100)
pacf(dlserie,ylim=c(-1,1),lag=100)

# Diferència estacional

ddlserie<-diff(dlserie,lag=12)
par(mfrow=c(1,1))
plot(ddlserie)

par(mfrow=c(2,1))
acf(ddlserie,ylim=c(-1,1),lag=100)
pacf(ddlserie,ylim=c(-1,1),lag=100)

par(mfrow=c(2,1))
acf(ddlserie,ylim=c(-1,1),lag=12)
pacf(ddlserie,ylim=c(-1,1),lag=12)


# Estimació temptativa

model<-arima(lserie,order = c(1,1,1),seasonal = list (order = c(1,1,1)))
model

# Significació (assimptòtica) coeficients - Valor-p

pnorm(c(abs(model$coef)/sqrt(diag(model$var.coef))), mean=0, sd=1, lower.tail=FALSE)

# Validació: Residus
tsdiag(model)

par(mfrow=c(2,1))
acf(model$residuals,ylim=c(-1,1),lag=100)
pacf(model$residuals,ylim=c(-1,1),lag=100)

# Validació: Càlcul periodogrames 
# Discret: sèrie periòdica
# Discret-Continu: sèrie aperiòdica
# Continu: sèrie estocàstica

# library(TSA)

#par(mfrow=c(1,1))
#periodogram(serie)
#periodogram(lserie)
#periodogram(dlserie)
#periodogram(ddlserie)
#periodogram(model$residuals)

# EXERCICI 2

serie<-ts(read.table("E:/ast_exemple10.txt"),start=1880,frequency=12)
plot.ts(serie,col=4)

# No hi ha estacionarietat de primer ordre (hi ha tendència)

# Sembla que hi ha estacionarietat de segon ordre: la variabilitat sembla uniforme al llarg del temps.

# Per tant, d'entrada, només agafem diferències regulars:

# Diferència regular

dserie<-diff(serie)
par(mfrow=c(1,1))
plot(dserie)

# Sembla estacionària. A veure si la FAS també ho confirma i decreix ràpidament cap a zero:

par(mfrow=c(2,1))
acf(dserie,ylim=c(-1,1),lag=100)
pacf(dserie,ylim=c(-1,1),lag=100)

# Sí que ho confirma. A més, podem veure que no hi ha estacionalitat. No cal agafar diferències esstacionals.

# Les dues "fotos", FAS i FAP, s'interpreten molt fàcilment i no hi ha gaires dubtes: 

# La FAS té un únic valor diferent de zero

# La FAP decreix ràpidament cap a zero.

# Identificació: SARIMA(0,1,1)(0,0,0)12

model<-arima(serie,order = c(0,1,1),seasonal = list (order = c(0,0,0)))
model

# Significació (assimptòtica) coeficients - Valor-p

pnorm(c(abs(model$coef)/sqrt(diag(model$var.coef))), mean=0, sd=1, lower.tail=FALSE)

# Validació: Residus
tsdiag(model)

par(mfrow=c(2,1))
acf(model$residuals,ylim=c(-1,1),lag=100)
pacf(model$residuals,ylim=c(-1,1),lag=100)


# EXERCICI 3

serie<-ts(read.table("E:/ast_exemple9.txt"),start=1973,frequency=12)
plot.ts(serie,col=4)

# Sembla estacionària (tant de primer com de segon ordre). A veure si la FAS també ho confirma i decreix ràpidament cap a zero:

par(mfrow=c(2,1))
acf(serie,ylim=c(-1,1),lag=150)
pacf(serie,ylim=c(-1,1),lag=150)

# Sí que ho confirma. A més, podem veure que hi ha estacionalitat. Per tant l'haurem d'identificar.

# Identificació part estacional:

# La FAS decreix ràpidament cap a zero

# La FAP té un valor diferent de zero.

# Per tant, la part estacional és un AR(1)

# Identificació de la part regular (fem primer un zoom al primer any)

par(mfrow=c(2,1))
acf(serie,ylim=c(-1,1),lag=12)
pacf(serie, ylim=c(-1,1),lag=12)

# La FAS decreix ràpidament cap a zero

# La FAP té un valor diferent de zero (o alternativament podem interpretar que decreix ràpid cap a zero).

# Per tant dues possible opcions per la part regular: o és un AR(1) o un ARMA(1,1)Diferència regular

# Primera estimació temptativa: SARIMA(1,0,0)(1,0,0)12

model<-arima(serie,order = c(1,0,0),seasonal = list (order = c(1,0,0)))
model

# Significació (assimptòtica) coeficients - Valor-p

pnorm(c(abs(model$coef)/sqrt(diag(model$var.coef))), mean=0, sd=1, lower.tail=FALSE)

# Validació: Residus
tsdiag(model)

par(mfrow=c(2,1))
acf(model$residuals,ylim=c(-1,1),lag=100)
pacf(model$residuals,ylim=c(-1,1),lag=100)

# Segona estimació temptativa: SARIMA(1,0,1)(1,0,0)12

model<-arima(serie,order = c(1,0,1),seasonal = list (order = c(1,0,0)))
model

# Significació (assimptòtica) coeficients - Valor-p

pnorm(c(abs(model$coef)/sqrt(diag(model$var.coef))), mean=0, sd=1, lower.tail=FALSE)

# Com que el coeficient associat a la part MA regular (ma1) és no significativa, es onfirma que el model vàlid és el primer.

