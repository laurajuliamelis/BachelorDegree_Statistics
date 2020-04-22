# SÈRIES TEMPORALS


# IDENTIFICACIÓ

serie<-ts(read.table("E:/had.txt"),start=1880,frequency=12)
par(mfrow=c(1,1))
plot.ts(serie,col=4)

# NORMALITAT

# Jarque-Bera (instal·lar paquet fBasics)

# installing the package plspm & plsdepot
install.packages("fBasics")

# load package plspm
library("fBasics")

jarqueberaTest(serie)

# ESTACIONARIETAT

# Les dades clarament tenen tendència: hem d'agafar diferències regulars per resoldre la no estacionarietat de primer ordre

# Diferència regular

dserie<-diff(serie)
par(mfrow=c(1,1))
plot(dserie)

par(mfrow=c(2,1))
acf(dserie,ylim=c(-1,1),lag=100)
pacf(dserie,ylim=c(-1,1),lag=100)

# TRANSFORMACIONS

# No sembla que siguin necessàries

# Box-Cox

# lambda<-0.6
# m<-10

# BCserie<-((serie+m)^lambda)/lambda

# IDENTIFICACIÓ

# Part estacional: no n'hi ha

# Part regular: fem zoom al primer any

par(mfrow=c(2,1))
acf(dserie,ylim=c(-1,1),lag=12)
pacf(dserie,ylim=c(-1,1),lag=12)

# FAS: un valor diferent de zero

# FAP: decreixement ràpid

# SARIMA(0,1,1)(0,0,0)12

# ESTIMACIÓ

model<-arima(serie,order = c(0,1,1),seasonal = list (order = c(0,0,0)))
model

# VALIDACIÓ

# Significació (assimptòtica) coeficients - Valor-p

pnorm(c(abs(model$coef)/sqrt(diag(model$var.coef))), mean=0, sd=1, lower.tail=FALSE)

# Validació: Residus

# Normalitat dels residus
jarqueberaTest(model$residuals)

# Autocorrelació dels residus: Ljung-Box
tsdiag(model)

model$residuals

par(mfrow=c(2,1))
acf(model$residuals,ylim=c(-1,1),lag=100)
pacf(model$residuals,ylim=c(-1,1),lag=100)

# COMPROVACIÓ ESTACIONARIETAT I INVERTIBILTAT

# Comprovar que les arrels de la part AR i de la part MA estan fora del cercle unitat

# Subdiferenciació? Sobrediferenciació?

# CRITERI D'INFORMACIÓ D'AKAIKE: Entre tots els models validats, elegir el que menor valor AIC

# PREDICCIÓ

# Predicció ex-post

serie2<-ts(serie[1:1548],start=1880,frequency=12)

model2<-arima(serie2,order = c(0,1,1),seasonal = list (order = c(0,0,0)))

model2

serie2f<-predict(model2,n.ahead=21)
serie2f
ts.plot(serie,serie2f$pred,col=1:2)

# Avaluació capacitat predictiva

errors<-serie[1549:1569]-serie2f$pred
ts.plot(errors,col=2)
eqm<-sum(errors*errors)/21
reqm<-eqm^(.5)
eam<-sum(abs(errors))/21
epam<-sum(abs(errors)/abs(serie[1549:1569]))/21

eqm
reqm
eam
epam

# Predicció ex-ante

serief<-predict(model,n.ahead=15)
serief
ts.plot(serie,serief$pred, col=c(4,2))

inf = serief$pred - 2*serief$se
sup = serief$pred + 2*serief$se

minx=min(serie,inf)
maxx=max(serie,sup)

ts.plot(serie,serief$pred, col=c(4,2))
lines(inf, col="blue", lty="dashed") 
lines(sup, col="blue", lty="dashed")
