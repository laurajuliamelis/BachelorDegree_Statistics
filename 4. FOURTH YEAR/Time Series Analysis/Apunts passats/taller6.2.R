# SÈRIES TEMPORALS


# IDENTIFICACIÓ

serie<-ts(read.table("C:/Users/victor.ferrer.vazque/Desktop/had.txt"),start=1880,frequency=12)
plot.ts(serie,col=4)

# Jarque-Bera (instal·lar paquet fBasics)
jarqueberaTest(serie)

par(mfrow=c(2,1))
acf(serie,ylim=c(-1,1))
pacf(serie,ylim=c(-1,1))

# Diferència regular

dserie<-diff(serie)
par(mfrow=c(1,1))
plot(dserie)

par(mfrow=c(2,1))
acf(dserie,ylim=c(-1,1),lag=12)
pacf(dserie,ylim=c(-1,1),lag=12)
#aixo es un ma1(sarima(0,1,1)(0,0,0))
#o be sarima(1,1,1)(0,0,0)
par(mfrow=c(2,1))
acf(dserie,ylim=c(-1,1),lag=120)
pacf(dserie,ylim=c(-1,1),lag=120)

# Box-Cox

lambda<-0.6
m<-10

BCserie<-((serie+m)^lambda)/lambda


# ARIMA(0,1,1)??? ARIMA(1,1,1)??? Logaritmes? NO!!! Box-Cox? m!!!

# ESTIMACIÓ

model<-arima(serie,order = c(0,1,1),seasonal = list (order = c(0,0,0)))
model
#incrementY(t)=epsilon(t)-(-0.4769)epsilon(t-1)
# VALIDACIÓ

# Significació (assimptòtica) coeficients - Valor-p

pnorm(c(abs(model$coef)/sqrt(diag(model$var.coef))), mean=0, sd=1, lower.tail=FALSE)

# Validació: Residus

# Jarque-Bera (instal·lar paquet fBasics)
jarqueberaTest(model$residuals)

# Ljung-Box
tsdiag(model)

model$residuals

par(mfrow=c(2,1))
acf(model$residuals,ylim=c(-1,1),lag=100)
pacf(model$residuals,ylim=c(-1,1),lag=100)

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

inf = serief$pred + 2*serief$se
sup = serief$pred - 2*serief$se

minx=min(serie,inf)
maxx=max(serie,sup)

ts.plot(serie,serief$pred, col=c(4,2))
lines(inf, col="blue", lty="dashed") 
lines(sup, col="blue", lty="dashed")

