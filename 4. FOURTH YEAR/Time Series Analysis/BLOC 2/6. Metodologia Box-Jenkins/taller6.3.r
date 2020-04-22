# ANALISIS DE SERIES TEMPORALES


#
# PREDICCIÓN CON MODELOS ARMA
#


# AR(1)

data<-arima.sim(model=list(ar=.7),n=120)
ts.plot(data,col=4)

fit<-arima(data,c(1,0,0))
ts.plot(data,fit$residuals, col=c(1,2))

# Predicció ex-ante

dataf<-predict(fit,n.ahead=24)
ts.plot(data,dataf$pred, col=c(4,2))

# Errors estàndard de predicció

dataf<-predict(fit,n.ahead=24)

inf = dataf$pred - 2*dataf$se
sup = dataf$pred + 2*dataf$se

minx=min(data,inf)
maxx=max(data,sup)

ts.plot(data,dataf$pred, col=c(4,2))
lines(inf, col="blue", lty="dashed") 
lines(sup, col="blue", lty="dashed")

# Predicció ex-post

data2<-data[1:108]
fit2<-arima(data2,c(1,0,0))

data2f<-predict(fit2,n.ahead=12)
ts.plot(data,data2f$pred,col=1:2)

# Avaluació capacitat predictiva

errors<-data[109:120]-data2f$pred
ts.plot(errors,col=2)
eqm<-sum(errors*errors)/12
reqm<-eqm^(.5)
eam<-sum(abs(errors))/12
epam<-sum(abs(errors)/abs(data[109:120]))/12

eqm
reqm
eam
epam

# AR(2)

data<-arima.sim(model=list(ar=c(.7,-0.3)),n=120)
ts.plot(data,col=4)

win.graph(width=8, height=10)
layout(matrix(1:2, nrow = 2, ncol = 1))     
acf(data,ylim=c(-1,1))
pacf(data,ylim=c(-1,1))

layout(matrix(1, nrow = 1, ncol = 1))     
fit<-arima(data,c(2,0,0))
ts.plot(data,fit$residuals, col=c(1,2))

# Predicció ex-ante

dataf<-predict(fit,n.ahead=24)
ts.plot(data,dataf$pred, col=c(4,2))

# Errors estàndard de predicció

dataf<-predict(fit,n.ahead=24)

inf = dataf$pred + 2*dataf$se
sup = dataf$pred - 2*dataf$se

minx=min(data,inf)
maxx=max(data,sup)

ts.plot(data,dataf$pred, col=c(4,2))
lines(inf, col="blue", lty="dashed") 
lines(sup, col="blue", lty="dashed")

# Predicció ex-post

data2<-data[1:108]
fit2<-arima(data2,c(2,0,0))

data2f<-predict(fit2,n.ahead=12)
ts.plot(data,data2f$pred, col=1:2)

# Avaluació capacitat predictiva

errors<-data[109:120]-data2f$pred
ts.plot(errors,col=2)
eqm<-sum(errors*errors)/12
reqm<-eqm^(.5)
eam<-sum(abs(errors))/12
epam<-sum(abs(errors)/abs(data[109:120]))/12


eqm
reqm
eam
epam


# MA(1)


data<-arima.sim(model=list(ma=.5),n=120)
ts.plot(data,col=4)

win.graph(width=8, height=10)
layout(matrix(1:2, nrow = 2, ncol = 1))     
acf(data,ylim=c(-1,1))
pacf(data,ylim=c(-1,1))

fit<-arima(data,c(0,0,1))

layout(matrix(1:2, nrow = 2, ncol = 1))     
acf(fit$residuals,ylim=c(-1,1))
pacf(fit$residuals,ylim=c(-1,1))


layout(matrix(1, nrow = 1, ncol = 1))   
ts.plot(data,fit$residuals, col=c(1,2))

# Predicció ex-ante

dataf<-predict(fit,n.ahead=24)
ts.plot(data,dataf$pred, col=c(4,2))

# Errors estàndard de predicció

dataf<-predict(fit,n.ahead=24)

inf = dataf$pred + 2*dataf$se
sup = dataf$pred - 2*dataf$se

minx=min(data,inf)
maxx=max(data,sup)

ts.plot(data,dataf$pred, col=c(4,2))
lines(inf, col="blue", lty="dashed") 
lines(sup, col="blue", lty="dashed")

# Predicció ex-post

data2<-data[1:108]
fit2<-arima(data2,c(0,0,1))

data2f<-predict(fit2,n.ahead=12)
ts.plot(data,data2f$pred, col=1:2)

# Avaluació capacitat predictiva

errors<-data[109:120]-data2f$pred
ts.plot(errors,col=2)
eqm<-sum(errors*errors)/12
reqm<-eqm^(.5)
eam<-sum(abs(errors))/12
epam<-sum(abs(errors)/abs(data[109:120]))/12


eqm
reqm
eam
epam






