# ANÀLISI DE SÈRIES TEMPORALS



# ANÀLSI DE MODELS ARMA SEGONS PARÀMETRES

# ARMA(2,2)

phi1<-0
phi2<-0
theta1<-0
theta2<-0

# 1- CALCUL DE FAS I FAP TEÒRIQUES

win.graph(width=8, height=10)
layout(matrix(1:2, nrow = 2, ncol = 1)) 
acf<-ARMAacf(ar=c(phi1,phi2),ma=c(theta1,theta2), lag.max = 20, pacf=FALSE)
plot(acf,type="h",ylim=c(-1,1))
pacf<-ARMAacf(ar=c(phi1,phi2),ma=c(theta1,theta2), lag.max = 20, pacf=TRUE)
plot(pacf,type="h",ylim=c(-1,1))

# 2- SIMULACIÓ

win.graph(width=8, height=10)
layout(matrix(1:1, nrow = 1, ncol = 1)) 
x<-arima.sim(model=list(ar=c(phi1,phi2),ma=c(theta1,theta2)),n=120)
plot.ts(x,col=4)

# 3- ESTIMACIÓ FAS I FAP

win.graph(width=8, height=10)
layout(matrix(1:2, nrow = 2, ncol = 1))     
acf(x,ylim=c(-1,1))
pacf(x,ylim=c(-1,1))

# 4- ESTIMACIÓ MODEL

ar<-0
ma<-0
model<-arima(x,order=c(ar,0,ma))
model

# 5- ESTIMACIÓ FAS I FAP RESIDUS MODEL

win.graph(width=8, height=10)
layout(matrix(1:2, nrow = 2, ncol = 1))     
acf(model$resid,ylim=c(-1,1))
pacf(model$resid,ylim=c(-1,1))


