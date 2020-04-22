# ANÀLISI DE SÈRIES TEMPORALS



# ANÀLSI DE MODELS ARMA SEGONS PARÀMETRES

#EXERCICI 1 Yt = 0.8Yt-1 + epsilon_t (PHI2=0)
#EXERCICI 3 Yt = 0.6Yt-1 + 0.3Yt-2 + epsilon_t
# ARMA(2,2) -> 2 coef phi(procés AR) i 2 theta (procés MA)

#PRIMER VALOR DE LA FUNCIÓ D'AUTOCORRELACIÓ HA DE SER IGUAL QUE EL PRIMER VALOR DE LA FUNCIÓ DE AUTOCORRELACIÓ PARCIAL

phi1<-0
phi2<-0
theta1<-1.5
theta2<-0

# 1- CALCUL DE FAS I FAP TEÒRIQUES

win.graph(width=8, height=10)
layout(matrix(1:2, nrow = 2, ncol = 1)) 
acf<-ARMAacf(ar=c(phi1,phi2),ma=c(theta1,theta2), lag.max = 20, pacf=FALSE)
plot(acf,type="h",ylim=c(-1,1))
pacf<-ARMAacf(ar=c(phi1,phi2),ma=c(theta1,theta2), lag.max = 20, pacf=TRUE) #pacf=funció d'autocorrelació parcial
plot(pacf,type="h",ylim=c(-1,1))

#plot de FAS i FAP
#FAP un únic valor diferent de zero, 0.8 = aplha1 = phi1
#Si phi1<-1.8 tindriem un procés no estacionari (pasa amb qualsevol major que 1)

#simtoma de serie estacionaria veient si decreix rapidament el FAS (funció d'autocorrelació) 


# 2- SIMULACIÓ

win.graph(width=8, height=10)
layout(matrix(1:1, nrow = 1, ncol = 1)) 
x<-arima.sim(model=list(ar=c(phi1,phi2),ma=c(theta1,theta2)),n=120)
plot.ts(x,col=4)

# 3- ESTIMACIÓ FAS I FAP

win.graph(width=8, height=10)
layout(matrix(1:2, nrow = 2, ncol = 1))     
acf(x,ylim=c(-1,1)) # Decreix ràpidament cap a 0 (es situa ràpid dintre de les línies blaves)
pacf(x,ylim=c(-1,1)) # Malament! Ens està representant una AR(1), perquè només te un valor diferent 
# de 0 (el primer) i n'hi hauria d'haver 2 perquè les dades que jo he generat provenen d'una AR(2).

# Mirem de tornar a simular dades, però la confusió no es soluciona. És incongruent, però la part positiva 
# és que mai sabrem si venim d'un model AR(2), simplement tindrem unes dades, i anirem a cegues. 

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


