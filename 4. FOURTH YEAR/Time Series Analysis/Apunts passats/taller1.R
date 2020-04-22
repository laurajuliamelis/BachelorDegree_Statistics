# AN???LISI DE S???RIES TEMPORALS


# Anem a simular alguns processos estoc???stics

# VARIABLES ALEAT???RIES NORMALS (0,1)

x1<-ts(rnorm(120),start=1900,frequency=1)
plot.ts(x1,type="l")

acf(x1, lag=60)
pacf(x1, lag=60)

# NORMAL(0,1) + CONSTANT

mu<-50
x2<-ts(rnorm(120)+mu,start=1900,frequency=1) 
plot.ts(x2,type="l")

acf(x2)
pacf(x2)

# NORMAL(0,1) + CONSTANT + TENDENCIA

mu<-50
beta<-0.2
x3<-ts(rnorm(120)+mu+beta*c(1:120),start=1900,frequency=1) 
plot.ts(x3,type="l")

# NORMAL(0,1) + CONSTANT + TENDENCIA + CICLE

mu<-50
beta<-0.1
alfa<-2
a<-0.2
x4<-ts(rnorm(120)+mu+beta*c(1:120)+alfa*cos(c(1:120)*a),start=1900,frequency=1) 
plot.ts(x4,type="l")

# CONVOLUCI???

epsilon<-ts(rnorm(120),start=1900,frequency=1)
x5<-filter(epsilon,0.5,method="convolution")
plot.ts(x5,type="l")

acf(x5)
pacf(x5)

# FILTRE RECURSIU

epsilon<-ts(rnorm(120),start=1900,frequency=1)
x6<-filter(epsilon,0.5,method="recursive")
plot.ts(x6,type="l")

acf(x6)
pacf(x6)


# CAMI ALEATORI

epsilon<-ts(rnorm(120),start=1990,frequency=12)
x7<-filter(epsilon,1,method="recursive")
plot.ts(x7,type="l")


