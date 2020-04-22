# AN�LISI DE S�RIES TEMPORALS


# Anem a simular alguns processos estoc�stics

# 1. VARIABLES ALEAT�RIES NORMALS (0,1)

x1<-ts(rnorm(120),start=1900,frequency=1)
plot.ts(x1,type="l")

acf(x1)
pacf(x1)

# 2. NORMAL(0,1) + CONSTANT

mu<-50
x2<-ts(rnorm(120)+mu,start=1900,frequency=1) 
plot.ts(x2,type="l")

acf(x2)
pacf(x2)

# 3. NORMAL(0,1) + CONSTANT + TENDENCIA

mu<-50
beta<-0.2
x3<-ts(rnorm(120)+mu+beta*c(1:120),start=1900,frequency=1) 
plot.ts(x3,type="l") # NO estacionari, clarament tenim tend�ncia encara que no problemes amb la variabilitat. Hauriem d'agafar difer�ncies.

# 4. NORMAL(0,1) + CONSTANT + TENDENCIA + CICLE

mu<-50
beta<-0.1
alfa<-2
a<-0.2
x4<-ts(rnorm(120)+mu+beta*c(1:120)+alfa*cos(c(1:120)*a),start=1900,frequency=1) 
plot.ts(x4,type="l")

# 5. CONVOLUCI� (proc�s de mitjana m�bil, MA(1) y_t = epsilon_t - 0.5 * epsilon_t-1)
# sempre es estacionri indepentendment del valor de psi, comprovarho fent 0.5, 0.9, 1.2 ....

epsilon<-ts(rnorm(120),start=1900,frequency=1)
x5<-filter(epsilon,0.5,method="convolution")
plot.ts(x5,type="l")

acf(x5) #FAS: nom�s un valor diferent de 0, el que surt diferent de 0 a la meitat segurament sigui un outlier.
pacf(x5) #FAP: tendeix tant rapid a 0 que tots els valors s�n iguals a 0.

# 6. FILTRE RECURSIU (Y_t = 0.5 * Y_t-1 + epsilon_t)

epsilon<-ts(rnorm(120),start=1900,frequency=1)
x6<-filter(epsilon,0.5,method="recursive") # psi(0 tatxat) = 0.5, pbservar que si psi �s major que 1, la s�rie t� tend�ncia i no va b� (no t� estacionarietat)
plot.ts(x6,type="l")

acf(x6) # Funci� d'autocorrelaci� del proc�s x6, veiem que tendeix molt r�pidament cap a 0
pacf(x6) # FAP, nom�s un valor diferents a 0, la resta (que est�n situats entre les franges blaves) son no significatius i podem considerar-los 0.


# 7. CAMI ALEATORI

epsilon<-ts(rnorm(120),start=1990,frequency=12)
x7<-filter(epsilon,1,method="recursive")
plot.ts(x7,type="l")

