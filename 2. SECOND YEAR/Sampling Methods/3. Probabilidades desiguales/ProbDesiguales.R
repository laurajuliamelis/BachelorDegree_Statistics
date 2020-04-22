##########################################
##         MÈTODES DE MOSTRATGE         ##
##  Pràctica 3: Probabilitas Desiguals  ##
##           Laura Julià Melis          ##
##########################################

# Se instala y se carga el paquete sampling:
install.packages("sampling")
library(sampling)

#####################################################
# LECTURA DE LA BASE DE DATOS ANUARIO DE MUNICIPIOS.#
#####################################################
base <- read.table("AnuarioN.CSV", header = TRUE, sep = ";", dec = ",", row.names = 1)
names(base)
View(base)

###################################################################
# 1. CÁLCULO PROBABILIDADES DE INCLUSIÓN PROPORCIONALES A Pob2010.#
###################################################################
set.seed(16810883)
Y <- base$ActComMinor # variable de interés
X <- base$Pob2010     # variable auxiliar
N <- length (Y)  # N= 218
n <- 70
pik <- inclusionprobabilities(X, n) 
pik # vector de probabilidades de inclusión de primer orden
sum(pik)


########################################################
# 2. EXTRACCIÓN DE LA MUESTRA CON EL ALGORITMO UPtille.#
########################################################
name <- base$ActComMinor
s <- UPtille(pik, eps = 1e-6)
s  # Vector de 1 y 0 según si la unidad pertenece a la muestra o no.

######################################
# 3. SIMULACIÓN DEL TRABAJO DE CAMPO.#
######################################
muestra <- getdata(base, s)
y <- muestra$ActComMinor  # Vector con los valores de la muestra.


##################################################
# 4. CÁLCULO DE ALGUNOS PARÁMETROS POBLACIONALES.#
##################################################
Tpob <- sum(Y)		# Total = 94337
Mpob <- Tpob/N		# Media = 432.7385
Qvar <- var(Y)		# Cuasi-varianza = 5991843
Vpob <- Qvar * (N-1)/N	# Varianza = 5964357


############################
# 5. ESTIMACIÓN DEL TOTAL. #
############################
# Estimación puntual: (HTestimator)
totaly <- HTestimator(name[s == 1], pik[s == 1]) 
totaly # 94153.52

# Estimación por intervalo: (UPtillepi2)
PIKL <- UPtillepi2(pik) 
PIKL  #matriz de probabilidades de inclusión de seguno orden

PIKL[, pik == 1] <- pik
tPIKL <- t(PIKL)
tPIKL[, pik == 1] <- pik
PIKL <- t(tPIKL)
PI <- PIKL
PI

liminf <- totaly - 2*sqrt(Vhat) 
liminf # 92685.79
limsup <- totaly + 2*sqrt(Vhat)
limsup # 95621.25
# Vhat = estimación de la variancia calculada a continuación

#####################################################
# 6. CÁLCULO DE LA VARIANZA DEL ESTIMADOR DEL TOTAL.#
#####################################################
D <- PI - pik %*% t(pik)  # Matriz delta (D)
Yp <- Y/pik # Vector poblacional Y/pik

# Estimación de la varianza del estimador:
ypm <- Yp[s == 1]  # ypm es Yp pero solo sobre la muestra
sum(ypm) # igual al total

UN <- rep(1, n)
MMM <- (ypm %*% t(UN) - UN %*% t(ypm)) ^ 2
MMM2 <- (D/PI)[s == 1, s == 1]
VVV <- MMM * MMM2
Vhat <- -sum(VVV)/2
Vhat # 538557.3

# Varianza exacta:
Vexact <- t(Yp) %*% D %*% Yp
Vexact # 843607.6


###############################
# 7. ESTIMADOCIÓN DE LA MEDIA.#
###############################
Ymedia <- totaly/N                      # Media estimada = 431.8969
Vexactmed <- Vexact/(N*N)               # Varianza de la media = 17.75119
Vhatmed <- Vhat/(N*N)                   # Estimación de la varianza = 11.33232
liminfmed <- Ymedia - 2*sqrt(Vhatmed)   # Límite inferior = 425.1642
limsupmed <- Ymedia + 2*sqrt(Vhatmed)   # Límite superior = 438.6296


#######################
# 8. EFECTO DE DISEÑO.#
#######################

# Exacto (Varianza del total en el Diseño simple: 2761725937)
DEFF <- Vexact / 2761725937  # 0.0003054639

# Estimado (Estimación varianza del total en el Diseño simple: 8296393048)
DEFFest <- Vhat / 8296393048  # 6.491463e-05
 

