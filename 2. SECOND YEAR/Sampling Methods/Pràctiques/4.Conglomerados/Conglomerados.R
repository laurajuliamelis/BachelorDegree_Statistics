##########################################
##         MÈTODES DE MOSTRATGE         ##
##        Pràctica 4: Conglomerats      ##
##           Laura Julià Melis          ##
##########################################

# Se instala y se carga el paquete sampling:
install.packages("sampling")
library(sampling)

#####################################################
# LECTURA DE LA BASE DE DATOS ANUARIO DE MUNICIPIOS.#
#####################################################
base <- read.table('AnuarioUP.csv', header = T, sep = ';',dec = ',', row.names=1)
names(base)
View(base)



######################################################
# 1. FORMACIÓN DE CONGLOMERADOS Y EXTRACCIÓN MUESTRA.#
######################################################
# Se ha elegido la variable GruD (en total existen 37 unidades primarias)
M <- 37
m <- 9 # m elegida para que haga el tamaño de la muestra de US similar a 50.

# Extracción de la muestra
set.seed(1234)
cl <- cluster(base, clustername = c('GruD'), size = m, method ='srswor', description = T)
names(cl)

muestra <- getdata(base, cl)
y <- muestra$ActComMinor # Vector con los valores de la variable de interés.
n <-length(y)
n   # n=52 por lo que m correcta



##################################################
# 2. CÁLCULO DE ALGUNOS PARÁMETROS POBLACIONALES.#
##################################################
Y <- base$ActComMinor
N <- length(Y)
Tpob <- sum(Y)		      # Total = 94337
Mpob <- Tpob/N	       	# Media = 432.7385
Qvar <- var(Y)	      	# Cuasi-varianza = 5991843
Vpob <- Qvar * (N-1)/N  # Varianza = 5964357


###############################
# 3. CÁLCULO DE ESTIMACIONES. #
###############################

######## ESTIMACIÓN DEL TOTAL ########
# Estimación por punto:
pik <- cl$Prob # Probabilidades de inclusión
pik
totaly <- HTestimator(y,pik)
totaly   # 30796.33


# Estimación por intervalo:
# Cálculo del vector Ti
cl.f <- as.factor(cl$GruD)
cl.f
UP <- as.numeric(cl.f)
UP
Ti <- tapply(y,UP,sum)
Ti

# Estimación de la varianza del estimador del total:
pikm <- rep(9/37, 9)
pikm
VarhatT <- varest(Ys = Ti, pik = pikm)
VarhatT   # 26113243

# Varianza del estimador del total:
Talfa <- tapply(Y, base$GruD, sum)
Talfa
ST2 <- var(Talfa)
ST2   # Cuasi-varianza de los totales
Vartotal <- M^2 * (1-m/M) * ST2/m
Vartotal # 3914945690

# Intervalo:
liminf <- totaly - 2*sqrt(VarhatT)
limsup <- totaly + 2*sqrt(VarhatT)
ICTotal <- c(liminf, limsup) # (20576.11, 41016.56)


######## ESTIMACIÓN DE LA MEDIA ########
# Estimación por punto:
media <- totaly/N
media   # 141.2676

# Estimación por intervalo:
Varmed <- Vartotal/(N*N)              # Varianza del estimador = 82378.29
VarhatM <- VarhatT/(N*N)              # Estimación de la varianza = 549.4749
liminfmed <- media - 2*sqrt(VarhatM)
limsupmed <- media + 2*sqrt(VarhatM)
ICMedia <- c(liminfmed, limsupmed)    # (94.38582, 188.14934)


#######################
# 4. EFECTO DE DISEÑO.#
#######################

# Exacto
VarExact_MAS<-(N*N)*(1-n/N)* var(Y)/n
VarExact_MAS
DEFF <- Vartotal / VarExact_MAS  # 0.938867

# Estimado (Estimación varianza del total en el Diseño simple: 8296393048)
DEFFest <- VarhatT / 8296393048  # 0.003147542


