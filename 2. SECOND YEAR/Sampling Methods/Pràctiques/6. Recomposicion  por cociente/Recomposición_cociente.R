##########################################
##         MÈTODES DE MOSTRATGE         ##
##        Pràctica Recomposición        ##
##              por cociente            ##
##           Laura Julià Melis          ##
##########################################

# LECTURA DE LA BASE DE DATOS.
install.packages("sampling")
library(sampling)
base<-read.table("AnuarioN.CSV",header=TRUE,sep=";",dec=",",row.names=1)
names(base)
View(base)

###############################
## MUESTREO ALEATORIO SIMPLE ##
###############################

# 1. ELECCIÓN DEL TAMAÑO DE LA MUESTRA Y DE LAS VARIABLES.
Y <- base$ActComMinor # variable de interés
X <- base$Pob2010     # variable auxiliar
n <- 100
N <- length(base$ActComMinor)

# 2. EXTRACCIÓN DE LA MUESTRA.
set.seed(16810883)
s <- srswor(n,N)
s
y <- base$ActComMinor[s == 1]
y
x <- base$Pob2010[s == 1]
x

# 3. ESTIMACIONES.
# 3.0. CÁLCULO DE PARAMETROS POBLACIONALES.
totalpob <- sum(base$ActComMinor)
totalpob   # 94337 

medpob <- totalpob/N
medpob     # 432.7385 (verdadera media)

Qvarpob <- var(base$ActComMinor) 
Qvarpob    # 5991843  Cuasivarianza (S al cuadrado)

varpob <- Qvarpob*(N-1)/N   
varpob     # 5964357 Varianza (sigma al cuadrado)

# 3.1. ESTIMACIÓN POR PUNTO.
# Estimación de la media
media <- mean(y)     
media    # 270.21

# Varianza del estimador de la media
varmedia <- (1-n/N)*Qvarpob/n
varmedia     #  32432.91

# Estimación de la varianza del estimador de la media
varmediaest <- (1-n/N)*var(y)/n    
varmediaest  # 1961.403

# Estimación del total 
pik <- rep(n/N, n)
totaly <- HTestimator(y, pik) 
totaly #  58905.78 (estimador del total en bruto)

# Varianza del estimador del total
vartotal <- (N^2)*varmedia
vartotal   # 1541341638

# Estimación de la varianza del estimador del total
vartotalest <- varest(y, pik = rep(n/N, n))
vartotalest    #  93213706

# 3.2. ESTIMACIÓN POR INTERVALO.
# Intervalo de confianza de la MEDIA:
M_interval <- c(media-2*sqrt(varmediaest), media+2*sqrt(varmediaest))
M_interval  # [181.6345, 358.7855]

# Intervalo de confianza del TOTAL:
T_interval <- c(totaly-2*sqrt(vartotalest), totaly+2*sqrt(vartotalest))   
T_interval # [39596.33, 78215.23]


################################
## RECOMPOSICIÓN POR COCIENTE ##
################################
# 1. ESTIMACIÓN PUNTUAL.
#Estimación del total. 
Tx <- sum(base$Pob2010) # Total de la variable auxiliar
totalyQ <- ratioest(y, x, Tx, pik)
totalyQ # 78586.18 

# Estimación de la varianza del estimador del total.
rh <- mean(y) /mean(x)
u <- y - rh*x
u
VhQT <- N^2 * (1-n/N)*var(u)/n
VhQT # 3925644

# Varianza del estimador del total.
R <- mean(base$ActComMinor) / mean(base$Pob2010)
U <- base$ActComMinor - R * base$Pob2010
VQT <- N^2 * (1-n/N) * var(U)/n 
VQT  # 72767015 (Verdadera varianza)

# Estimación de la media.
MediaestQ <- totalyQ /N
MediaestQ # 360.4871

# Estimación de la varianza del estimador de la media.
VhQM <- VhQT / (N^2)
VhQM # 82.6034

# Varianza del estimador de la media
VQM <- VQT/ (N^2)
VQM # 1531.164


# 2. ESTIMACIÓN POR INTERVALO.
# Intervalo de confianza de la MEDIA:
M_interval_Q <- c(MediaestQ - 2*sqrt(VhQM), MediaestQ + 2*sqrt(VhQM))
M_interval_Q  # [342.3098, 378.6643]

# Intervalo de confianza del TOTAL:
T_interval_Q <- c(totalyQ - 2*sqrt(VhQT), totalyQ + 2*sqrt(VhQT))
T_interval_Q # [74623.53, 82548.83]

######################################
# COMPARACIÓN DE VARIANZAS DEL TOTAL #
######################################
DEFF <- VQT / vartotal
DEFF # 0.04721018 (Con las varianzas exactas)

DEFFest <-  VhQT / vartotalest
DEFFest # 0.04211445 (Con las varianzas estimadas)