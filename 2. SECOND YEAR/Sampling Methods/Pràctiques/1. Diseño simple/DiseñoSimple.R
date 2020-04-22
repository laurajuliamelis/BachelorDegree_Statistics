##########################################
##         MÈTODES DE MOSTRATGE          #
##     Pràctica 1: Disseny Simple        #
##           Laura Julià Melis           #
##########################################

# Se instala y se carga el paquete sampling:
install.packages("sampling")
library(sampling)


# 1. LECTURA DE LA BASE DE DATOS
base<-read.table("AnuarioN.CSV",header=TRUE,sep=";",dec=",",row.names=1)
names(base)
View(base)



# 2. EXTRACCIÓN DE LA MUESTRA (n = 70)
n <- 70   # Tamaño de la muestra
N <- length(base$ActComMinor) # Tamaño de la población

set.seed(1234)
s <- srswor(n,N)
s
P <- (1:N)[s==1]
P   # Individuos de mi muestra

muestra <- base$ActComMinor[P]
muestra  # Valor de la variable de interés de los 70 individuos escogidos al azar.



# 3. CÁLCULO DE ESTIMACIONES.
# 3.0. Cálculo de algunos parámetros poblacionales que se utilizarán posteriormente.

totalpob <- sum(base$ActComMinor)
totalpob   # 94337 

medpob <- totalpob/N
medpob     # 432.7385

Qvarpob <- var(base$ActComMinor) 
Qvarpob    # 5991843  Cuasivarianza (S al cuadrado)

varpob <- Qvarpob*(N-1)/N   
varpob     # 5964357 Varianza (sigma al cuadrado)

CV <- sqrt(varpob)/medpob
CV         # 5.6436  Coeficiente de variación.



# 3.1. Estimación por punto.
# Estimación de la media
media <- mean(muestra)     
media    # 798.9571 

# Varianza del estimador de la media
varmedia <- (1-n/N)*Qvarpob/n
varmedia     # 58112.24

# Estimación de la varianza del estimador de la media
varmediaest <- (1-n/N)*var(muestra)/n    
varmediaest  # 174572.7

# Estimación del total 
total <- HTestimator(muestra, rep(n/N, n)) 
total      # 174172.7

# Varianza del estimador del total
vartotal <- (N^2)*varmedia
vartotal   # 2761725937

# Estimación de la varianza del estimador del total
vartotalest <- varest(muestra, pik = rep(n/N, n))
vartotalest    # 8296393048



# 3.2. Estimación por intervalo.
# Intervalo de confianza de la MEDIA:
  # Con la varianza del estimador
cat("[", media-2*sqrt(varmedia), ",", media+2*sqrt(varmedia), "]")         # [316.8275, 1281.087]

  # Con la estimación de la varianza del estimador
cat("[", media-2*sqrt(varmediaest), ",", media+2*sqrt(varmediaest), "]")   # [-36.68082, 1634.595]



# Intervalo de confianza del TOTAL:
  # Con la varianza del estimador
cat("[", total-2*sqrt(vartotal), ",", total+2*sqrt(vartotal), "]")         # [69068.41, 279276.9]

  #  la estimación de la varianza del estimador
cat("[", total-2*sqrt(vartotalest), ",", total+2*sqrt(vartotalest), "]")   # [-7996.419, 356341.7]


