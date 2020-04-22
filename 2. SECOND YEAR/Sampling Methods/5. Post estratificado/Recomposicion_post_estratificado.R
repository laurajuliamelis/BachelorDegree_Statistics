##########################################
##         MÈTODES DE MOSTRATGE         ##
##     Pràctica Recomposición por       ##
##         post-estratificación         ##
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
# 1. EXTRACCIÓN DE LA MUESTRA.
n <- 70   # Tamaño de la muestra
N <- length(base$ActComMinor) # Tamaño de la población
set.seed(2345)
s <- srswor(n,N)
s
muestra <- base$ActComMinor[s == 1]
muestra # Valor de la variable de interés de los 70 individuos 

# 2. ESTIMACIONES.
# 2.0. CÁLCULO DE PARAMETROS POBLACIONALES.
totalpob <- sum(base$ActComMinor)
totalpob   # 94337 

medpob <- totalpob/N
medpob     # 432.7385

Qvarpob <- var(base$ActComMinor) 
Qvarpob    # 5991843  Cuasivarianza (S al cuadrado)

varpob <- Qvarpob*(N-1)/N   
varpob     # 5964357 Varianza (sigma al cuadrado)

# 2.1. ESTIMACIÓN POR PUNTO.
# Estimación de la media
media <- mean(muestra)     
media    # 226.5429

# Varianza del estimador de la media
varmedia <- (1-n/N)*Qvarpob/n
varmedia     # 58112.24

# Estimación de la varianza del estimador de la media
varmediaest <- (1-n/N)*var(muestra)/n    
varmediaest  # 2112.281

# Estimación del total 
pik <- rep(n/N, n)
total <- HTestimator(muestra, pik) 
total      # 49386.34

# Varianza del estimador del total
vartotal <- (N^2)*varmedia
vartotal   # 2761725937

# Estimación de la varianza del estimador del total
vartotalest <- varest(muestra, pik = rep(n/N, n))
vartotalest    # 100384041

# 2.2. ESTIMACIÓN POR INTERVALO.
# Intervalo de confianza de la MEDIA:
M_interval <-c(media-2*sqrt(varmediaest),media+2*sqrt(varmediaest)) 
M_interval # [134.6237, 318.462]

# Intervalo de confianza del TOTAL:
T_interval <-c(total-2*sqrt(vartotalest),total+2*sqrt(vartotalest))   
T_interval # [29347.98, 69424.71]

##########################
## POST-ESTRATIFICACIÓN ##
##########################
# 1. POST-ESTRATIFICACIÓN DE LA MUESTRA.
unique(base$Size_pob)
Nh <- table (base$Size_pob)
Nh <- as.vector(Nh) 
Nh # No queremos una tabla, sinó un vector.

xpos<- poststrata(m,postnames=c("Size_pob"))
xpos # Clasificación muestra en los estratos.

# 2. ESTIMACIONES.
# 2.1. ESTIMACIÓN POR PUNTO.
# Estimación del total:
Pos_totaly<-postest(xpos$data,y=xpos$data$ActComMinor,pik=pik,NG=Nh,description=TRUE)
Pos_totaly #  63690.19

# Estimación de la media:
Pos_M <- Pos_totaly/N
Pos_M # 292.1568

# Cuasi-varianza para cada estrato.
s2_1 <- var(xpos$data$ActComMinor[xpos$data$poststratum == 1])
s2_2 <- var(xpos$data$ActComMinor[xpos$data$poststratum == 2])
s2_3 <- var(xpos$data$ActComMinor[xpos$data$poststratum == 3])
s2_4 <- var(xpos$data$ActComMinor[xpos$data$poststratum == 4])
s2_5 <- var(xpos$data$ActComMinor[xpos$data$poststratum == 5])

s2_h <- c(s2_1, s2_2, s2_3, s2_4, s2_5)
s2_h
s2_h[is.na(s2_h)] = 0 # Corregimos el NA de s2_5
s2_h

# Estimación de la varianza del estimador recompuesto de la media.
pos_varhatMed<-(1-n/N)/n*1/N*sum(Nh*s2_h)+(1-n/N)/(n^2)*sum((1-Nh/N)*s2_h)
pos_varhatMed # 390.6352

# Estimación de la varianza del estimador recompuesto del total.
pos_varhatT  <- ((1-n/N)/n * 1/N*sum(Nh*s2_h) + (1-n/N)/(n^2) * sum((1-Nh/N)*s2_h))*(N^2)
pos_varhatT #  18564549

# 2.2. VERDADERAS VARIANZAS.
#Se reparte la población en 5 estratos:
Y1 <- getdata(base$ActComMinor,base$Size_pob=="1")
Y2 <- getdata(base$ActComMinor,base$Size_pob=="2")
Y3 <- getdata(base$ActComMinor,base$Size_pob=="3")
Y4 <- getdata(base$ActComMinor,base$Size_pob=="4")
Y5 <- getdata(base$ActComMinor,base$Size_pob=="5")

# Varianzas de los estratos
S2_1 <- var(Y1$data)
S2_2 <- var(Y2$data)
S2_3 <- var(Y3$data)
S2_4 <- var(Y4$data)
S2_5 <- var(Y5$data)

S2_h <- rbind(S2_1,S2_2,S2_3,S2_4,S2_5)
S2_h <- as.vector(S2_h)
S2_h

# Varianza del estimador recompuesto de la media.
pos_varMed<-(1-n/N)/n*1/N*sum(Nh*S2_h)+(1-n/N)/(n^2)*sum((1-Nh/N)*S2_h)
pos_varMed # 67167.25

# Varianza del estimador recompuesto del total.
pos_varT<-((1-n/N)/n*1/N*sum(Nh*S2_h)+(1-n/N)/(n^2)*sum((1-Nh/N)*S2_h))*(N^2)
pos_varT # 3192056267

# 2.3. ESTIMACIÓN POR INTERVALO.  
# Intervalos de confianza para el total y la media poblacionales.
pos_M_interval<-c(Pos_M-2*sqrt(pos_varhatMed),Pos_M+2*sqrt(pos_varhatMed)) 
pos_M_interval # [252.6279, 331.6858]

pos_T_interval<-c(Pos_totaly-2*sqrt(pos_varhatT),Pos_totaly +2*sqrt(pos_varhatT))   
pos_T_interval # [55072.87, 72307.51]

######################################
# COMPARACIÓN DE VARIANZAS DEL TOTAL #
######################################
DEFF <- pos_varT / vartotal
DEFF # 1.155819 (Con las varianzas exactas)

DEFFest <-  pos_varhatT / vartotalest
DEFFest # 0.1849353 (Con las varianzas estimadas)