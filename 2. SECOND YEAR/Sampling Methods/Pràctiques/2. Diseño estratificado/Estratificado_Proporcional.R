##########################################
##         MÈTODES DE MOSTRATGE         ##
##   Pràctica 2: Disseny Estratificat   ##
##           Laura Julià Melis          ##
##########################################

# Se instala y se carga el paquete sampling:
install.packages("sampling")
library(sampling)


# 1. LECTURA DE LA BASE DE DATOS
base<-read.table("AnuarioN.CSV",header=TRUE,sep=";",dec=",",row.names=1)
names(base)
View(base)

# 2. CÁLCULO TAMAÑOS POBLACIONALES DE LOS ESTRATOS.
Nh <- table(base$Size_pob) # Población en  cada estrato h
N <- sum(Nh)   # Población total


##########################################
##         REPARTO PROPORCIONAL         ##
##########################################

# 3. REPARTO DE LAS 50 OBSERVACIONES PROPORCIONALMENTE ENTRE LOS ESTRATOS.

n = 50
nh = Nh*(n/N)
nh = (round(nh))
sum(nh)      
# Al redondear la muestra nos sale de 51, por lo que eliminamos 1.
nh = c(12,12,13,11,2)   # antes era c(12,13,13,11,2)



# 4. EXTRACCIÓN DE LA MUESTRA.

set.seed(1234)
s <- strata(base, stratanames=c("Size_pob"), size=nh, method="srswor", description=TRUE)
muestra <- getdata(base,s)
muestra
# Nos quedamos únicamente con el valor de nuestra variable de interés. 
y <- muestra$ActComMinor
y  



# 5. ESTIMACIONES. 
# 5.1. Estimación puntual del Total.
totaly <- HTstrata(y,muestra$Prob,muestra$Stratum,description=TRUE)
totaly  # 61641.98

# 5.2. Estimación de la varianza del estimador del Total. 
y1 = muestra$ActComMinor[s$Stratum == 1] ## valores estrato1
y2 = muestra$ActComMinor[s$Stratum == 2] ## valores estrato2
y3 = muestra$ActComMinor[s$Stratum == 3] ## valores estrato3
y4 = muestra$ActComMinor[s$Stratum == 4] ## valores estrato4
y5 = muestra$ActComMinor[s$Stratum == 5] ## valores estrato5

varhat1 = varest(y1,pik=muestra$Prob[s$Stratum == 1]) ## Varianza estimada del estrato 1
varhat2 = varest(y2,pik=muestra$Prob[s$Stratum == 2]) ## Varianza estimada del estrato 2
varhat3 = varest(y3,pik=muestra$Prob[s$Stratum == 3]) ## Varianza estimada del estrato 3
varhat4 = varest(y4,pik=muestra$Prob[s$Stratum == 4]) ## Varianza estimada del estrato 4
varhat5 = varest(y5,pik=muestra$Prob[s$Stratum == 5]) ## Varianza estimada del estrato 5

varhat_h = rbind(varhat1,varhat2,varhat3,varhat4,varhat5) 
varhatT = sum(as.vector(varhat_h))
varhatT     # 25937839

# 6. OTROS CÁLCULOS.
# 6.1.  Verdadera varianza del estimador.

# Población repartida en 5 estratos:
Y1 = getdata(base$ActComMinor,base$Size_pob=="1")
Y2 = getdata(base$ActComMinor,base$Size_pob=="2")
Y3 = getdata(base$ActComMinor,base$Size_pob=="3")
Y4 = getdata(base$ActComMinor,base$Size_pob=="4")
Y5 = getdata(base$ActComMinor,base$Size_pob=="5")

# Varianzas de los estratos
s2_1 = var(Y1$data)
s2_2 = var(Y2$data)
s2_3 = var(Y3$data)
s2_4 = var(Y4$data)
s2_5 = var(Y5$data)

s2_h <- rbind(s2_1,s2_2,s2_3,s2_4,s2_5)
s2_h <- as.vector(s2_h)

varexact <- (Nh*Nh)*(1- nh/Nh)*(s2_h/nh)
varexact

S2T <- sum(varexact)  
S2T   # 2648723173

# 6.2. Efecto de diseño 

# Exacto
# DEFF = VARIANZA TOTAL ESTRATIFICADA/VARIANZA TOTAL MAS

VartotalASSR <- 2761725937    # Varianza del total (variable vartotal en el script de Diseño simple)
DEFFp <- S2T / VartotalASSR
DEFFp       #  0.9590826

# Estimado
# DEFFest = ESTIMACION VARIANZA ESTRATIFICADA/VARIANZA ESTIMADA MAS

VartotalASSRest <- 8296393048    # Estimación varianza del total (variable vartotalest en el script de Diseño simple) 
DEFFpest <- varhatT / VartotalASSRest 
DEFFpest      #  0.003126399

##########################################
##            REPARTO ÓPTIMO            ##
##########################################

# 7.  REPARTO DE LAS 50 OBSERVACIONES ENTRE LOS ESTRATOS.
n <- 50
Ypob <- base$ActComMinor
Ypob1 <- Ypob[base$Size_pob==1]
Ypob2 <- Ypob[base$Size_pob==2]
Ypob3 <- Ypob[base$Size_pob==3]
Ypob4 <- Ypob[base$Size_pob==4]
Ypob5 <- Ypob[base$Size_pob==5]

S2_1 <- var(Ypob1)
S2_2 <- var(Ypob2)
S2_3 <- var(Ypob3)
S2_4 <- var(Ypob4)
S2_5 <- var(Ypob5)

S2_h <- rbind(S2_1,S2_2,S2_3,S2_4,S2_5)
S2_h <- as.vector(S2_h)
S2_h
S_h <- sqrt(S2_h)
S_h
denom <- sum(Nh*S_h)
denom
nho <- (Nh*S_h/denom)*n
nho

# Como no se pueden observar más de 7 municipios en el estrato 5 y hay adjudicados 39, se cambia y
# a continuación, se reparten las n-Nh[5] observaciones que nos quedan entre los estratos restantes
nho[5] <- Nh[5]
NHSh <- Nh[1:4]*S_h[1:4]
NHSh
denom<-sum(NHSh)
nbis <- n - Nh[5] 
nho[1] <- (Nh[1]*S_h[1]/denom)*nbis
nho[2] <- (Nh[2]*S_h[2]/denom)*nbis
nho[3] <- (Nh[3]*S_h[3]/denom)*nbis
nho[4] <- (Nh[4]*S_h[4]/denom)*nbis
nho[4]
nho <- round(nho)
nho

# para no tener un estrato con una sola observación
nho[1] <- 2
nho[4] <- 32
nho   # Tamaño de nuestros estratos (sum(nho) = 50).



# 8. EXTRACCIÓN DE LA MUESTRA.
soptimo <- strata(base, stratanames=c("Size_pob"), size=nho, method="srswor", description=TRUE)
muestraoptimo <- getdata(base,soptimo)
muestraoptimo
# Nos quedamos únicamente con el valor de nuestra variable de interés. 
y <- muestraoptimo$ActComMinor
y  



# 9. ESTIMACIONES.
# 9.1. Estimación puntual del total.
totalyoptimo <- HTstrata(y,muestraoptimo$Prob,muestraoptimo$Stratum,description=TRUE)
totalyoptimo  # 94320.44


# 9.2. Estimación de la varianza del estimador del Total.
y1opt = muestraoptimo$ActComMinor[soptimo$Stratum == 1] ## valores estrato1
y2opt = muestraoptimo$ActComMinor[soptimo$Stratum == 2] ## valores estrato2
y3opt = muestraoptimo$ActComMinor[soptimo$Stratum == 3] ## valores estrato3
y4opt = muestraoptimo$ActComMinor[soptimo$Stratum == 4] ## valores estrato4

varhat1opt = varest(y1opt, pik = muestraoptimo$Prob[soptimo$Stratum == 1]) ## Varianza estimada del estrato 1
varhat2opt = varest(y2opt, pik = muestraoptimo$Prob[soptimo$Stratum == 2]) ## Varianza estimada del estrato 2
varhat3opt = varest(y3opt, pik = muestraoptimo$Prob[soptimo$Stratum == 3]) ## Varianza estimada del estrato 3
varhat4opt = varest(y4opt, pik = muestraoptimo$Prob[soptimo$Stratum == 4]) ## Varianza estimada del estrato 4

varhat_hopt = rbind(varhat1opt,varhat2opt,varhat3opt,varhat4opt) 
varhatTopt = sum(as.vector(varhat_hopt))
varhatTopt     # 6896479



# 10. OTROS CÁLCULOS.
# 10.1. Verdadera varianza del estimador.
varexactoptimo <- (Nh*Nh)*(1- nho/Nh)*(s2_h/nho) 
varexactoptimo

S2Topt <- sum(varexactoptimo)  
S2Topt   # 5840141

# 10.2. Efecto de diseño 

# Exacto
# DEFF = VARIANZA TOTAL ESTRATIFICADA/VARIANZA TOTAL MAS

VartotalASSR <- 2761725937    # Varianza del total (variable vartotal en el script de Diseño simple)
DEFFo <- S2Topt / VartotalASSR
DEFFo    # 0.002114671

# Estimado
# DEFFest = ESTIMACION VARIANZA ESTRATIFICADA/VARIANZA ESTIMADA MAS

VartotalASSRest <- 8296393048    # Estimación varianza del total (variable vartotalest en el script de Diseño simple)
DEFFoest <- varhatTopt / VartotalASSRest
DEFFoest  # 0.0008312624 


# 11. COMPARACIÓN DE RESULTADOS CON LOS DOS REPARTOS.

comp <- S2Topt / S2T
comp
compest <- varhatTopt / varhatT
compest
# La varianza obtenida con el reparto óptimo es más pequeña en ambos casos.
