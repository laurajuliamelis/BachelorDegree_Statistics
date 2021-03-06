
## GENERICOS (BIOEQUIVALENCIA)

setwd("D:\\ESTAD�STICA\\Estadistica_Medica\\Bloc_2\\E-status\\Bioequivalencia")

dades <- read.table("dades.txt")

# Estime en primer lugar el promedio de la diferencia de logaritmos de 
# concentraciones en sangre (referencia menos gen�rico)

dades$R.log <- log(dades$R)
dades$G.log <- log(dades$G)

dades$dif.logs <- dades$R.log - dades$G.log
m.difs <- mean(dades$dif.logs)


#  A continuaci�n, obtenga la desviaci�n t�pica muestral de la variable anterior

sd.difs <- sd(dades$dif.logs)


# Calcule el estad�stico del contraste de hip�tesis con el que va a poner a prueba 
# si mu(R)/mu(G) < P.	

P <- 0.8
n <- 10

t1 <- (m.difs-log(P))/(sd.difs/sqrt(n))

# valor de la t-student: 
qt(0.95,n)

# Ahora obtenga el estad�stico ligado a la hip�tesis mu(R)/mu(G) > 1/P.	

t2 <- (m.difs-log(1/P))/(sd.difs/sqrt(n))


# P-valor

# t1: <

pt(t1, (n-1), lower.tail=F)

# t2: <

pt(t2, (n-1), lower.tail=T)

# nos quedamos con el p-valor MAYOR

# Intervalo de confianza

# Extremo superior

exp(m.difs + qt(0.95, (n-1))*(sd.difs/sqrt(n)))


# Extremo inferior

exp(m.difs - qt(0.95, (n-1))*(sd.difs/sqrt(n)))







