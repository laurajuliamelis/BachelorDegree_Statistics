# REGRRESI??N A LA MEDIA
# 12.09.2018

#PAS MEDIA
mu=115

#PAS HIPERTENSI??N (en ml)
HT=140

#Variabilidad entre pacientes (desv tipica entre hipo  e hipertensos)
se=12.5

#Variabilidad intra pacientes (oscilaci??n aleatoria de PAS de cada paciente)
si=7

#N??mero de observaciones
N= 2000

# PAS t??pica del individuo
pas0= mu + rnorm(N,0,se)

# PAS al incicio
pas1= pas0 + rnorm(N,0,si)

# PAS al final
pas2= pas0 + rnorm(N,0,si)

plot(pas1, pas2)

#Hipertenso es PAS>HT
col1= as.numeric(pas1>HT)
col2= as.numeric(pas2>HT)


table(col1,col2)

col=col1*2 + col2*2

plot(pas1, pas2, t='n')
abline(h=HT, lty=2)
abline(v=HT, lty=2)

points(pas1, pas2, col=col)

# ??Qu?? grupo de pacientes est?? representado en azul, y en rojo?

# Tomamos al grupo con PAS alta al inicio (que fueron a consulta)
ht=which(pas1>HT)
cons=data.frame(p1=pas1[ht], p2=pas2[ht], col[ht])

# ??Qu?? variaci??n han tenido estos pacientes?
attach(cons)
var=p2-p1
boxplot(var)
stripchart(var, vertical = T, add = T)
detach(cons)
# Se observa que como m??nimo un 80% de los PAS han bajado

# En media, ??la PAS ha cambiado? ??Ha subido o bajado?
t.test(var)

# df=7, as?? que tenemos 71 observaciones
# p-valor significativo, pero no es convincente.Pero vemos con el IC que en promedio,
# la PAS baja entre 6 y 10  ml. El efecto de "tomarse ese medicamento" es que, en un
# IC el 95% hace bajar la PAS en un promedio de entre 6 y 10 puntos.


#??Qu?? hemos hecho para bajarla? ...

# Nos ha bajado a todos, no es casualidad, pero nosotros no hemos hecho nado. Hay algo
# sistem??tico que nos hace a todos obtener PAS menores al final.

# Influencias: el punto de corte (cuanto mas extremo mas acusado) y la correlaci??n


# 2. COMENTAR GR??FICO:

x = range(pas1)
Q=seq(x[1], x[2], len=10)
P=round(Q[1:9]+Q[2:10]/2)
cortes= cut(pas1, Q, lab=P)
boxplot(pas2-pas1 ~cortes, ylab="pas2-pas1", xlab="pas basal")
abline(h=0, col="grey")

# Variacion de la PAS. El grupo que hemos analizado podr??a ser uno de los dos ??ltimos
# BOXPLOTS. Vemos que los que tenian una PAS baja al principio, les sube; lo que la
# ten??an alta, les baja; y la que la tenian media, nulo. En conjunto, la variaci??n es
# nula. 

# 3. C??MO CONSEGUIR UN GRUPO CONTROL:
n=100 # muestra al azar
rand=sample(1:N,n,replace=F)
pr1=pas1[rand]
pr2=pas2[rand]
hist(pr2-pr1)
t.test(pr2-pr1) # miro la diferencia de sus presiones. En promedio su variaci??n es 
# compatible con ser nula (el 0 est?? en el intervalo, no podemos rechazar que la media sea 0)

# Los pacientes disponibles son los que vienen a la consulta la 1a vez, y se les
# seguir?? hasta la 2a visita. Los repartimos al azar, y comparamos sus descensos.

m= round(length(ht)/2) 
cons$asig= sample(c(rep(1,m), rep(2, length(ht)-m))) 
# divido en dos grupos de longitud m a los pacientes, asignandoles al azar el valor 1 o 2
attach(cons)
boxplot(var~asig)
t.test(var~asig, var.equal=T) # t test de comparacion de medias de grupos indep y medias iguales(vienen de la misma poblaci??n)
detach(cons)

# Observamos que el PAs va de -8 a 0, con p-valor 0.06 (al borde de la significaci??n), pudiendo
# asumir que no hay diferencia en el descenso entre los grupos que han tomado placebo y el 
# grupo que ha tomado pastillas. La diferencia entre estos dos promedios podria ser nula (aunqe
# se podr??a dudar mucho)

# Lo normal es no encontrar diferencia significativa, porque observamos que la media del grupo 1
# es -10 y la del grupo 2, -6. Es lo que se supone que tiene que ocurrir, porque no hemos hecho
# ninguna diferencia entre los dos grupos; es logico que consideremos que tienen igual media.
