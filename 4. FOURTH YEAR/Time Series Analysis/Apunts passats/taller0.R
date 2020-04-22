# Taller 0

# PASSATGERS L???NIES A???RIES

# Carregar i representar dades

pasa<-ts(read.table("C:/Users/miriam.quero/Downloads/.txt"),start=1978.1,frequency=12) #s=12 mesos

plot.ts(pasa,col=4)
#NO ES ESTACIONARI (ni en mitjanes ni en variabilitat)
# Funci??? d'autocorrelaci??? i autocorrelaci??? parcial

par(mfrow=c(2,1))
acf(pasa,ylim=c(-1,1),lag=100)
pacf(pasa,ylim=c(-1,1),lag=100)
# com que no decreix ràpid (primer gràfic) ens confirma que no 
# és estacionari
# Difer???ncia regular
#analitzem les diferències regulars
dpasa<-diff(pasa)
par(mfrow=c(1,1))
plot.ts(dpasa,col=4)
#Ara hem aconseguit que sigui estacionaria en mitjanes, tot
#i que segueix no sent-ho en quant a variabilitat
par(mfrow=c(2,1))
acf(dpasa,ylim=c(-1,1),lag=100)
pacf(dpasa,ylim=c(-1,1),lag=100)
#Segueix sense decreixer de forma ràpida, per tant no podem
#assegurar que la nostre sèrie sigui estacionaria
# Difer???ncia estacional
#agafem diferències estacionals
d12pasa<-diff(pasa,lag=12) #s'ha de tenir en compte que això es s=12

par(mfrow=c(1,1))
plot.ts(d12pasa,col=4)
#té tendència i té problemes de variabilitat, tot i que menors
par(mfrow=c(2,1))
acf(d12pasa,ylim=c(-1,1), lag=100)
pacf(d12pasa,ylim=c(-1,1), lag=100)
#ara la funció de correlació decreix més ràpidament que abans
#