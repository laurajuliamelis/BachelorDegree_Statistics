data(globwarn, package="faraway")

lmod()
summary(lmod)
# aquestes dades han estat recollides temporalment, per tant, és típics que elserros estiguin correlacionats

ee <-residuals(lmod)

cor(ee[-1], ee[-145])

plot(ee[-1], ee[-145])

#volem que els errors estiguin incorrelacionants, per tant, el gràfic hauria de ser un cercle

#model autorregressiu
#packet nlme, funcio gls(model lineal generalitzat) 
#la correlacio dels errors sera del tipo ar1(autoregressiva 1=any(unitat de temps))