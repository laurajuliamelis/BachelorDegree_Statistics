
#calcul coeficient de determinació
resum <- summary(lmod)
resum$r.squared

cor(predict(lmod), gala$Species)^2 #ha de donar el mateix que el coef de determinacio

# Veurem que passa considerant el model sense beta0
lmod0 <- lm(Species ~ 0 + Area + Elevation + Nearest + Scruz + Adjacent, data=gala)

resum0 <- summary(lmod0)
resum0$r.squared #dóna un valor superior (millor)

#el R^2 d'un model o a l'altre no tenen res a veure

cor(predict(lmod0), gala$Species)^2   #coeficient segons faraway 
                                      #no dóna el mateix que el coef de determinacio

#el coef de determinacion en el cas que no hi hagi terme independent no ha de donar el mateix que la cor^2

sum(predict(lmod0)^2)/sum(gala$Species^2) #aquesta es la formula classica de R^2 quan no hi ha terme independent
                                          # és a dir, aquest és el coeficent clàssic (és el que utilitza R)



