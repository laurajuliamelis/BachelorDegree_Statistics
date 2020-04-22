# Los datos

dens<-c(12.7,17.0,66.0,50.0,87.8,81.4,75.6,66.2,81.1,62.8,77.0,89.6,
18.3,19.1,16.5,22.2,18.6,66.0,60.3,56.0,66.3,61.7,66.6,67.8)
vel<-c(62.4,50.7,17.1,25.9,12.4,13.4,13.7,17.9,13.8,17.9,15.8,12.6,
51.2,50.8,54.7,46.5,46.3,16.9,19.8,21.2,18.3,18.0,16.6,18.3)
rvel<-sqrt(vel) # Raíz de la velocidad

# Gráfico de puntos

par(pty="m")
plot(dens,rvel,type="p",xlab="densidad",ylab="RAIZ(vel)")

# Regresión simple

recta.ls<-lsfit(dens,rvel) # no muestra el resultado #forma no molt adecuada per trobar la recta

# Añadimos la recta de regresión al gráfico anterior.

abline(recta.ls)

# Los coeficientes de la recta son:

recta.ls$coef

# También se puede obtener una información más completa con la
# instrucción {ls.print}.

ls.print(recta.ls, digits=4, print.it=T)

# La estimación de la desviación estándar de los errores y otros
# elementos de diagnosis del modelo se obtienen con la función
# {ls.diag} 

ls.diag(recta.ls)$std.dev


# Con el vector de residuos y las predicciones se pueden dibujar
# unos interesantes gráficos

e<-recta.ls$residuals
par(mfrow=c(1,2))
par(pty="s")
plot(dens,e,type="p",xlab="densidad",ylab="residuos",ylim=c(-0.6,0.6))
abline(h=0)
pred<-rvel-e
plot(pred,e,type="p",xlab="predicción",ylab="residuos",ylim=c(-0.6,0.6))
abline(h=0)

# Finalmente, podemos repetir los cálculos para el modelo
# parabólico. Simplemente debemos introducir los valores de la
# variable densidad y sus cuadrados en una matriz de datos. El resto
# es idéntico al modelo de regresión simple.

matriz.frame<-data.frame(dens,dens^2)

parabola.ls<-lsfit(matriz.frame,rvel)
parabola.ls$coef
round(parabola.ls$coef,5)
e<-parabola.ls$residuals

par(mfrow=c(1,2))
par(pty="s")
plot(dens,e,type="p",xlab="densidad",ylab="residuos",ylim=c(-0.6,0.6))
abline(h=0)
pred<-rvel-e
plot(pred,e,type="p",xlab="predicción",ylab="residuos",ylim=c(-0.6,0.6))
abline(h=0)

# Otras instrucciones de R, en especial la función {lm}, permiten 
# ajustar un modelo de regresión a unos datos.
