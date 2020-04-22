
#leemos los datos preprocesados y sin missing values.
#dd credscoClean
dd <- read.table("credscoClean.csv",header=T, sep=";");

#elimino una instancia que estaba a missing 
# no hacer esto...
#dd[3310:4454,]<-dd[3311:4455,]
#dd<-dd[1:4454,]



# Selecciono 2/3 partes de los datos para generar el árbol (aprender/entrenar) i 1/3 para testear/validar

# fijo una semilla para que no cambie cuando se vuelva a generar el script
set.seed(2108)
?sample()
training<-sample(1:nrow(dd), round(2*nrow(dd)/3))


library(rpart)

#Generamos un árbol de clasificación
# la fórumla es del tipo : var.respuesta ~ var1.explicatoria + ...+ varm.explicatoria
# se indican los datos seleccionando sólo las filas que pertenecen al aprendizaje, dejamos fuera las de testeo
# dictamen es un factor , por lo tanto usaremos el método class
p1 = rpart(Dictamen~Edad + Ingresos + Patrimonio, data=dd[training,], method="class")
p1 = rpart(Dictamen~Edad + Ingresos + Patrimonio, data=dd[training,], method="class", parms = list(split="gini"))
# uses 
p1 = rpart(Dictamen~Edad + Ingresos + Patrimonio, data=dd[training,], method="class", parms = list(split="information"))
#devuleve un objecto rpart
# lo podemos inspeccionar de la siguiente forma
p1 #enseña el árbol
attributes(p1) # los atributos del objeto rpart
summary(p1) # detailed summary of splits


# para dibujar el árbol
plot(p1)
text(p1, use.n=T)

#para mirar lo buenos que es el modelo resultante, se predeciran los filas del training 
#con las que se ha creado el árbol

p1l=predict(p1,data=dd[training,])
head(p1l)

p1lp=p1l[,2]
p1lp[p1l[,2]<0.5]=0
p1lp[p1l[,2]>=0.5]=1
t1<-table(dd$Dictamen[training],p1lp)

#miramos el ratio de los que ha predicho bien (accuracy): los positivos que ha dado como positivos + los negativos que ha predicho como negativos
# dividido entre todas las predicciones
(t1[1,1]+t1[2,2])/sum(t1)

# para predecir y en este caso validar el modelo, se mira que precide el a´rbol para las filas que no pertenencen al training 
p1t = predict(p1,newdata=dd[-training,])
p1tp=p1t[,2]
p1tp[p1t[,2]<0.5]=0
p1tp[p1t[,2]>=0.5]=1
t2<-table(dd$Dictamen[-training],p1tp)
t2

# la accuracy de la validación saldrá más baja que la del training ya que no se han usado estas instancias para crear
#el modelo
(t2[1,1]+t2[2,2])/sum(t2)

# ahora crearemos el árbol usando todas las variables explicativas
p2 = rpart(Dictamen~., data=dd[training,], method="class", parms = list(split="gini"))

p2

plot(p2)
text(p2, use.n=T)

p2l=predict(p2,data=dd[training,])
head(p2l)

p2lp=p2l[,2]
p2lp[p2l[,2]<0.5]=0
p2lp[p2l[,2]>=0.5]=1
t2l<-table(dd$Dictamen[training],p2lp)
t2l
(t2l[1,1]+t2l[2,2])/sum(t2l)

p2t = predict(p2,newdata=dd[-training,])
p2tp=p2t[,2]
p2tp[p2t[,2]<0.5]=0
p2tp[p2t[,2]>=0.5]=1
t2t<-table(dd$Dictamen[-training],p2tp)
t2t
(t2t[1,1]+t2t[2,2])/sum(t2t)

# si quisieramos hacer una árbol de regresión, se usaría como respuesta una variable numerica
r1<-rpart(Edad~.,dd[training,], method="anova")
plot(r1)
text(r1,use.n=T)
r1p=predict(r1)
#2 Antiguedad,Trabajo y 5 Edad
plot(dd[,c(2)])

points(dd[,c(2)],r1p,col="violet",pch=20)

  
# prune the tree
  pfit<- prune(r1, cp=   r1$cptable[which.min(r1$cptable[,"xerror"]),"CP"])
  #ejemplo cogiendo otro nodo
  pfit2<-prune(r1, cp=r1$cptable[5,"CP"])
  pfit<- prune(p2, cp=0.01160389) # from cptable   
  
  # plot the pruned tree
  plot(pfit, uniform=TRUE,
       main="Pruned Classification Tree for Credsco")
  text(pfit, use.n=TRUE, all=TRUE, cex=.8)
  
  
  printcp(pfit) # display the results
  plotcp(pfit) # visualize cross-validation results
  summary(pfit) # detailed summary of splits
  
  # create additional plots
  par(mfrow=c(1,2)) # two plots on one page
  rsq.rpart(pfit) # visualize cross-validation results  
  
  ##################################################
  # Random Forest prediction of Credsco Clean data #
  ##################################################
  
  library(randomForest)
  # the model works with a formula specifying the response variable or using the data
  fit <- randomForest(Dictamen ~ Edad + Antiguedad.Trabajo + Ingresos,   data=dd)
  
  print(fit) # view results
  importance(fit) # importance of each predictor 
  
  #otras formas de crearlo
  fit2 <- randomForest(Dictamen ~ .,   data=dd)
  fit2 <- randomForest(dd[,2:16], y = dd$Dictamen)
  print(fit2) # view results
  importance(fit2) # importance of each predictor 
  
  #con importance = T para que calcule la importancia de las explicativas
  #n tree se decide el numero de arboles a generar, lo ideal es que cada instancia vaya en varios árboles 
  #pero no en todos.
  #proximity , se calcula la proximidad entre filas
  fit3 <- randomForest(dd[,2:16], y = dd$Dictamen,importance = T, ntree=100, proximity = T)
  importance(fit3)
  ## Look at variable importance:
  round(importance(fit3), 2)
  
  print(fit3)
  
  varImpPlot(fit3)
  varImpPlot(fit3, sort=F)
  varImpPlot(fit3, sort=T)
  
  
  #interesting attributes
  fit3$confusion
  fit3$votes
  
  prediction<-fit3$confusion
  (prediction[1,1]+prediction[2,2])/sum(prediction)
  
  #Usando training/test para validar.
  fit4<- randomForest(dd[training,2:16], y = dd$Dictamen[training], xtest = dd[-training, 2:16], ytest=dd$Dictamen[-training],importance = T, ntree=100, proximity = T)
  ptrain<-fit4$confusion
  (ptrain[1,1]+ptrain[2,2])/length(training)
  (ptrain[1,1]+ptrain[2,2])/sum(ptrain[1:2,1:2])
  
  #dentro de fit4$test hay otro objeto con la info de las filas de test/validacion
  #  predicted, err.rate, confusion, votes para clasificación
  ptest<-fit4$test$confusion
  (ptest[1,1]+ptest[2,2])/(nrow(dd)-length(training))
  (ptest[1,1]+ptest[2,2])/sum(ptest[1:2,1:2])
  
  
    
  #para ver uno de los árboles generados:
  
  getTree(randomForest(iris[,-5], iris[,5], ntree=10), 3, labelVar=TRUE)
  getTree(fit3, labelVar = T)

  
  ## The `unsupervised' case:
  set.seed(17)
  iris.urf <- randomForest(iris[, -5])
  
  MDSplot(iris.urf, iris$Species)
  
  #no lo ehecuto pq tarda demasiado, se ha de poner proximity=T en clasificacion al generar lso a´árboles
  #MDSplot(fit3, dd$Dictamen)
  
  
  # see the sizes of the generated trees
  treessize(fit3)
  ## Grow no more than 4 nodes per tree:
  (treesize(randomForest(Species ~ ., data=iris, maxnodes=4, ntree=30)))
  
   
  #other trees
  library(tree)
  tree_fit <- ctree(survived ~ ., data = .data$training)
  