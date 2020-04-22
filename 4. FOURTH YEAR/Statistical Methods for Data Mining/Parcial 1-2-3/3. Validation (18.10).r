

#Las predicciones pueden salir como probabilidad en lugar de 1 o 0
p1t = predict(p1,newdata=dd[-training,])
p1t
# en este caso, mis valores positivos estan en la segunda columna
p1tp=p1t[,2]
#los binarizo eligiendo una prob de 0.5, los que esten por arriba serán positivos y los que no negativos
p1tp[p1t[,2]<0.5]=0
p1tp[p1t[,2]>=0.5]=1


#Confusion matrix
#table( predictions,  real.values)
t2<-table(p1tp, dd$Dictamen[-training])
# en las filas tengo los valores predecidos y en las columnas los reales

t2

# p1tp negatiu positiu
#0     107  =TN   104 =FN
#1     301  =FP   973  = TP

tp<-t2[2,2]
tn<-t2[1,1]
fp<-t2[2,1]
fn<-t2[1,2]
n<-length(dd$Dictamen[-training])

#error rate (fp+fn)/n
errorRate <- (t2[1,2]+t2[2,1])/sum(t2)
errorRate <- (fp+fn)/n
#more than 2 categories

errorRate <- 1-(sum(diag(t2))/n)

#accuracy (tp+tn)/n or 1-error rate
accuracy <- (t2[1,1]+t2[2,2])/sum(t2)
accuracy <- 1- errorRate
accuracy <- (tp+tn)/n

#more than 2 categories
accuracy <-(sum(diag(t2))/sum(t2))
# la accuracy de la validación saldrá más baja que la del training ya que no se han usado estas instancias para crear
#el modelo


#recall(sensitivity)  
rcall = tp/(tp+fn)

#precision
precision = tp /(tp+fp)
#specificity
spec = tn/(tn+fp)

#f-measure (f-score)
fscore = (2*precision*rcall)/(precision+rcall)

#for numerical variables
r1p #no las tengo calculadas
out.real<-dd$Edad[-training]
out.pred<-r1p

mse<-sum((out.real-out.pred)^2)/(nrow(dd[-training,])) 
rmse<-sqrt(mse)
aux<-sum((out.pred - mean(out.pred))^2)/(nrow(dd[-training,])) 
r.square<-1- mse/aux

#otra forma

r.square<-1-(sum((out.pred-out.real)^2)/sum((out.pred-mean(out.pred))^2))


#categoricas binarias  p1t

library(ROCR)

predict.roc<-prediction(p1t[,2], dd$Dictamen[-training])
#predict.roc
#tpr = recall(tp/(tp+fn)) fpr=FP/(FP+TN)
perf.roc<- performance(predict.roc,"tpr","fpr") #True y False postivie.rate


# GRAFICO CURVA ROC
#------------------------------------------------------------------------------
auc <- as.numeric(performance(predict.roc ,"auc")@y.values)
plot(perf.roc,type='o', main = paste('Area Bajo la Curva =',round(auc,2)))  
abline(a=0, b= 1)



#igual para p2t
predict.roc2<-prediction(p2t[,2], dd$Dictamen[-training])
perf.roc2     <- performance(predict.roc2,"tpr","fpr") #True y False postivie.rate


# GRAFICO CURVA ROC
#------------------------------------------------------------------------------
auc2 <- as.numeric(performance(predict.roc2 ,"auc")@y.values)
plot(perf.roc2,type='o', main = paste('Area Bajo la Curva =',round(auc2,2)))  
abline(a=0, b= 1)

#plot(x=c(3/4,1/4,1/4, 0), y=c(1,4/4,2/4,1/4), type="l", xlab="FP Rate", ylab="TP Rate", main="ROC")
#abline(a=0, b= 1)

# GRAFICO ARBOL DECISION + bonito
#------------------------------------------------------------------------------

library(rattle)
fancyRpartPlot(p2)     




# Bagging
#-----------------------------------------------

library(adabag)
library(rpart)
## rpart library should be loaded
#bagging(formula, data, mfinal = 100, minsplit = 5, cp = 0.01, maxdepth = nlevels(vardep))
dd.bagging <- bagging(Dictamen ~.,data=dd[training, ],mfinal=5, control=rpart.control(maxdepth=5, minsplit=15))
#Using the pruning option
dd.bagging.pred <- predict.bagging(dd.bagging,newdata=dd[-training, ], newmfinal=3)
dd.bagging.pred$confusion
dd.bagging.pred$error

#otros parametros

dd.bagging2 <- bagging(Dictamen ~.,data=dd[training, ], mfinal = 100, minsplit = 5, cp = 0.01, maxdepth = nlevels(vardep))
#Using the pruning option
dd.bagging.pred2 <- predict.bagging(dd.bagging2,newdata=dd[-training, ], newmfinal=3)
dd.bagging.pred2$confusion
dd.bagging.pred2$error

#respuesta multi-class

iris.bagging<-bagging(Species~., data=iris, mfinal=100)
iris.bagging.pred <- predict.bagging(iris.bagging,newdata=iris, newmfinal=3)
iris.bagging.pred$confusion
iris.bagging.pred$error


#respuesta numérica
#no te deja

#usando k-fold cross validation
## rpart library should be loaded
#en este caso no se puede analiar los errores
library(rpart)
dd.baggingcv <- bagging.cv(Dictamen ~ ., v=10, data=dd, mfinal=3, control=rpart.control(cp=0.01))
dd.baggingcv[-1]
dd.bagging.cv2 <- bagging.cv(Dictamen ~.,data=dd,v=5,mfinal=10, control=rpart.control(misplit=5))
dd.bagging.cv2[-1]


#ADABOOST
dd.boos<-boosting(Dictamen~., data=dd, boos = TRUE, mfinal = 100, coeflearn = 'Breiman', minsplit = 5, cp = 0.01, maxdepth = nlevels(vardep)) 

#datos credsco
mfinal <- 100
maxdepth <- 5
dd.rpart <- rpart(Dictamen~.,data=dd[training,],maxdepth=maxdepth)
dd.rpart.pred <- predict(dd.rpart,newdata=dd[-training, ],type="class")
tb <- table(dd.rpart.pred,dd$Dictamen[-training])
error.rpart <- 1-(sum(diag(tb))/sum(tb))
tb
error.rpart

# coeflearn, defines the method use "Breiman" or "Freund" for Adaboost.M1
dd.adaboost <- boosting(Dictamen ~.,data=dd[training, ],mfinal=mfinal, coeflearn="Breiman",
                             control=rpart.control(maxdepth=maxdepth))
dd.adaboost.pred <- predict.boosting(dd.adaboost,newdata=dd[-training, ])
dd.adaboost.pred$confusion
dd.adaboost.pred$error
#comparing error evolution in training and test set
errorevol(dd.adaboost,newdata=dd[training, ])->evol.train
errorevol(dd.adaboost,newdata=dd[-training, ])->evol.test

plot(evol.train$error, type="l")
lines(evol.test$error, col="red")
 
## rpart library should be loaded 
#iris contains 3 categories
data(iris)
iris.adaboost <- boosting(Species~., data=iris, boos=TRUE, mfinal=3)
iris.adaboost

# respuesta numerica
#no lo admite

#with k-fold cross validation
#es bastante lento

dd.adaboost.cv <- boosting.cv(Dictamen ~.,data=dd[training, ], v=5, mfinal=mfinal, coeflearn="Breiman",
                        control=rpart.control(maxdepth=maxdepth))
dd.adaboost.cv[-1]


margins(dd.bagging, dd[training,])
plot.margins(margins(dd.bagging, dd[training,]))
plot.margins(margins(dd.adaboost, dd[training,]))
