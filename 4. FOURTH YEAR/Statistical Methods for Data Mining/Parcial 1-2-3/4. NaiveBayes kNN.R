

#Datos iris
dd<-iris
set.seed(2018)
test<-sample(1:nrow(dd),size = nrow(dd)/3)
dataTrain<-dd[-test,]
dataTest<-dd[test,]
#Naive Bayes
install.packages("naivebayes")
library(naivebayes)
nb <- naive_bayes(Species ~ ., data = dataTrain)
plot(nb, legend=T)

pred <- predict(nb, dataTest)
table(pred, dd[test,5])
tables(nb, 1:4)

nb_kernel <- naive_bayes(x = dataTrain[-5], y = dataTrain[ ,5], usekernel = TRUE)
plot(nb_kernel)

pred <- predict(nb_kernel, dataTest)
table(pred, dd[test,5])


#datos Credsco
#dd credscoClean
dd <- read.table("credscoClean.csv",header=T, sep=";");
#elimino una instancia que estaba a missing 
# no hacer esto...
dd[3310:4454,]<-dd[3311:4455,]
dd<-dd[1:4454,]
test<-sample(1:nrow(dd),size = nrow(dd)/3)
dataTrain<-dd[-test,]
dataTest<-dd[test,]

nb <- naive_bayes(Dictamen ~ ., data = dataTrain)
plot(nb, legend=T)
plot(nb, legend=T, which="Antiguedad.Trabajo")
plot(nb, legend=T, which="Registros")

pred <- predict(nb, dataTest)
table(pred, dd[test,1])
tables(nb, 1:4)

nb_kernel <- naive_bayes(x = dataTrain[-1], y = dataTrain[ ,1], usekernel = TRUE)

plot(nb_kernel, legend=T)
plot(nb_kernel, legend=T, which="Registros")
plot(nb_kernel, legend=T, which="Cargas.Patrimoniales")


pred <- predict(nb_kernel, dataTest)
table(pred, dataTest[,1])



#kNN
installed.packages("VIM")
library(VIM)
#truco para que classificar nuevos
#no tiene modelo

aux<-dd
aux[test,1]<-NA

result<-kNN(aux, metric="gower", variable = "Dictamen")
table(result$Dictamen[test], dd$Dictamen[test])


result<-kNN(aux, metric="gower", k=3)
table(result[test,1], dd[test,1])

result<-kNN(aux, metric="gower", k=1)
table(result[test,1], dd[test,1])


result<-kNN(aux, metric="gower", variable = "Dictamen", weightDist = T)
table(result$Dictamen[test], dd$Dictamen[test])


result<-kNN(aux, metric="gower", k=3,variable = "Dictamen", weightDist = T)
table(result[test,1], dd[test,1])

result<-kNN(aux, metric="gower", k=1,variable = "Dictamen", weightDist = T)
table(result[test,1], dd[test,1])

#regression



aux<-dd
aux[test,2]<-NA
result<-kNN(aux, metric="gower", k=1,variable = "Antiguedad.Trabajo", weightDist = T)
table(result[test,2], dd[test,2])
plot(result[test,2], dd[test,2])
plot(result[test,2]- dd[test,2])


y<-dd[test,2]
yp<-result[test,2]

mse<-sum((y-yp)^2)/(length(y))
rmse<-sqrt(mse)
aux2<-sum((y - mean(y))^2)/(length(y)) 
r.square<-1- mse/aux2

1-(sum((yp-y)^2)/sum((y-mean(y))^2))

