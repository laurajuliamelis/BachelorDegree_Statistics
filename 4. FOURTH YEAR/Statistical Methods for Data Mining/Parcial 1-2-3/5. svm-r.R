library(e1071)
library(rpart)
library(mlb)
#Linear

set.seed(2018)
x = matrix(rnorm(40), 20, 2)
y = rep(c(-1, 1), c(10, 10))
x[y == 1,] = x[y == 1,] + 1
plot(x, col = y + 3, pch = 19)

dat = data.frame(x, y = as.factor(y))
svmfit = svm(y ~ ., data = dat, kernel = "linear", cost =10000, scale = FALSE)
print(svmfit)

plot(svmfit, dat)


make.grid = function(x, n = 75) {
  grange = apply(x, 2, range)
  x1 = seq(from = grange[1,1], to = grange[2,1], length = n)
  x2 = seq(from = grange[1,2], to = grange[2,2], length = n)
  expand.grid(X1 = x1, X2 = x2)
}
# vemos los vectores soporte
xgrid = make.grid(x)
xgrid[1:10,]
ygrid = predict(svmfit, xgrid)
plot(xgrid, col = c("red","blue")[as.numeric(ygrid)], pch = 20, cex = .2)
points(x, col = y + 3, pch = 19)
points(x[svmfit$index,], pch = 5, cex = 2)

#para ver como lo ha separado
beta = drop(t(svmfit$coefs)%*%x[svmfit$index,])
beta0 = svmfit$rho

plot(xgrid, col = c("red", "blue")[as.numeric(ygrid)], pch = 20, cex = .2)
points(x, col = y + 3, pch = 19)
points(x[svmfit$index,], pch = 5, cex = 2)
abline(beta0 / beta[2], -beta[1] / beta[2])
abline((beta0 - 1) / beta[2], -beta[1] / beta[2], lty = 2)
abline((beta0 + 1) / beta[2], -beta[1] / beta[2], lty = 2)

#non-linear

load(file = "data/ESL.mixture.rda")
names(ESL.mixture)
rm(x, y)
attach(ESL.mixture)
plot(x, col = y + 1)
dat = data.frame(y = factor(y), x)
fit = svm(factor(y) ~ ., data = dat, scale = FALSE, kernel = "radial", cost = 5)
xgrid = expand.grid(X1 = px1, X2 = px2)
ygrid = predict(fit, xgrid)
plot(xgrid, col = as.numeric(ygrid), pch = 20, cex = .2)
points(x, col = y + 1, pch = 19)

func = predict(fit, xgrid, decision.values = TRUE)
func = attributes(func)$decision

xgrid = expand.grid(X1 = px1, X2 = px2)
ygrid = predict(fit, xgrid)
plot(xgrid, col = as.numeric(ygrid), pch = 20, cex = .2)
points(x, col = y + 1, pch = 19)

contour(px1, px2, matrix(func, 69, 99), level = 0, add = TRUE)
contour(px1, px2, matrix(func, 69, 99), level = 0.5, add = TRUE, col = "blue", lwd = 2)

#data breast cancer two classes

cancer <- read.csv("data/breast-cancer-wisconsin.data", header = F)
colnames(cancer)<- c("id", "Clump Thickness", "Uniformity of Cell Size","Uniformity of Cell Shape", "Marginal Adhesion", "Single Epithelial Cell Size",
"Bare Nuclei","Bland Chromatin","Normal Nucleoli", "Mitoses", "Class")
cancer$Class<-factor(cancer$Class)
dat<-cancer[,-1]
svm.model <- svm(Class ~ ., data = dat, cost = 100, gamma = 1)
svm.pred <- predict(svm.model, dat[,-10])
t2<-table(svm.pred, dat$Class)
n<-nrow(dat)
errorRate <- 1-(sum(diag(t2))/n)
t2
errorRate

plot(cmdscale(dist(dat[,-10])),
     col = as.integer(dat[,10]),
     pch = c("o","+")[1:nrow(dat) %in% svm.model$index + 1])

index <- 1:nrow(dat)
set.seed(2018)
testindex <- sample(index, trunc(length(index)/3))
testset <- dat[testindex,]
trainset <- dat[-testindex,]

svm.model <- svm(Class ~ ., data = trainset, cost = 100, gamma = 1)
svm.pred <- predict(svm.model, testset[,-10])
t2<-table(svm.pred, testset$Class)
n<-nrow(testset)

errorRate <- 1-(sum(diag(t2))/n)
t2
errorRate

plot(cmdscale(dist(trainset[,-10])),
     col = as.integer(trainset[,10]),
     pch = c("o","+")[1:nrow(trainset) %in% svm.model$index + 1])

#coste 5 y decrease gamma
svm.model <- svm(Class ~ ., data = trainset, cost = 5, gamma = 0.1)
svm.pred <- predict(svm.model, testset[,-10])
t2<-table(svm.pred, testset$Class)
n<-nrow(testset)

errorRate <- 1-(sum(diag(t2))/n)
t2
errorRate

plot(cmdscale(dist(trainset[,-10])),
     col = as.integer(trainset[,10]),
     pch = c("o","+")[1:nrow(trainset) %in% svm.model$index + 1])

# rb
svm.model <- svm(Class ~ ., data = trainset, cost = 10, kernel="radial", gamma = 0.1)
svm.pred <- predict(svm.model, testset[,-10])
t2<-table(svm.pred, testset$Class)
n<-nrow(testset)

errorRate <- 1-(sum(diag(t2))/n)
errorRate

#los escala y dibuja en unos nuevas coordenadas
# en+ los support vectors
plot(cmdscale(dist(trainset[,-10])),
     col = as.integer(trainset[,10]),
     pch = c("o","+")[1:nrow(trainset) %in% svm.model$index + 1])


# rb
svm.model <- svm(Class ~ ., data = trainset, cost = 10, kernel="polynomial",degree=2,  gamma = 0.1)
svm.pred <- predict(svm.model, testset[,-10])
t2<-table(svm.pred, testset$Class)
n<-nrow(testset)

errorRate <- 1-(sum(diag(t2))/n)
errorRate
t2
#los escala y dibuja en unos nuevas coordenadas
# en+ los support vectors
plot(cmdscale(dist(trainset[,-10])),
     col = as.integer(trainset[,10]),
     pch = c("o","+")[1:nrow(trainset) %in% svm.model$index + 1])

plot(cmdscale(dist(testset[,-10])),
     pch = as.integer(testset[,10]),
     col = as.integer(svm.pred))


# multi-classs


data(Glass)
glass <- read.csv("data/glass.data", header = F)
glass<-glass[,-1]
## split data into a train and test set
index <- 1:nrow(glass)
set.seed(2018)

colnames(glass)<- c("RI","Na","Mg", "Al","Si","K","Ca", "Ba", "Fe", "Type")

glass$Type<-factor(glass$Type, levels = c(1:7))
#, labels=c("building_windows_float_processed",
#                                                         "building_windows_non_float_processed",
#                                                         "vehicle_windows_float_processed",
#                                                         "vehicle_windows_non_float_processed",
#                                                         "containers",
#                                                         "tableware",
#                                                         "headlamps"))
testindex <- sample(index, trunc(length(index)/3))
testset <- glass[testindex,]
trainset <- glass[-testindex,]
## svm
svm.model <- svm(Type ~ ., data = trainset, cost = 100, gamma = 1)
svm.pred <- predict(svm.model, testset[,-10])
t2<-table(svm.pred, testset$Type)
n<-nrow(testset)

errorRate <- 1-(sum(diag(t2))/n)

# rb
svm.model <- svm(Type ~ ., data = trainset, cost = 10, kernel="radial", gamma = 0.1)
svm.pred <- predict(svm.model, testset[,-10])
t2<-table(svm.pred, testset$Type)
n<-nrow(testset)

errorRate <- 1-(sum(diag(t2))/n)
errorRate


plot(cmdscale(dist(trainset[,-10])),
     col = as.integer(trainset[,10]),
     pch = c("o","+")[1:nrow(trainset) %in% svm.model$index + 1])

plot(svm.model, trainset)
#iriss

x <- subset(iris, select = -Species)
y <- iris$Species
model <- svm(x, y) 

print(model)
summary(model)

# test with train data
pred <- predict(model, x)
# (same as:)
pred <- fitted(model)

# Check accuracy:
table(pred, y)

# compute decision values and probabilities:
pred <- predict(model, x, decision.values = TRUE)
attr(pred, "decision.values")[1:4,]

# visualize (classes by color, SV by crosses):
plot(cmdscale(dist(iris[,-5])),
     col = as.integer(iris[,5]),
     pch = c("o","+")[1:150 %in% model$index + 1])



#datos mixtos
dd <- read.table("credscoClean.csv",header=T, sep=";");
#elimino una instancia que estaba a missing 
# no hacer esto...
dd[3310:4454,]<-dd[3311:4455,]
dd<-dd[1:4454,]
test<-sample(1:nrow(dd),size = nrow(dd)/3)
dataTrain<-dd[-test,]
dataTest<-dd[test,]

svm.model <- svm(Dictamen ~ ., data = dataTrain, cost = 10, kernel="radial", gamma = 0.1)
svm.pred <- predict(svm.model, dataTest[,-1])
t2<-table(svm.pred, dataTest$Dictamen)
t2
n<-nrow(testset)

errorRate <- 1-(sum(diag(t2))/n)
errorRate


svm.model <- svm(Antiguedad.Trabajo ~ ., data = dataTrain, cost = 10, kernel="radial", gamma = 0.1)
svm.pred <- predict(svm.model, dataTest[,-2])
t2<-table(svm.pred, dataTest$Antiguedad.Trabajo)
t2
n<-nrow(testset)

y<-dataTest[,2]
yp<-svm.pred

mse<-sum((y-yp)^2)/(length(y))
rmse<-sqrt(mse)
aux2<-sum((y - mean(y))^2)/(length(y)) 
r.square<-1- mse/aux2

1-(sum((yp-y)^2)/sum((y-mean(y))^2))


svm.model <- svm(Antiguedad.Trabajo ~ ., data = dataTrain, cost = 10, kernel="radial", gamma = 0.1)
svm.pred <- predict(svm.model, dataTest[,-2])
t2<-table(svm.pred, dataTest$Antiguedad.Trabajo)
t2
n<-nrow(testset)

y<-dataTest[,2]
yp<-svm.pred

mse<-sum((y-yp)^2)/(length(y))
rmse<-sqrt(mse)
aux2<-sum((y - mean(y))^2)/(length(y)) 
r.square<-1- mse/aux2

1-(sum((yp-y)^2)/sum((y-mean(y))^2))

