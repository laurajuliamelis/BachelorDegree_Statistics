##EXAMEN PARCIAL NOVEMBRE 2012##
##PROBLEMA 2##
setwd("C:/Users/paula.gusta/Desktop")
rust<-read.table("rust.txt", header=TRUE, sep=",")
##o amb clipboard des de l'excel copiant les dades##
##observem que name la considera Factor, i aixo no te sentit##
rownames(rust) <- rust$name
rownames(rust) 
rust<-rust[,-1] ##Eliminem la variable "name"
##La variable "Type" la passem a factor: 0=ordinary 1=non ordinary
rust$type<-factor(rust$type, labels=c("ordinary","non ordinary"))
str(rust)
##Observem que el data frame te 3 columnes, i preparat per treballar

###Apartat a###
#Gràfic de dispersió
attach(rust)
plot(age,rust$rust)
##Variable causal, es l'edat. L'oxidació esta en funció de l'edat.
##posem rust$rust, ja que la variable es diu igual que les dades.
##Veiem dos punts estranys: A la edat 0, molt de rust. I amb una edat de casi 20000 amb poc rust.
##Anem a identificar aquests dos punts.
identify(age,rust$rust)
##Aquesta funció, ens permet identificar l'observació anòmala.(Apropem el punter a la observació, clickem.. i posteriorment fem escape i ens apareixera el valor)
##Aixo es util quan tenim dues variables. Amb més no.

##Pero, ens demana que ho fem a través dels residus
g<-lm(rust ~ age,data=rust)
plot(g,which=1)
##Veiem les dues observacions rares.
which(rownames(rust) == "Allegan")
##Aquesta comanda t'indica el numero que correspon a Allegan
##Les eliminem perque no tenen el comportament de la resta, i podria veure's afectat la resta.
rust0<- rust[-c(2,61),]
##Treiem les dues a la vegada, perque si treus una abans, l'altre ja no sera la mateixa observacio!!!!

##Apartat b##
g <- lm(rust ~ age, data=rust0)
sg <- summary(g)
coef(g)
sg
sg$sigma^2

sg$r.squared
##Es significatiu ja que el p-valor del estadistic F es 3.871e-13
##Es inferior a 0.05 de manera que la regressió es significativa
##Aixo vol dir que: Beta1 no es 0.
##Si ho fos(beta1=0), la recta seria horitzontal. (NO SERVEIX X RES LA REGRESSIÓ)

##Apartat c##
plot(g,which=2)
shapiro.test(residuals(g))  # p-valor superior a 0.05 per tant acceptem la hipotesis de que segueixen normalitat

### apartat d##
confint(g,level=0.99)

###apartat e###

g0<- lm(rust~0+ offset(23 +0.0009*age),data=rust0)
anova(g0,g)
# acceptem la h_0 


# apartat f

predict(g, newdata=data.frame(age<-rust["Acfer_215","age"]), interval="prediction")  # tambe ho podriem fent mirant la BD

rust["Acfer_215", ]
predict(g, newdata=data.frame(age=19500),interval="prediction")


# g)

r1 <- lm(rust ~ age, data=rust0, subset=type=="ordinary")
r2 <- lm(rust ~ age, data=rust0, subset=type=="non ordinary")

plot(rust0$age[rust0$type=="ordinary"],rust0$rust[rust0$type=="ordinary"])
abline
plot(rust0$age[rust0$type=="ordinary"],rust0$rust[rust0$type=="ordinary"])


r1 <- lm(rust ~ age, data=rust0, subset=type=="ordinary")
r2 <- lm(rust ~ age, data=rust0, subset=type=="non ordinary")

idx <- rust$type == "ordinary"
plot(rust0$age,rust0$rust, pch=ifelse(idx,3,20),xlab="age", ylab="rust")
abline(r1)
abline(r2, lty=4,col="red")

# per poder comprovar si son paraleles feim : o una ancova o bé s'ha de construir la matriu de diseny per poder comparar les pendents.

# per contrastar la hipotesis de paral·lelisme fem:
# Model de la hipotesis alternativa --> ha de tenir 4 parametres, b0,b1 ordinaries i b0 i b1 de les no ordinaries.

# en forma de matriu ficarem:
table(rust0$type)
beta0.ord<-c(rep(1,47),rep(0,33))
beta1.ord<- c(rust0$age[rust0$type=="ordinary"],rep(0,33))
beta0.nord<-c(rep(0,47),rep(1,33))
beta1.nord<- c(rep(0,47),rust0$age[rust0$type=="non ordinary"])
cbind(beta0.ord,beta1.ord,beta0.nord,beta1.nord)

h1<- lm(rust~0 + beta0.ord + beta1.ord + beta0.nord +beta1.nord,data=rust0)
beta1<- beta1.ord + beta1.nord
h0<- lm(rust ~0 +beta0.ord  + beta0.nord +beta1,data=rust0)
anova(h0,h1)# per tant deduim que el pendent es diferents ino son paral·leles. 

# tambe podem fer el test de coincidencia:


