rm(list=ls())

##-- Load packages
library(FactoMineR)
library(effects)
library(car)

##-- Read data
setwd(...)                      # path
base <- read.csv2("NLSAH.csv")  # data

##-- Transform numeric to factor
base$sex <- factor(base$sex)
base$race <- factor(base$race)
base$income <- factor(base$income)
base$insurance <- factor(base$insurance)
base$arrested <- factor(base$arrested)
base$sleep <- factor(base$sleep)
base$glasses <- factor(base$glasses)
base$caffeine <- factor(base$caffeine)
base$Phealth <- factor(base$Phealth)

##-- Descriptive
vars=colnames(base)[-21]
windows(10,10)
par(mfrow=c(4,5),mar=c(3,3,3,1))
for (va in vars){
  if (!is.factor(base[,va])){
    boxplot(as.formula(paste0(va,"~Phealth")),base,main=va,col=c(2,3),horizontal=T)
  } else{
    plot(as.formula(paste0("Phealth~",va)),base,main=va,col=c(2,3))
  }
}

##-- Numeric description
catdes(base,21)

##-- Model 0 with Stepwise
base$resp <- as.numeric(as.character(base$Phealth))                      # Numeric response
summary(m0 <- step(glm(resp~.,base[,c(1:20,22)],family=binomial),direction="both",k=log(nrow(base))))
BIC(m0)
marginalModelPlots(m0) 
residualPlots(m0, layout=c(3, 3))
plot(allEffects(m0))


# Funcio per agregar
agregacio <- function(dfin,nve,formula1, formula2){
  # Exemple crida: dfage <- agregacio(elecc92,1,as.formula(uns~age),as.formula(pres~age))
  # Retorna dataframe: dfage amb age, m, ypos, yneg. 
  #
  # Formula1 ha de ser uns~var1+...+var_nve
  # Formula2 ha de ser resposta~var1+...+var_nve.Resposta ha de ser num?rica.
  # Requereix que dfin,data.frame d'entrada contingui una columna amb uns
  # Retorna: dfout, data.frame amb var1 , ..., var_nve, m, ypos, yneg.
  # V?lid per nve arbitrari > 0
  
  taulam <- xtabs(formula1,exclude=NULL,dfin,drop.unused.levels=TRUE);
  taulap <- xtabs(formula2,exclude=NULL,dfin,drop.unused.levels=TRUE);
  
  dfout <- as.data.frame(taulam);
  dfaux<- as.data.frame(taulap);
  
  names(dfout)[nve+1] <- "m";
  names(dfaux)[nve+1] <- "ypos";
  attach(dfaux);
  dfout <- data.frame( dfout, ypos );
  dfout$yneg <- dfout$m-dfout$ypos;
  attach(dfout);
  dfout <- dfout[m>0,]
  # 
  # Les var.explicatives covariables ara surten com factor: canviar-ho
  #   dfout$var_i <- as.numeric(levels(var_i))[var_i]
}

##-- Agregacio
base$ones <- rep(1,nrow(base))                                           # Auxiliar variable
dfbase <- agregacio(base,1,as.formula(ones~waist),as.formula(resp~waist))# Data.frame agreggate by waist (cintura)
summary(dfbase)                                                          # Descriptive
dfbase$waist <- as.numeric(levels(dfbase$waist))[dfbase$waist]           # To numeric

##-- Model 1 amb waist
m1a <- glm(cbind(ypos,yneg)~waist,data=dfbase,family=binomial)
summary(m1a)

##-- Goodness of fit
# Residuals
par(mfrow=c(2,2))
plot(m1a)

# Linearity
par(mfrow=c(1,1))
dfbase$olog <- log((dfbase$ypos+0.5)/(dfbase$yneg+0.5))
plot(dfbase$waist,dfbase$olog,pch=19)
lines(lowess(dfbase$waist,dfbase$olog,f=0.75),col=4,lwd=2)
lines(lowess(dfbase$waist,dfbase$olog,f=0.5),col=3)
lines(lowess(dfbase$waist,dfbase$olog,f=1.0),col=4)
points(dfbase$waist,m1a$linear.predictor,col=2,lwd=1.5)

# Linearity --> Package car
scatterplot(log((base$resp+0.05)/(1-base$resp+0.05))~base$waist)

# Effects
plot(allEffects(m1a),ask=FALSE)
residualPlots(m1a, layout=c(1, 2))
marginalModelPlots(m1a) 

##-- models with poly
m1.1 <- glm(resp~waist,data=base,family=binomial)
m1.2 <- glm(resp~poly(waist,2),data=base,family=binomial)
m1.3 <- glm(resp~poly(waist,3),data=base,family=binomial)
m1.4 <- glm(resp~poly(waist,4),data=base,family=binomial)

##-- Compare models
# Deviance
anova(m1.3,m1.4,test="Chisq")
anova(m1.2,m1.3,test="Chisq")
anova(m1.1,m1.2,test="Chisq")

# AIC and BIC
cbind(AIC(m1.1,m1.2,m1.3,m1.4),
      BIC=BIC(m1.1,m1.2,m1.3,m1.4)[,2])

##-- Goodness of fit
marginalModelPlots(m1.3) 
residualPlots(m1.3, layout=c(1, 2))
m1.3a <- glm(cbind(ypos,yneg)~poly(waist,3),data=dfbase,family=binomial)

##-- Waist categorical
base$c.waist=cut(base$waist,c(50,86,98,108,194))
summary(m1.5 <- glm(resp~c.waist,data=base,family=binomial))

##-- AIC and BIC
AIC(m1.5)
BIC(m1.5)

##-- Another recategorization
base$Iwaist=as.factor(ifelse(base$waist<=105,0,1))  # Different behaviour after 105?
summary(base$Iwaist)

##-- Interaction with waist (two slopes?)
summary(m1.6 <- glm(resp~waist*Iwaist,data=base,family=binomial))  # Model with interaction
linearHypothesis(m1.6,"waist + waist:Iwaist1 = 0")                 # Two slopes?                   
AIC(m1.6)
BIC(m1.6)

marginalModelPlots(m1.6) 
par(mfrow=c(2,2))
residualPlots(m1.6, layout=c(1, 2))
plot(allEffects(m1.6))

##-- With aggregated data
dfbase$Iwaist <- as.factor(ifelse(dfbase$waist<=105,0,1))
summary(dfbase$Iwaist)
summary(m1.6a <- glm(cbind(ypos,yneg)~waist*Iwaist,data=dfbase,family=binomial))

marginalModelPlots(m1.6a) 
residualPlots(m1.6a, layout=c(1, 2))
plot(allEffects(m1.6a))


## Definite model
m1 <- m1.6 

##-- Weight
summary(m2.0 <- glm(resp~weight,data=base,family=binomial))
summary(m2.1 <- glm(resp~waist*Iwaist+weight,data=base,family=binomial))
BIC(m2.1)

# Collinearity?
vif(m2.1)

##-- Income
summary(m3.0 <- glm(resp~income,data=base,family=binomial))
BIC(m3.0)

summary(m3.1 <- glm(resp~waist*Iwaist+income,data=base,family=binomial))
anova(m3.1)
BIC(m3.1)

marginalModelPlots(m3.1) 
residualPlots(m3.1, layout=c(1, 2))

plot(allEffects(m3.1))


##-- Income recode
base$incomeR <- base$income
base$incomeR[base$income==2|base$income==3]=2
base$incomeR[base$income==4|base$income==5]=3

summary(m3.2<-glm(resp~waist*Iwaist+incomeR,data=base,family=binomial))
anova(m3.2)
BIC(m3.2)

##-- Stepwise
summary(m4 <- step(glm(resp~.+waist:Iwaist,base[,c(1:20,22,25,26)],family=binomial),scope=list(lower=m3.2),k=log(nrow(base))))
BIC(m4)

marginalModelPlots(m4) 
residualPlots(m4, layout=c(3, 3))

plot(allEffects(m4))

##-- Remove cigars
summary(m5.1<-update(m4,.~.-cigars+poly(cigars,2)))
BIC(m5.1)

##-- Add cigars categorical
base$Icigars=as.factor(ifelse(base$cigars==0,0,ifelse(base$cigars<5,1,2)))
summary(base$Icigars)
summary(m5.2<-update(m4,.~.-cigars+Icigars,data=base[,c(1:20,22,25,26,27)]))
BIC(m5.2)

m5=m5.2

marginalModelPlots(m5) 
residualPlots(m5, layout=c(3, 3))
par(mfrow=c(2,2))
plot(m5)
par(mfrow=c(1,1))
plot(allEffects(m5))

##-- Observacions outliers i influents
outlierTest(m5)
influenceIndexPlot(m5,id=list(lab=row.names(base),vars=c("Cook", "Student","hat"), n=5))
influencePlot(m5)

# Remove one observation
base[1544,]
m5 <- update(m5,data=base[-1544,])
outlierTest(m5)
influenceIndexPlot(m5,id=list(lab=row.names(base),vars=c("Cook", "Student","hat"), n=5))

# Remove another observation
base[2946,]
m5 <- update(m5,data=base[-c(1544,2946),])
influencePlot(m5)

# Remove another observation
base[3685,]
m5 <- update(m5,data=base[-c(1544,2946,3685),])
influencePlot(m5)

# Remove another observation
base[294,]
m5 <- update(m5,data=base[-c(1544,2946,3685,294),])
influencePlot(m5)

marginalModelPlots(m5) 
residualPlots(m5, layout=c(3, 3))




