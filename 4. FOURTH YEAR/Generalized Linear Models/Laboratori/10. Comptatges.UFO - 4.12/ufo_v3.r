############################
# Inizialitation 
############################
##-- Remove objects in memory
rm(list=ls())

##-- Load packages
library(effects)
library(ggplot2)
library(gridExtra)
library(car)
library(AER)
library(MASS)
library(pscl)
library(data.table)
install.packages("R2admb")
install.packages("glmmADMB",repos=c("http://glmmadmb.r-forge.r-project.org/repos",getOption("repos")),
                 type="source")
library(glmmADMB)

############################
# Data 
############################
# State: Estat dels EEUU (50 categories)
# Period Quinqueni: (1:2000-2004, 2:2005-2009,3:2010-2014)
# Month: Mes de l'any (1-12)
# Weekday: Dia de la setmana (1:dilluns,...7:diumenge)
# Hora: Franja horària (H00-05, H06-11, H12-17, H18-23)
# Sights: Número d'avistaments


############################
# Depuration
############################
##-- Read data
setwd('...')
ufo <- read.csv2("UFOsights.csv")

##-- Covert to factors
ufo$Weekday <- factor(ufo$Weekday)
ufo$Month <- factor(ufo$Month)
ufo$Period <- factor(ufo$Period)

##-- Grouped states by region using States data
# Remove one State (Puerto Rico and change the number of another)
ufo$State <- as.character(ufo$State)
ufo2 <- ufo[ufo$State!='PR',]
ufo2$State[ufo2$State=='CT'] <- 'CN'

##-- Load States data
data(States)
States2 <- data.frame(State=rownames(States),Region=States$region)

##-- Merge and create "Region" variable
ufo3 <- merge(ufo2,States2,by='State')
ufo3 <- as.data.table(ufo3)
ufo4 <- ufo3[,.(Sights=sum(Sights)),by=.(Region,Period,Month,Weekday,Hour)]

##-- Add zeros to the hours without sights
df <- expand.grid(unique(ufo4$Region),unique(ufo4$Period),unique(ufo4$Month),unique(ufo4$Weekday),unique(ufo4$Hour))
names(df) <- names(ufo4)[1:5]
ufo5 <- merge(ufo4,df,by=names(ufo4)[1:5],all.y=TRUE)
ufo5$Sights[is.na(ufo5$Sights)] <- 0


##-- Renamed data
ufo <- ufo5 # 9072 files perquè hi ha 9*3*12*7*4=9072 combinacions.

############################
# Descriptiva
############################
##-- Numerical univariant
summary(ufo)

##-- Graphical univariant (per veure que està equilibrat i q no ens hem deixat cap cas)
par(mfrow=c(2,2),las=1)
barplot(table(ufo$Period),main='Period')
barplot(table(ufo$Month),main='Month')
barplot(table(ufo$Weekday),main='Weekday')
barplot(table(ufo$Hour),main='Hour')

##-- Graphical by sights
# Boxplots
par(mfrow=c(2,2),las=1)
boxplot(I(Sights+0.1)~Period,ufo,log='y')
boxplot(I(Sights+0.1)~Month,ufo,log='y')
boxplot(I(Sights+0.1)~Weekday,ufo,log='y')
boxplot(I(Sights+0.1)~Hour,ufo,log='y')

# Barplots
gg1 <- ggplot(ufo, aes(x=Period,y=Sights)) + geom_col()
gg2 <- ggplot(ufo, aes(x=Month,y=Sights)) + geom_col()
gg3 <- ggplot(ufo, aes(x=Weekday,y=Sights)) + geom_col()
gg4 <- ggplot(ufo, aes(x=Hour,y=Sights)) + geom_col()
grid.arrange(gg1,gg2,gg3,gg4,ncol=2)

ggplot(ufo, aes(x=Region,y=Sights)) + geom_col()




############################
# Poisson model
############################
##-- Initial model with all first order interactions except for Region
m1 <- glm(Sights ~ Region + (Period+Weekday+Month+Hour)^2,ufo,family=poisson)
summary(m1)
anova(m1)

##-- Stepwise
m2 <- stats::step(m1, direction="both", k=log(nrow(ufo)))
summary(m2)

##-- Validation
par(mfrow=c(2,2))
plot(m2)

##-- Effects
par(mfrow=c(1,1))
plot(allEffects(m2))

##-- Observed vs. predict
par(mfrow=c(1,2))
plot(fitted(m2),ufo$Sights)
abline(c(0,1),col=2,lwd=2)
marginalModelPlot(m2)

##-- Residus according categories
residualPlots(m2)


##-- Influence and outliers
influenceIndexPlot(m2,id.n=5)
(ou <- outlierTest(m2))
sel <- as.numeric(names(ou$rstudent))
ufo[sel,]

##-- Removing outliers
m3 <- update(m2,data=ufo[-sel,])
par(mfrow=c(2,2))
plot(m3)
par(mfrow=c(1,1))
plot(allEffects(m3))


############################
# Overdispersion
############################
dispersiontest(m2,trafo=1)
dispersiontest(m2,trafo=2)

############################
# Quasi-poisson
############################
##-- Model
m.qp <- glm(Sights ~ Region +  Period*Weekday + 
              Period*Month + Period*Hour + Month*Hour, family = quasipoisson,data = ufo)
summary(m.qp)

##-- Validation
par(mfrow=c(2,2))
plot(m.qp)

############################
# Negative binomial
############################
##-- Model
m4a <- glm.nb(Sights ~ Region +  Period*Weekday + 
                Period*Month + Period*Hour + Month*Hour, ufo)

m4 <- glm(Sights ~ Region +  Period*Weekday + 
            Period*Month + Period*Hour + Month*Hour, ufo, family=neg.bin(m4a$theta))
summary(m4)
Anova(m4,test="F")

##-- Validacio
par(mfrow=c(2,2))
plot(m4)

par(mfrow=c(1,1),las=1)
residualPlots(m4)

plot(allEffects(m4))


##-- Model comparison
AIC(m2,m4)

par(mfrow=c(1,2))
plot(fitted(m2),ufo$Sights,main='Poisson')
lines(lowess(ufo$Sights~fitted(m2)),col=3)
abline(c(0,1),col=2,lwd=2)

plot(fitted(m4),ufo$Sights,main='Negative Binomial')
lines(lowess(ufo$Sights~fitted(m4)),col=3)
abline(c(0,1),col=2,lwd=2)

##-- Mena Quadratic error
sum((fitted(m2)-ufo$Sights)^2)
sum((fitted(m4)-ufo$Sights)^2)

############################
# Negative binomial zero inflated
############################
##-- Don't converge with pscl package
m5 <- zeroinfl(Sights ~ Region +  Period*Weekday + 
                 Period*Month + Period*Hour + Month*Hour, ufo, dist='negbin',
                 link='logit',
               control = zeroinfl.control(method="SANN",EM=FALSE,trace=TRUE,maxit = 1000)) 

##-- Try another package (be patient...)
m5 <- glmmadmb(Sights ~ Region +  Period*Weekday + 
                 Period*Month + Period*Hour + Month*Hour, ufo,
                        zeroInflation=TRUE,
                        family="nbinom")
summary(m5)

##-- Model comparison
AIC(m2,m4,m5)

##-- Predicted vs.observed
par(mfrow=c(1,3))
plot(fitted(m2),ufo$Sights,main='Poisson')
lines(lowess(ufo$Sights~fitted(m2)),col=3)
abline(c(0,1),col=2,lwd=2)

plot(fitted(m4),ufo$Sights,main='Negative Binomial')
lines(lowess(ufo$Sights~fitted(m4)),col=3)
abline(c(0,1),col=2,lwd=2)

plot(fitted(m5),ufo$Sights,main='ZI - Negative Binomial')
lines(lowess(ufo$Sights~fitted(m5)),col=3)
abline(c(0,1),col=2,lwd=2)

##-- Coefficients
round(cbind(coef(m2),coef(m4),coef(m5)),2)
sum(round(coef(m4),2)!=round(coef(m5),2))
