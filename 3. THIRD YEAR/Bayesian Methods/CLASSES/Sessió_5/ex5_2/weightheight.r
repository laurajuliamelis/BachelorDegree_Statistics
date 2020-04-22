

path.bug <- "E:/metodes bayesians/sessio 5/WeightHeight/"

# instaling the library R2jags

install.packages('R2jags')
library(R2jags)





########### exercise 5.2   weight-height-gender

##### read data

Data <- read.table(paste(path.bug,"WeightHeight.txt",sep=""), dec=",", header=TRUE)
n <- dim(Data)[1] # simple size

plot(Data$weight, Data$height) # fem un grafic per veure com es distribueixen les dades



Iter <- 5000 # iteracions que farem
Burn <- 1000 # iteracions que farem abans que no comptaran per res
Chain <- 2 # numero de vegades que ho farem, per tant en total hi haura 10000 iteracions



########   linear regression


data <- list(n=n, y = Data$weight, x = Data$height)

initials <- list(list(tau=1, b0=0, b1=0),list(tau=0.1, b0=10, b1=10))

parameters <- c("b0", "b1", "tau")


model.M1 <- jags(data, initials, parameters.to.save=parameters, 
      model=paste(path.bug,"M1.bug",sep=""),               
      n.iter=(Iter+Burn),n.burnin=Burn, n.thin=1, n.chains=Chain)       


traceplot(model.M1, mfrow = c(1,1), varname = c("b0", "b1", "tau"), col=c("black","red"))
        
print(model.M1)

# El valor de DIC=134.6 es un valor d'estimar la prediccio, quan més baix es millor es com la
# AIC
attach.jags(model.M1)

 sigma <- sqrt(1/tau)

 
 par(mfrow=c(2,2))
#DIstribucions a posteriori de cada parametre
	plot(density(b0, adjust = 1.5), main = expression(paste(pi,"(",beta[0],"|y)")), xlab= "" ); abline(v=quantile(b0,c(0.025,0.975)),lty=3)
	plot(density(b1, adjust = 1.5), main = expression(paste(pi,"(",beta[1],"|y)")), xlab= ""  ); abline(v=quantile(b1,c(0.025,0.975)),lty=3)
	plot(density(sigma, adjust = 1.5), main = expression(paste(pi,"(",sigma,"|y)")), xlab= "" ); abline(v=quantile(sigma,c(0.025,0.975)),lty=3)
#Estimacio puntual de la recta de regressio
      plot(c(1.55,1.85), c(50,100), type = "n", xlab = "height", ylab = "weight" )
 	 points(Data$height, Data$weight, pch=19, col="gray50")
 	 abline(coef=c(mean(b0),mean(b1)))
# si volem fer prediccio fora del rang on l'acabe, de fer, poden surtir valors sense sentit.
detach.jags()


########   linear regression with a dicothomy co-variable


data <- list(n=n, y = Data$weight, x1 = Data$height, x2 = Data$sex)

initial <- list(list(tau=1, b0=0, b1=0, b2=10),list(tau=0.1, b0=10, b1=10, b2=-10))

parameters <- c("b0", "b1", "b2", "tau")

model.M2 <- jags(data, initials, parameters.to.save=parameters, 
      model=paste(path.bug,"M2.bug",sep=""),               
      n.iter=(Iter+Burn),n.burnin=Burn, n.thin=1, n.chains=Chain)       

traceplot(model.M2, mfrow = c(1,1), varname = c("b0", "b1", "tau"), col=c("black","red"))

print(model.M2)

attach.jags(model.M2)

 sigma <- sqrt(1/tau)

 par(mfrow=c(3,2))
#A posteriori
	plot(density(b0, adjust = 1.5), main = expression(paste(pi,"(",beta[0],"|y)")), xlab= "" ); abline(v=quantile(b0,c(0.025,0.975)),lty=3)
	plot(density(b1, adjust = 1.5), main = expression(paste(pi,"(",beta[1],"|y)  ; height")), xlab= ""  ); abline(v=quantile(b1,c(0.025,0.975)),lty=3)
	plot(density(b2, adjust = 1.5), main = expression(paste(pi,"(",beta[2],"|y)  ; sex")), xlab= ""  ); abline(v=quantile(b2,c(0.025,0.975)),lty=3)
	plot(density(sigma, adjust = 1.5), main = expression(paste(pi,"(",sigma,"|y)")), xlab= "" ); abline(v=quantile(sigma,c(0.025,0.975)),lty=3)
#Rectes de regressio
plot(c(1.55,1.85), c(50,100), type = "n", xlab = "height", ylab = "weight" )
 points(Data$height[Data$sex==0], Data$weight[Data$sex==0], pch=19, col="gray80")
 points(Data$height[Data$sex==1], Data$weight[Data$sex==1], pch=19, col="gray50")
 abline(coef=c(mean(b0),mean(b1)), lty=2)
 abline(coef=c(mean(b0)+mean(b2),mean(b1)), lty=3)
 legend("topleft", c("women","men"), lty=c(2,3))

detach.jags()



########   linear regression with a dicothomy co-variable and interaction


data <- list(n=n, y = Data$weight, x1 = Data$height, x2 = Data$sex)

initials <- list(list(tau=1, b0=0, b1=0, b2=10, b3=-1),list(tau=0.1, b0=10, b1=10, b2=-10, b3=1))

parameters <- c("b0", "b1", "b2", "b3", "tau")

model.M3 <- jags(data, initials, parameters.to.save=parameters, 
      model=paste(path.bug,"M3.bug",sep=""),               
      n.iter=(Iter+Burn),n.burnin=Burn, n.thin=1, n.chains=Chain)       

traceplot(model.M3, mfrow = c(1,1), varname = c("b0", "b1", "tau"), col=c("black","red"))
   
print(model.M3)

attach.jags(model.M3)

 sigma <- sqrt(1/tau)

 par(mfrow=c(3,2))
  #A posteriori
	plot(density(b0, adjust = 1.5), main = expression(paste(pi,"(",beta[0],"|y)")), xlab= "" ); abline(v=quantile(b0,c(0.025,0.975)),lty=3)
	plot(density(b1, adjust = 1.5), main = expression(paste(pi,"(",beta[1],"|y)  ; height")), xlab= ""  ); abline(v=quantile(b1,c(0.025,0.975)),lty=3)
	plot(density(b2, adjust = 1.5), main = expression(paste(pi,"(",beta[2],"|y)  ; sex")), xlab= ""  ); abline(v=quantile(b2,c(0.025,0.975)),lty=3)
	plot(density(b3, adjust = 1.5), main = expression(paste(pi,"(",beta[3],"|y)  ; sex*height")), xlab= ""  ); abline(v=quantile(b3,c(0.025,0.975)),lty=3)
	plot(density(sigma, adjust = 1.5), main = expression(paste(pi,"(",sigma,"|y)")), xlab= "" ); abline(v=quantile(sigma,c(0.025,0.975)),lty=3)

# scatterplot wiht the two lines, men and women

plot(c(1.55,1.85), c(50,100), type = "n", xlab = "height", ylab = "weight" )
 points(Data$height[Data$sex==0], Data$weight[Data$sex==0], pch=19, col="gray80")
 points(Data$height[Data$sex==1], Data$weight[Data$sex==1], pch=19, col="gray50")
 abline(coef=c(mean(b0),mean(b1)), lty=2)
 abline(coef=c(mean(b0)+mean(b2),mean(b1)+mean(b3)), lty=3)
 legend("topleft", c("women","men"), lty=c(2,3))

detach.jags()






########   linear regression with a dicothomy co-variable and interaction, and different vairance


data <- list(n=n, y = Data$weight, x1 = Data$height, x2 = Data$sex)

initials <- list(list(tau1=1, tau2=1, b0=0, b1=0, b2=10, b3=-1),list(tau1=0.1, tau2=0.1, b0=10, b1=10, b2=-10, b3=1))

parameters <- c("b0", "b1", "b2", "b3", "tau1", "tau2")

model.M4 <- jags(data, initials, parameters.to.save=parameters, 
      model=paste(path.bug,"M4.bug",sep=""),               
      n.iter=(Iter+Burn),n.burnin=Burn, n.thin=1, n.chains=Chain)       

traceplot(model.M4, mfrow = c(1,1), varname = c("b0", "b1", "tau"), col=c("black","red"))
        
print(model.M4)

attach.jags(model.M4)


 sigma1 <- sqrt(1/tau1)
 sigma2 <- sqrt(1/tau2)


 par(mfrow=c(3,2))

	plot(density(b0, adjust = 1.5), main = expression(paste(pi,"(",beta[0],"|y)")), xlab= "" ); abline(v=quantile(b0,c(0.025,0.975)),lty=3)
	plot(density(b1, adjust = 1.5), main = expression(paste(pi,"(",beta[1],"|y)  ; height")), xlab= ""  ); abline(v=quantile(b1,c(0.025,0.975)),lty=3)
	plot(density(b2, adjust = 1.5), main = expression(paste(pi,"(",beta[2],"|y)  ; sex")), xlab= ""  ); abline(v=quantile(b2,c(0.025,0.975)),lty=3)
	plot(density(b3, adjust = 1.5), main = expression(paste(pi,"(",beta[3],"|y)  ; sex*height")), xlab= ""  ); abline(v=quantile(b3,c(0.025,0.975)),lty=3)
	plot(density(sigma1, adjust = 1.5), main = expression(paste(pi,"(",sigma[1],"|y) and ",pi,"(",sigma[2],"|y)")), xlab= "" )
	lines(density(sigma2, adjust = 1.5), main = expression(paste(pi,"(",sigma[2],"|y)")), xlab= "" )
	plot(density(sigma1/sigma2, adjust = 1.5), main = expression(paste(pi,"(",sigma[1],"/",sigma[2],"|y)")), xlab= "" ); abline(v=quantile(sigma1/sigma2,c(0.025,0.975)),lty=3)

detach.jags()





