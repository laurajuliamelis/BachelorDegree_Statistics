




path.bug <- "E:/metodes bayesians/sessio 5/birthweight/"



# instaling the library R2jags

install.packages('R2jags')
library(R2jags)



##### read data

BirthWeight <- read.table(paste(path.bug,"birthweight.txt",sep=""), col.names=c("birhtweight", "age", "smoke"))
n <- dim(BirthWeight )[1] # grandaria mostra

plot(BirthWeight$age, BirthWeight$birhtweight)

#####  MODEL BAYESIA  #####


Iter <- 5000
Burn <- 1000
Chain <- 2



### en llença "Burn", i despres se'n queda "Iter" que les tria cada "Thins", es a dir en simmula Burn+Iter*Thin per 
### a cada cadena, nombre de cadenes es "Chain"




#########  Comparacio de mitjanes amb variancies diferents 




data <- list(n=n, y = BirthWeight$birhtweight, x = BirthWeight$smoke)

initials <- list(list(tau1=1, tau2=1, b0=0, b1=0),list(tau1=0.1, tau2=0.1, b0=10, b1=10))


parameters <- c("b0", "b1", "tau1", "tau2")


model.M1 <- jags(data, initials, parameters.to.save=parameters, 
      model=paste(path.bug,"M1.bug",sep=""),               
      n.iter=(Iter+Burn),n.burnin=Burn, n.thin=1, n.chains=Chain)       

traceplot(model.M1, mfrow = c(1,1), varname = c("b0", "b1", "tau"), col=c("black","red"))
 
print(model.M1, digits=6)

attach.jags(model.M1)

par(mfrow=c(2,2))

 sigma1 <- sqrt(1/tau1)
 sigma2 <- sqrt(1/tau2)
 
 plot(density(b0, adjust = 1.5), main = expression(paste(pi,"(",beta[0],"|y)")), xlab= "" ); abline(v=quantile(b0,c(0.025,0.975)),lty=3)
 plot(density(b1, adjust = 1.5), main = expression(paste(pi,"(",beta[1],"|y)")), xlab= ""  ); abline(v=quantile(b1,c(0.025,0.975)),lty=3)
 plot(density(sigma2, adjust = 1.5), main = expression(paste(pi,"(",sigma[1],"|y) and ",pi,"(",sigma[2],"|y)")), xlab= "" )
  lines(density(sigma1, adjust = 1.5), main = expression(paste(pi,"(",sigma[2],"|y)")), xlab= "" )
 plot(density(sigma1/sigma2, adjust = 1.5), main = expression(paste(pi,"(",sigma[1]/sigma[2],"|y)")), xlab= "" ); abline(v=quantile(sigma1/sigma2,c(0.025,0.975)),lty=3)

detach.jags()





######### Comparacio de mitjanes amb variances iguals   




data <- list(n=n, y = BirthWeight$birhtweight, x = BirthWeight$smoke)

initials <- list(list(tau=1, b0=0, b1=0),list(tau=0.1, b0=10, b1=10))

parameters <- c("b0", "b1", "tau")

model.M2 <- jags(data, initials, parameters.to.save=parameters, 
      model=paste(path.bug,"M2.bug",sep=""),               
      n.iter=(Iter+Burn),n.burnin=Burn, n.thin=1, n.chains=Chain)       

traceplot(model.M2, mfrow = c(1,1), varname = c("b0", "b1", "tau"), col=c("black","red"))






########   regressio lineal simple


data <- list(n=n, y = BirthWeight$birhtweight, x = BirthWeight$age)

initials <- list(list(tau=1, b0=0, b1=0),list(tau=0.1, b0=10, b1=10))

parameters <- c("b0", "b1", "tau")

model.M3 <- jags(data, initials, parameters.to.save=parameters, 
      model=paste(path.bug,"M2.bug",sep=""),               
      n.iter=(Iter+Burn),n.burnin=Burn, n.thin=1, n.chains=Chain)       

traceplot(model.M3, mfrow = c(1,1), varname = c("b0", "b1", "tau"), col=c("black","red"))
   
print(model.M3)

attach.jags(model.M3)

 sigma <- sqrt(1/tau)

 par(mfrow=c(2,2))

	plot(density(b0, adjust = 1.5), main = expression(paste(pi,"(",beta[0],"|y)")), xlab= "" ); abline(v=quantile(b0,c(0.025,0.975)),lty=3)
	plot(density(b1, adjust = 1.5), main = expression(paste(pi,"(",beta[1],"|y)")), xlab= ""  ); abline(v=quantile(b1,c(0.025,0.975)),lty=3)
	plot(density(sigma, adjust = 1.5), main = expression(paste(pi,"(",sigma,"|y)")), xlab= "" ); abline(v=quantile(sigma,c(0.025,0.975)),lty=3)

detach.jags()





########   regressio lineal simple més variable categòrica 


data<- list(n=n, y = BirthWeight$birhtweight, x1 = BirthWeight$smoke,  x2 = BirthWeight$age)

initials <- list(list(tau=1, b0=0, b1=0, b2=10),list(tau=0.1, b0=10, b1=10, b2=-10))



parameters <- c("b0", "b1", "b2", "tau")

model.M4 <- jags(data, initials, parameters.to.save=parameters, 
      model=paste(path.bug,"M4.bug",sep=""),               
      n.iter=(Iter+Burn),n.burnin=Burn, n.thin=1, n.chains=Chain)       

traceplot(model.M4, mfrow = c(1,1), varname = c("b0", "b1", "tau"), col=c("black","red"))
   
print(model.M4)

attach.jags(model.M4)

 sigma <- sqrt(1/tau)

 par(mfrow=c(2,2))

	plot(density(b0, adjust = 1.5), main = expression(paste(pi,"(",beta[0],"|y)")), xlab= "" ); abline(v=quantile(b0,c(0.025,0.975)),lty=3)
	plot(density(b1, adjust = 1.5), main = expression(paste(pi,"(",beta[1],"|y)  ; smoke")), xlab= ""  ); abline(v=quantile(b1,c(0.025,0.975)),lty=3)
	plot(density(b2, adjust = 1.5), main = expression(paste(pi,"(",beta[2],"|y)  ; age")), xlab= ""  ); abline(v=quantile(b2,c(0.025,0.975)),lty=3)
	plot(density(sigma, adjust = 1.5), main = expression(paste(pi,"(",sigma,"|y)")), xlab= "" ); abline(v=quantile(sigma,c(0.025,0.975)),lty=3)


detach.jags()



########   regressio lineal simple més variable categòrica més interacció



data <- list(n=n, y = BirthWeight$birhtweight, x1 = BirthWeight$smoke,  x2 = BirthWeight$age)

initials <- list(list(tau=1, b0=0, b1=0, b2=10, b12=-1),list(tau=0.1, b0=10, b1=10, b2=-10, b12=1))



parameters<- c("b0", "b1", "b2", "b12", "tau")

model.M5 <- jags(data, initials, parameters.to.save=parameters, 
      model=paste(path.bug,"M5.bug",sep=""),               
      n.iter=(Iter+Burn),n.burnin=Burn, n.thin=1, n.chains=Chain)       

traceplot(model.M5, mfrow = c(1,1), varname = c("b0", "b1", "tau"), col=c("black","red"))
   
print(model.M5)

attach.jags(model.M5)

 sigma <- sqrt(1/tau)

 par(mfrow=c(3,2))

	plot(density(b0, adjust = 1.5), main = expression(paste(pi,"(",beta[0],"|y)")), xlab= "" ); abline(v=quantile(b0,c(0.025,0.975)),lty=3)
	plot(density(b1, adjust = 1.5), main = expression(paste(pi,"(",beta[1],"|y)  ; smoke")), xlab= ""  ); abline(v=quantile(b1,c(0.025,0.975)),lty=3)
	plot(density(b2, adjust = 1.5), main = expression(paste(pi,"(",beta[2],"|y)  ; age")), xlab= ""  ); abline(v=quantile(b2,c(0.025,0.975)),lty=3)
	plot(density(b12, adjust = 1.5), main = expression(paste(pi,"(",beta[12],"|y)  ; age*smoke")), xlab= ""  ); abline(v=quantile(b12,c(0.025,0.975)),lty=3)
	plot(density(sigma, adjust = 1.5), main = expression(paste(pi,"(",sigma,"|y)")), xlab= "" ); abline(v=quantile(sigma,c(0.025,0.975)),lty=3)


detach.jags()












par(mfrow=c(2,2))

attach.jags(model.M3)

plot(c(10,50), c(500,6000), type = "n", xlab = "age", ylab = "birthweight" )
 points(BirthWeight$age, BirthWeight$birhtweight, pch=19, col="gray90")
 abline(coef=c(mean(b0),mean(b1)), lwd=2)
detach.jags()


attach.jags(model.M4)
plot(c(10,50), c(500,6000), type = "n", xlab = "age", ylab = "birthweight" )
 points(BirthWeight$age[BirthWeight$smoke==0], BirthWeight$birhtweight[BirthWeight$smoke==0], pch=19, col="gray90")
 points(BirthWeight$age[BirthWeight$smoke==1], BirthWeight$birhtweight[BirthWeight$smoke==1], pch=19, col="gray80")
 abline(coef=c(mean(b0),mean(b2)), lwd=2, lty=2)
 abline(coef=c(mean(b0)+mean(b1),mean(b2)), lty=3, lwd=2)
 legend("topleft", c("no somke","smoke"), lty=c(2,3))

detach.jags()


attach.jags(model.M5)
plot(c(10,50), c(500,6000), type = "n", xlab = "age", ylab = "birthweight" )
 points(BirthWeight$age[BirthWeight$smoke==0], BirthWeight$birhtweight[BirthWeight$smoke==0], pch=19, col="gray90")
 points(BirthWeight$age[BirthWeight$smoke==1], BirthWeight$birhtweight[BirthWeight$smoke==1], pch=19, col="gray80")
 abline(coef=c(mean(b0),mean(b2)), lwd=2, lty=2)
 abline(coef=c(mean(b0)+mean(b1),mean(b2)+mean(b12)), lwd=2, lty=3)
 legend("topleft", c("no somke","smoke"), lty=c(2,3))

detach.jags()


par(mfrow=c(2,2))



####  Interpretació per escenaris 





