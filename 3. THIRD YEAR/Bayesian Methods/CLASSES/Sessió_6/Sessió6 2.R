
path.bug <- "E:/Mètodes Bayesians/CLASSES/Sessió 6/"


# instaling the library R2jags

install.packages('R2jags')
library(R2jags)


####  CHANGE POINT  BINOMIAL  ANALYSIS  ####

# n1 nombre de mostres abans del punt de canvi
# n2 nombre de mostres després del punt de canvi
# p1 probabilitat èxit abans del punt de canvi
# p2 probabilitat èxit després del punt de canvi
# N1 vector amb la mida de cada mostra d'abans del punt de canvi
# N2 vector amb la mida de cada mostra després del punt de canvi

############### Tria dels paràmetres  #############

n1 <- 30; N1.mitjana <- 50
n2 <- 20; N2.mitjana <- 50
n <- n1+n2

# r és n1+1, primera mostra amb la moneda trucada 

p1 <- 0.5
p2 <- 0.7

cat("\n",
    "prob cara 1a moneda  ", p1, "\n",
    "prob cara 2a monbeda ", p2, "\n",
    "punt de canvi        ", n1+1,  "\n")


############## Generació dades  #############

N1 <- rpois(n1, N1.mitjana)  
N2 <- rpois(n2, N2.mitjana)

y1 <- rbinom(n1, size=N1, prob=p1)
y2 <- rbinom(n2, size=N2, prob=p2)

cbind(c(y1,y2),c(N1,N2))

par(mfrow=c(1,1))
plot(1:n,c(y1/N1,y2/N2), pch=19, ylab="% cares")




############## BUGS' model  #############

sink(paste(path.bug,"BinChangePoint.bug",sep=""))
cat("
    model  {
    
    for (i in 1:n) { 
    
    Y[i] ~ dbin(theta[I[i]] , N[i])
    
    I[i]<-1+step(i-r) # r is the change.point
    
    }
    
    theta[1]~dbeta(a1[1],a1[2])
    theta[2]~dbeta(a2[1],a2[2])
    
    r~dcat(p[])
    
    }
    ",fill=TRUE)
sink()


# PRIORIS

a1 <- c(1,1) # valors per la beta de theta1
a2 <- c(1,1) # valors per la beta de theta2
prob.r <- rep(1/n,n) # probabilititats per r

par(mfrow=c(1,3))
plot(function(x)dbeta(x,a1[1],a1[2]), xlab="",ylab="", main="priori de theta1")
plot(function(x)dbeta(x,a2[1],a2[2]), xlab="",ylab="", main="priori de theta2")
plot(1:n, prob.r, lwd=2, ty="h", xlab="",ylab="", main="prior de r")




## VALORS INICIALS 

dades <- list(n=n, Y = c(y1,y2), N = c(N1,N2), 
              p = prob.r, a1 = a1, a2 = a2)


inits <- list(list(theta =c(0.5,0.5), r=trunc(n*0.5)),
              list(theta =c(0.1,0.9), r=trunc(n*0.25)),
              list(theta =c(0.9,0.1), r=trunc(n*0.75)))

parameters <- c("theta", "r")

Iter <- 1500
Burn <- 100
Thin <- 1
Chain <- 3

### en llença "Burn", i despres se'n queda "Iter" que les tria cada "Thins", es a dir en simmula Burn+Iter*Thin per 
### a cada cadena, nombre de cadenes es "Chain"



canvi <- jags(dades, inits, parameters.to.save=parameters,
              model=paste(path.bug,"BinChangePoint.bug",sep=""),
              n.iter=(Iter+Burn),n.burnin=Burn, n.thin=1, n.chains=Chain)


traceplot(canvi , mfrow = c(1,1), varname = c("theta", "r"), col=c("black","red","yellow"))


print(canvi)


attach.jags(canvi )

table(r)
round(table(r)/sum(table(r)),4)

par(mfrow=c(1,2))

boxplot(theta[,1],theta[,2], col="blue")
plot(table(r)/sum(table(r)), xlim=c(min(as.numeric(names(table(r))))-1,max(as.numeric(names(table(r))))+1), main="change point", lwd=3)


detach.jags()







##################  GIBBS SAMPLING  #######################################################3



n1 <- 30; N1.mitjana <- 20
n2 <- 20; N2.mitjana <- 20
n <- n1+n2

# r és n1

p1 <- 0.5
p2 <- 0.7

cat("\n",
    "prob cara 1a moneda  ", p1, "\n",
    "prob cara 2a monbeda ", p2, "\n",
    "punt de canvi       ", n1,  "\n")





############## Generació dades

N1 <- rpois(n1, N1.mitjana)  
N2 <- rpois(n2, N2.mitjana)

y1 <- rbinom(n1, size=N1, prob=p1)
y2 <- rbinom(n2, size=N2, prob=p2)

cbind(c(y1,y2),c(N1,N2))

par(mfrow=c(1,1))
plot(1:n,c(y1/N1,y2/N2), pch=19, ylab="% cares")




y <- c(y1,y2)
N <- c(N1,N2)
a1 =c(1,1)
a2 =c(1,1)




#initialization
M = 10000
r.support = 1:n
r = rep(NA,M)
theta1 = rep(NA,M)
theta2 = rep(NA,M)
theta1[1] = 0.5
theta2[1] = 0.5
r[1] = 20


# Utilities
r.aus = function(r,p1,p2,y){
  if(r<n){
    aus1 = p1^(sum(y[1:r]))
    aus2 = (1-p1)^(sum(N[1:r]) - sum(y[1:r]))
    aus3 = p2^(sum(y[(r+1):n]))
    aus4 = (1-p2)^(sum(N[(r+1):n]) - sum(y[(r+1):n]))
    out = aus1*aus2*aus3*aus4
  }else{
    aus1 = p1^(sum(y[1:n]))
    aus2 = (1-p1)^(sum(N[1:n]) - sum(y[1:n]))
    out = aus1*aus2
  }
  return(out)
}



r.fc = function(p1,p2,y){
  vecc = rep(NA,n)
  for(i in 1:n){
    vecc[i] = r.aus(i,p1,p2,y)
  }
  out = vecc/sum(vecc)
  return(out)
}




# Gibbs Sampling
for(m in 2:M){
  theta1[m] = rbeta(1,a1[1] + sum(y[1:r[m-1]]), a1[2] + sum(N[1:r[m-1]]) - sum(y[1:r[m-1]]))
  theta2[m] = rbeta(1,a2[1] + sum(y[(r[m-1]+1):n]), a2[2] + sum(N[(r[m-1]+1):n]) - sum(y[(r[m-1]+1):n]))
  r[m] = sample(r.support,size=1,prob=r.fc(theta1[m],theta2[m],y))
} 




# the convergence

par(mfrow=c(3,1))
plot(1:M,theta1,type="l")
plot(1:M,theta2,type="l")
plot(1:M,r,type="l")



r <- r[2:M]
theta1 <- theta1[2:M]
theta2 <- theta2[2:M]

# the posterior distributions

par(mfrow=c(1,2))

boxplot(theta1,theta2, col="blue")
plot(table(r)/sum(table(r)), xlim=c(min(as.numeric(names(table(r))))-1,max(as.numeric(names(table(r))))+1), main="change point", lwd=3)






####  regression - CHANGE POINT    ####

# a1 ordenada origen abans punt de canvi
# a2 ordenada origen després punt de canvi
# b1 pendent abans punt de canvi
# b2 pendent després punt de canvi
# r punt de canvi

############### Tria dels paràmetres

n <- 15
x <- 1:n 
r <- 10

a1 <- 150
b1 <- 1.5
b2 <- -1.5
sig <- 0.3 

???????????????????????????

par(mfrow=c(1,1))

plot(x, c(y1,y2), main="evolució taxes x 100.000",ylab="taxa",xlab="any")



############## BUGS' model 

sink(paste(path.bug,"regressioPuntCanvi.bug",sep=""))
cat("
    model	{
    
    for (i in 1 : n) { 
    
    y[i] ~ dnorm(mu[i] , tau)
    mu[i] <- alpha[I[i]] + beta[I[i]]*x[i]
    I[i] <- 1 + step(x[i] - r) # r is the change.point
    
    }
    
    tau <- 1/(sigma*sigma)
    
    alpha[1] ~ dnorm(0.0, 1.0E-6)
    
    alpha[2] ??????????????
    
    beta[1] ~ dnorm(0.0, 1.0E-6)
    beta[2] ~ dnorm(0.0, 1.0E-6)
    
    sigma ~ dunif(0,5)
    r~dcat(punif[])
    
    }
    ",fill=TRUE)
sink()




## VALORS INICIALS

dades <- list( n = n, y = c(y1,y2), x = x, punif = rep(1/n,n))

inits <- list(list(alpha = c(-1,NA), beta = c(0.1,5), r=trunc(r/2)),
              list(alpha = c(1,NA), beta = c(5,0.1),  r=trunc(r/2)))

parameters <- c("alpha", "beta", "r")

Iter <- 5000
Burn <- 10
Thin <- 100
Chain <- 2

### en llença "Burn", i despres se'n queda "Iter" que les tria cada "Thins", es a dir en simmula Burn+Iter*Thin per 
### a cada cadena, nombre de cadenes es "Chain"



canvi <- jags(dades, inits, parameters.to.save=parameters,
              model=paste(path.bug,"regressioPuntCanvi.bug",sep=""),
              n.iter=(Iter+Burn),n.burnin=Burn, n.thin=1, n.chains=Chain)


traceplot(canvi , mfrow = c(1,1), varname = c("alpha", "beta", "r"), col=c("black","red"))


print(canvi)


attach.jags(canvi )


table(r)
round(table(r)/sum(table(r)),4)

par(mfrow=c(2,2))

plot(table(r)/sum(table(r)), xlim=c(min(as.numeric(names(table(r))))-1,max(as.numeric(names(table(r))))+1), main="change point", lwd=3)

plot(density(beta[,1], adjust = 1.5), main = expression(paste(pi,"(",beta[1],"|y)")), xlab="")
plot(density(beta[,2], adjust = 1.5), main = expression(paste(pi,"(",beta[2],"|y)")), xlab="")

plot(x, c(y1,y2), main="evolució taxes x 100.000",ylab="taxa",xlab="any")
plot( function(x)(mean(alpha[,1])+x*mean(beta[,1])),xlim=c(1,mean(r)), ad=T)
plot( function(x)(mean(alpha[,2])+x*mean(beta[,2])),xlim=c(mean(r),n),ad=T)
abline(v=mean(r), lty=2)


detach.jags()