####################################################################
################    SESSIÓ  2   ####################################
####################################################################




####################  2.1 SEPIA  VERDA   ##############################

#####	prior distribution and prior predictive distribution


prior <- c(alpha = 10, beta = 0.5)


par(mfrow=c(1,2))

plot(function(x)dgamma(x, prior[1], prior[2]), xlim=c(0,60), ylab="", xlab = expression(lambda))
title("prior distribution")



# simulated prior predictive distribution

M <- 100000

prior.sim <- rgamma(M, prior[1], prior[2])
pre.prior.sim <- rpois(M, prior.sim)

plot(table(pre.prior.sim)/M, ty="h")
 abline(v=c(5,40), lyt=2, col="blue")
 title("prior predictive distribution")




###### data

y <- read.table("G:\\200547 - METODES BAYESIANS\\dades\\sepiaverda.txt", header = F)

n <- dim(y)[1]

y


####### standardaized likelihood function  

sd.like <- function(th) {

(th^sum(y)*exp(-n*th))/integrate(function(th)(th^sum(y)*exp(-n*th)),lower=0, upper=50)$value

}



par(mfrow=c(1, 1))
plot(function(x)sd.like(x), xlim=c(0,60), ylab="", xlab = expression(lambda))
 plot(function(x)dgamma(x, prior[1], prior[2]), xlim=c(0,60), add=T, lty=2)
 legend("topright", c("prior", "likelihood"), lty = c(2,1))



#########  posterior distribution


posterior <- c(a = prior[1] + sum(y) , b = prior[2] + n )



# prior, likelihood and posterior

plot(function(x)dgamma(x, posterior[1], posterior[2]), xlim=c(0,60), lty=1)
 plot(function(x)sd.like(x), xlim=c(0,60), ylab="", xlab = expression(lambda), add=T, lty=3)
 plot(function(x)dgamma(x, prior[1], prior[2]), xlim=c(0,60), add=T, lty=2)
 legend("topright",  c("prior", "likelihood", "posterior"), lty = c(2,3,1))



#  summary results

results <- matrix(nrow = 7, ncol = 2)

colnames(results ) <- c('prior', 'posterior')
rownames(results ) <- c('alpha', 'beta', 'mean', 'variance', '2,5%', 'median', '97.5%')

results [1:2, 1] <- prior
results [3, 1] <- prior[1]/prior[2]
results [4, 1] <- prior[1]/prior[2]^2
results [5, 1] <- qgamma(0.025, shape = prior[1], prior[2])
results [6, 1] <- qgamma(0.5, shape = prior[1], prior[2])
results [7, 1] <- qgamma(0.975, shape = prior[1], prior[2])

results [1:2, 2] <- posterior
results [3, 2] <- posterior[1]/posterior[2]
results [4, 2] <- posterior[1]/posterior[2]^2
results [5, 2] <- qgamma(0.025, shape = posterior[1], posterior[2])
results [6, 2] <- qgamma(0.5, shape = posterior[1], posterior[2])
results [7, 2] <- qgamma(0.975, shape = posterior[1], posterior[2])

round(results , 3)



#  priror and posterior predictive distribution

par(mfrow=c(2,1)) 


M <- 100000

prior.sim <- rgamma(M, prior[1], prior[2])
pre.prior.sim <- rpois(M, prior.sim)

post.sim <- rgamma(M, shape = posterior[1], posterior[2])
post.pre.sim <- rpois(M, post.sim)

plot(table(pre.prior.sim)/M, ty="h", xlim=c(0, 60), ylab = "", xlab = "",  col="blue", lwd=0.5)
 title("prior predictive distribution")

plot(table(post.pre.sim)/M, ty="h", xlim=c(0, 60), ylab = "", xlab = "",  col="blue", lwd=0.5)
title("posterior predictive distribution")




round(sum(post.pre.sim<10)/M,3)

h1 <- round(sum(post.pre.sim<15)/M,3)
h3 <- round(sum(post.pre.sim>24)/M,3)

h2 <- 1- h1- h3

h1;h2;h3





#################### 2.2  ASMA   #################################################


# prior distribution

prior <- c(alpha = 2, beta = 40)
#prior <- c(alpha = 1 , beta = 1)


par(mfrow=c(1,1))

plot(function(x)dbeta(x, prior[1],prior[2]), xlim=c(0,1), ylab="", xlab = "theta")
 title(paste("Prior: Beta","(","a=",prior[1],",","b=",prior[2],")"))


# data

N <- 200; y <- 11

#N <- 20; y <- 1
#N <- 2000; y <- 110

# likelihood

plot(function(x)dbinom(y, N, x),ylab="",xlab=expression(theta), xlim=c(0,1))
 abline(v=y/N, lty=2, col="blue")

K <- integrate(function(x)dbinom(y, N, x),lower=0, upper=1)$value

plot(function(x)dbeta(x, prior[1], prior[2]), xlim=c(0,1), ylab="", xlab =expression(theta), ylim=c(0,25))
 plot(function(x){dbinom(y, N, x)/K}, add=T, lty=2)
 legend("topright", c("prior","likelihood"),lty=c(1,2))
 title("prior & likelihood")



# posterior distribution


posterior <- c(a = prior[1]+y, b = prior[2]+(N-y) )



# DIBUIX DE LA DISTRIBU DISTRIBUCIO A PRIORI, A POSTERIORI I LA VERSEMBLANÇA

plot(function(x)dbeta(x, posterior[1], posterior[2]), xlim=c(0,1), ylab="", xlab =expression(theta))
 plot(function(x){dbinom(y, N, x)/K}, add=T, lty=3)
 plot(function(x)dbeta(x, prior[1], prior[2]), add=T, lty=2)

 legend("topright", c("prior","posterior","likelihood"), lty = c(2,1,3))
 title("prior , posterior & likelihood")



# summnary

sortida <- matrix(nrow = 7, ncol = 2)

colnames(sortida) <- c('prior', 'posterior')
rownames(sortida) <- c('alpha', 'beta', 'mean', 'variance', '2,5%', 'median', '97.5%')

sortida[1:2, 1] <- prior
sortida[3, 1] <- prior[1]/(prior[1] + prior[2])
sortida[4, 1] <- (prior[1]*prior[2])/(((prior[1]+prior[2])^2)*(prior[1]+prior[2]+1))
sortida[5, 1] <- qbeta(0.025, prior[1], prior[2])
sortida[6, 1] <- qbeta(0.5, prior[1], prior[2])
sortida[7, 1] <- qbeta(0.975, prior[1], prior[2])

sortida[1:2, 2] <- posterior
sortida[3, 2] <- posterior[1]/(posterior[1] + posterior[2])
sortida[4, 2] <- (posterior[1]*posterior[2])/(((posterior[1]+posterior[2])^2)*(posterior[1]+posterior[2]+1))
sortida[5, 2] <- qbeta(0.025, posterior[1], posterior[2])
sortida[6, 2] <- qbeta(0.5, posterior[1], posterior[2])
sortida[7, 2] <- qbeta(0.975, posterior[1], posterior[2])


round(sortida, 3)


# prior and posterior predictive distribution

n.sim <- 100000

z <- rbeta(n.sim, prior[1], prior[2])
pre.prior <- rbinom(n.sim, 50, z)

z2 <- rbeta(n.sim, posterior[1], posterior[2])
pre.post <- rbinom(n.sim, 50, z2)

par(mfrow=c(2, 1))
plot(table(pre.prior)/n.sim, xlim=c(0, 50), ty="h", ylab="")
  title("prior predictive distribuiton")
plot(table(pre.post)/n.sim, xlim=c(0, 50), ty="h", ylab="")
  title("posterior predictive distribuiton")

