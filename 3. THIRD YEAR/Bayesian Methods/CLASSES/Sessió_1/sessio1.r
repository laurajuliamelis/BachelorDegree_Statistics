####################################################################
################    SESSIÓ  1   ####################################
####################################################################


######	DIBUIX DE LA DISTRIBUCIO BETA  ########



# la distribucio beta en funcio dels parametres

par(mfrow=c(3,3))

for (a in c(0.5,1, 10)) {
	for (b in c(0.5, 1,10)) {

		plot(function(x)dbeta(x, a, b), xlim=c(0,1), ylab="", xlab = "")
  		  title(paste("Distribució Beta","(","a=",a,",","b=",b,")"))
	}
}




######################## % fumadors	Granollers   ###########################

par(mfrow=c(1,1))

a <- 7
b <- 13
 
plot(function(x)dbeta(x, a, b), xlim=c(0,1), ylab="", xlab = "")
 title(paste("Distribució Beta","(","a=",a,",","b=",b,")"))




######################## moneda, xinxeda, ? ###########################

par(mfrow=c(1,2))

# moneda

a <- 50
b <- 50

plot(function(x)dbeta(x, a, b), xlim=c(0,1), ylab="", xlab = "")
 title(paste("Distribució Beta","(","a=",a,",","b=",b,")"))

# xinxeta

a <- 4
b <- 2

plot(function(x)dbeta(x, a, b), xlim=c(0,1), ylab="", xlab = "")
 title(paste("Distribució Beta","(","a=",a,",","b=",b,")"))

# ?

a <- 1
b <- 1
 
plot(function(x)dbeta(x, a, b), xlim=c(0,1), ylab="", xlab = "")
 title(paste("Distribució Beta","(","a=",a,",","b=",b,")"))







#######################  1.3 ALÇADA   ####################

par(mfrow=c(1,1))

priori <- c(170,5 )
priori1 <- c(170, 15)

#####  DIBUIX DE LA DISTRIBU DISTRIBUCIO A PRIORI

plot(function(x)dnorm(x,priori1[1], priori1[2]),xlim=c(130,220), ylab="", xlab = expression(mu))
 title("Distribució Normal")

 plot(function(x)dnorm(x,priori1[1], priori1[2]),xlim=c(130,220), ylab="", xlab = expression(paste(mu," ,   y")))
 plot(function(x)dnorm(x,priori[1], sqrt(priori[2]^2+8^2)),xlim=c(130,220),lty=3,add=T)
 legend("topright", c("priori","pre.priori"), lty=c(1,3))
 title("Distribució Normal")


 # simulacio de la predictiva a priori
 
 M <- 100000
 
 sim <- rnorm(M, priori[1], priori[2])
 dist.pre.priori.sim <- rnorm(M, sim, 8)
 
 hist(dist.pre.priori.sim, freq = FALSE, breaks = 50)
 lines(density(dist.pre.priori.sim))
 
 plot(function(x)dnorm(x,priori[1], priori[2]),xlim=c(130,220), ylab="", xlab = expression(paste(mu," ,   y")))
 plot(function(x)dnorm(x,priori[1], sqrt(priori[2]^2+8^2)),xlim=c(130,220),lty=3,add=T)
 lines(density(dist.pre.priori.sim), lty=2, col="blue")
 legend("topright", c("priori", "pre.priori.sim","pre,priori.exact"), lty=c(1,2,3))
 title("Distribució Normal")
 














###################### 1.1  ASMA  ######################


#####  DIBUIX DE LA DISTRIBU DISTRIBUCIO A PRIORI


par(mfrow=c(1,1))

a <-2
b <-40
priori <- c(a,b)


plot(function(x)dbeta(x, a, b), xlim=c(0,1), ylab="", xlab = "")
 title(paste("Distribució Beta","(","a=",a,",","b=",b,")"))



#####	DIBUIX DE LA DISTRIBU DISTRIBUCIO PREDICTIVA A PRIORI calculada via simulació

 
 M <- 100000
 n <- 200
 
 p <- rbeta(M, priori[1], priori[2])
 pre.priori <- rbinom(M, n, p)
 
 barplot(table(pre.priori)/M) #serveix de poc!!!!!
 




# podriem implementar la Beta-binomial per dibuixar-la exacata, la fòrmula està al FORMULARI



#  DADES MOSTRA

N <- 200
y <- 11


####   DIBUIX DE LA VERSEMBLANÇA
plot(function(th)dbinom(y, N, th),ylab="",xlab=expression(theta), xlim=c(0,1))
abline(v=y/N,lty=2, col="blue")

plot(function(th)dbeta(th, a, b), xlim=c(0,1), ylab="", xlab =expression(theta))#versemblança amb maxim
plot(function(th)dbinom(y, N, th), add=T, lty=2)
legend("topright", c("priori","versemblança"),lty=c(1,2))
title("priori i versemblança") # aquest grafic no es igual de significatiu i no ha sortit com esperavem


  



# versemblança reescalada

K <- integrate(function(x)dbinom(y,N,x), lower=0, upper=1, subdivisions=1000)$value

plot(function(x)dbeta(x, a, b), xlim=c(0,1), ylab="", xlab =expression(theta),ylim=c(0,25))
plot(function(x){dbinom(y, N, x)/K}, add=T, lty=2)
legend("topright", c("priori","versemblança"),lty=c(1,2))
title("priori i versemblança") # aixi si que tenim una bona versemblança










