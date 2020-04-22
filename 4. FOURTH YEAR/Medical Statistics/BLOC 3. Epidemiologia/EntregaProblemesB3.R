## ESTADÍSTICA MÈDICA
## Laura Julià Melis

# EJERCICIO 5

# Apartado a
library(epitools)
?wcgs
data(wcgs)
View(wcgs)
summary(wcgs)

df <- data.frame(wcgs)
df<- df[-c(1:9,12,14)]
table(df$chd69, df$dibpat0)
table(df$chd69)
table(df$dibpat0)

# Apartado b
casosexposats <- 0
tempsexposats <- 0
casosNoexposats <- 0
tempsNoexposats <- 0

for(i in 1:nrow(df)){
  if(df$chd69[i]  == 1){
    if(df$dibpat0[i]  == 1){
      casosexposats <- casosexposats+1
      tempsexposats<-tempsexposats + df$time169[i]
    }else{
      casosNoexposats <- casosNoexposats+1
      tempsNoexposats<-tempsNoexposats + df$time169[i]
    }
  }
}
taula <- matrix(c(casosexposats, casosNoexposats, tempsexposats, tempsNoexposats),2,2, byrow = F)
taula


(0.0006310378+(2.325*sqrt(0.0006310378/282075)))*10000
(0.0006310378-(2.325*sqrt(0.0006310378/282075)))*10000
(0.0005517415+(2.325*sqrt(0.0005517415/173183)))*10000
(0.0005517415-(2.325*sqrt(0.0005517415/173183)))*10000


# Apartado c

casos <- c(exposed = casosexposats, unexposed = casosNoexposats)
pyears <- c(exposed = tempsexposats, unxposed = tempsNoexposats )
rateratio(casos,pyears, conf.level=0.99)
