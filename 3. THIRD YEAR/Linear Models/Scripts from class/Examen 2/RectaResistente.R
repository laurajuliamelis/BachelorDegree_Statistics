
greenberg <- read.table("greenberg.txt", header=T)

# Recta resistente de los tres grupos de Tukey 

groups <- rep(1:3, each=6)
median_x <- tapply(greenberg$edad, groups, median)                                 
median_y <- tapply(greenberg$altura, groups, median)
b <- (median_y[3] - median_y[1])/(median_x[3] - median_x[1])
a <- median(greenberg$altura - b*greenberg$edad)

line(greenberg$edad,greenberg$altura)

plot(greenberg$edad,greenberg$altura)
abline(lm(altura~edad, data=greenberg))
abline(a,b,lty=2)

# Recta resistente de los tres grupos
median_median_line <- function(x, y, data)
{
  if(!missing(data))
  {
    x <- eval(substitute(x), data) 
    y <- eval(substitute(y), data) 
  }

  stopifnot(length(x) == length(y))

  #Step 1 #dividir les dades en tres grups en funció de la longitud de x entre 3 (depen de si la n és multiple de 3)
  one_third_length <- floor(length(x) / 3)
  groups <- rep(1:3, times = switch((length(x) %% 3) + 1,
     c(one_third_length, one_third_length, one_third_length),
     c(one_third_length, one_third_length + 1, one_third_length),
     c(one_third_length + 1, one_third_length, one_third_length + 1)
  ))

  #Step 2 #ordenacio de les x i les y en funció de les x
  o <- order(x)
  x <- x[o]
  y <- y[o]

  #Step 3 #càlcul de la mediana dels tres grups
  median_x <- tapply(x, groups, median)  #sortiran 3                               
  median_y <- tapply(y, groups, median)  #sortiran 3  

  #Step 4
  slope <- (median_y[3] - median_y[1]) / (median_x[3] - median_x[1]) #pendent (b0)
  intercept <- (median_y[1] - slope * (median_x[1]-median_x[2])
                + median_y[2]
                + median_y[3] - slope * (median_x[3]-median_x[2])
                )/3

  #Step 5 #sortida de la funció
  c(intercept = unname(intercept), slope = unname(slope), median_x_C = unname(median_x[2]))
}

recta0 <- median_median_line(edad,altura,greenberg) #recta inicial 
recta0

p0 <- recta0["intercept"] + recta0["slope"] * (greenberg$edad - recta0["median_x_C"])
plot(greenberg$edad,greenberg$altura)
lines(greenberg$edad,p0)

# Procedimiento iterativo
rline <-function(x, y, k=10, ajust=0.01){
  # k es el nÃºmero de iteraciones
  # ajust es el ajuste en tanto por ciento de la pendiente inicial
recta0 <- median_median_line(x,y)
a <- recta0["intercept"]
b <- b0 <- recta0["slope"]
x_cent <- x - recta0["median_x_C"]
gamma <- a
delta <- b0
ee <- y
for(i in 1:k){
  while(abs(delta) > ajust * abs(b0)){
ee <- ee - (gamma + delta*x_cent)   
recta <- median_median_line(x,ee)  
gamma <- recta["intercept"]
delta <- recta["slope"]
a <- a + gamma
b <- b + delta
  }
}
c(intercept=unname(a), slope=unname(b), recta0["median_x_C"])
}

rline(greenberg$edad,greenberg$altura) #verdadera recta dels 3 grups (pel metode iteratiu dels 3 grups)
resistant_line(greenberg$edad,greenberg$altura, iter=10) intercept=a+bx-xt
#!!!ajust diferencia entre el pendent en una iteració i la seguent


# Recta con medianas repetidas de Siegel(1982)
library(mblm) # Median-Based Linear Models
mblm(altura ~ edad, data=greenberg)                   # Siegel intercept=a+bx 
mblm(altura ~ edad, data=greenberg, repeated = FALSE) # Theil-Sen

# Una funciÃ³n que calcula la recta de Siegel con diferente intercept
repmedians <- function(x,y) {
stopifnot(length(x) == length(y))
med.pendientes <- numeric(length(x))
# mediana de las pendientes para cada punto fijo i
for(i in 1:length(x)){
med.pendientes[i]<-median((y[i] - y)/(x[i] - x), na.rm=T)
}
bRM =median(med.pendientes)
aRM <- median(y - bRM*x)
c(ord.origen=aRM, pendiente=bRM)
}

repmedians(greenberg$edad,greenberg$altura)

#punt de colapse: proporcio de punts que podem fer salvatges sense que la entercepcio i el pendent es molestin
#per colapsar la mediana cal que la meitat dels punts siguin salvatges
# el punt de colapse de la mitjana és 0 (qualsevol punt que mogui al infinit fa també que la mitjana vagi cap al infinit)