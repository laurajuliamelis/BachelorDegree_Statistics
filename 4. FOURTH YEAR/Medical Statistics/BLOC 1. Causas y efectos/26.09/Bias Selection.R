## Simulación de sesgo de selección, basado en caso de Hernán, "Causal inference", chap. 8.
## 26.09.2018

# Heart disease
P.Z = c(1/3, 2/3)

# tratamiento
P.X = c(0.5, 0.5)

# censura (depende de Z y de X): P.C[·, X, Z]
P.C = array(c(1, 0, 0.5, 0.5, 0.6, 0.4, 0.2, 0.8), dim=c(2,2,2))

# respuesta (depende de Z): P.Y[·, Z]
P.Y = array(c(0.8, 0.2, 0.25, 0.75), dim=c(2,2))

N = 60000

z = sample(0:1, N, pr=P.Z, replace=TRUE)
x = sample(0:1, N, pr=P.X, replace=TRUE) # asignación de wasabi
c = array(NA, dim=N) # censura
y = array(NA, dim=N) # muerte

for (i in 0:1) {
  for (j in 0:1) {
    m = which(z==i & x==j)
    c[m] = sample(0:1, length(m), pr=P.C[,j+1, i+1], replace=TRUE)
  }  }

for (i in 0:1) {
  m = which(z==i)
  y[m] = sample(0:1, length(m), pr=P.Y[,i+1], replace=TRUE)
}
T = table(x,y)

T[1,1]*T[2,2]/T[1,2]/T[2,1]

# correcto


S = table(x,y,c)

# S[,,1] los datos no censurados

S[1,1,1]*S[2,2,1]/S[1,2,1]/S[2,1,1]

# Ajuste IPW por la variable Z
# pesos: cuántas veces los sujetos no censurados se han de multiplicar 
#        para equiparar el tamaño de la población original.

w = table(x,z) / table(c,x,z)[1,,]

u0 = table(y,c,x,z)[1,1,,] * w   # Y=0
u1 = table(y,c,x,z)[2,1,,] * w   # Y=1

Ad = matrix(c(apply(u0, 1, sum), apply(u1, 1, sum)), nrow=2)

Ad[1,1]*Ad[2,2]/Ad[1,2]/Ad[2,1] # Para comprobar que despues de la correción IPW, 
#volvemos a tener la medida correcta de la ausencia de efecto entre el wasabi y la mortalidad.

