%% Introduccion Algebra Lineal
A=[1 2 3; 0 1, -1; 7 8 9]
A(:,1) % columna 1 de la matriz A
A(3,:) % fila 3 de la matriz
ones(4) % matriz quadrada toda de 1's 4x4
zeros(3) % matriz quadrada toda de 0's 3x3
zeros([3,5]) % matriu toda de 0's 3x5
eye(6) % matriz identidad 6x6
diag(A) % vector que contiene la diagonal de la matriz A
B=rand(4) % numeros pseudorandom 
triu(B) % parte triangular superior de la matriz B
tril(B) % parte triangular inferior de la matriz B
tril(B, -2)
tril(B, -1)

%% Ejercicio
A= randn(42);
diag(A);
AT=A' % matriz traspuesta
inv(A) % matriz inversa
A(:,2)=[] % columna 2 borrada
A(:,3)=[] % columna 4 borrada
A([3,5],:)=[] % filas de la 3 a la 5 borradas
 mean(A) %medias para cada fila
 mean( mean(A)) % media de la matriz