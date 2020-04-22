%%Introducció Àlgebra Lineal
A=[1 2 3; 0 1, -1; 7 8 9]
A(:,1) %columna 1 de la matriu
A(3,:) %fila 3 de la matriu 
ones(4) % matriu quadrada tota de "uns"
zeros(3) %matriu  quadrada tota de zeros
zeros([3,5]) % matriu de 3 files i 5 columnes 
eye(6) %matriu identitat 
diag(A) % es un vector que conté la diagonal de la matriu A 
B=rand(4) % nombres pseudorandom
triu(B) % matriu triangular superior
tril(B) % matriu triangular inferior 
tril(B,-2) % M'agafa dos per sota de la diagonal principal 
c(2,:)=[] % esborra la fila 2