%JACOBIANO 2
%%EJERCICIO JACOBIANO
A=[10 -1 2 0 ; -1 11 -1 3; 2 -1 10 -1; 0 3 -1 8];
b = [6 25 -11 15]
U=triu(A,1)
L=tril(A,-1)
D=diag(diag(A))
Bj=-(inv(D))*(L+U)
cj=(inv(D))*b'  %% para que te lo ponga en columna, vector columna porque b'
roBj=max(abs(eig(Bj)))   %% calcula los valores propios de la matriz eig
norm(Bj)  

x=[0;0;0;0];
iter=10;
error=1;
while error>0.0005
    x=Bj*x+cj;
    error= norm(b'-A*x);
end
x'
error