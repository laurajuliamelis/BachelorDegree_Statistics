%% Exercici Jacobi 
A=[10 -1 2 0 ; -1 11 -1 3; 2 -1 10 -1; 0 3 -1 8]
b=[6 25 -11 15]
U=triu(A,1)
L=tril(A,-1)
D=diag(diag(A))
Bj=-(inv(D))*(L+U)
cj=(inv(D))*b'
roBj=max(abs(eig(Bj)))        %% Radi espectral, si (roBj > 1) -> divergent %% 
norm(Bj)                 %% norm(Bj) > roBj %%

x=[0;0;0;0];
iter=10;
error=1
while error>0.0005
    x=Bj*x+cj
    error=norm(b'-A*x);
end
x'
error