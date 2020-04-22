%% Exercici Jacobi 
A=[5 -2 0 0 -2; -2 5 -2 0 0; 0 -2 5 -2 0; 0 0 -2 5 -2;-2 0 0 -2 5]
b=[ 1 ;1; 1; 1; 1]
U=triu(A,1)
L=tril(A,-1)
D=diag(diag(A))
Bj=-(inv(D))*(L+U)
cj=(inv(D))*b'
roBj=max(abs(eig(Bj)))      %% Radi espectral, si (roBj > 1) -> divergent %% 
norm(Bj)                %% norm(Bj) > roBj %%
X=cell(1,3);
if roBj>=1
    disp('divergent')
break
end
for n=0:9
   Y=Bj*X+cj
   Cota_error=(norm(Bj))/(1-(norm(Bj)))*norm(Y-X)
   Y=X
   n=n+1
end