A=[3 1 -1;1 5 -1;-1 1 -4]
b=[ 2;1;-3]
U=triu(A,1)
L=tril(A,-1)
D=diag(diag(A))
Bj=-(inv(D))*(L+U)
cj=(inv(D))*b
roBj=max(abs(eig(Bj)))        %% Radi espectral, si (roBj > 1) -> divergent %% 
norm(Bj)                 %% norm(Bj) > roBj %%
X=[0; 0; 0]
if roBj>=1
    disp('divergent')
break
end
for n=0:9
   Y=Bj*X+cj
   Cota_error=(norm(Bj))/(1-(norm(Bj)))*norm(Y-X)
   X=Y
   n=n+1
end
exacte=A\b %% Cuidao con la barra
err=abs(X(1)-exacte(1)) 
%% metode de jacobi (x^k+1,y^k+1,z^k+1)'= Bj * (x^k,y^k,z^k)+ cj
