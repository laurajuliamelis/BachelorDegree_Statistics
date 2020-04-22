A=[10 -1 2 0 ; -1 11 -1 3; 2 -1 10 -1; 0 3 -1 8];
b = [6 25 -11 15]
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
else disp('convergent')
end
for n=0:9
   Y=Bj*X+cj
   Cota_error=(norm(Bj))/(1-(norm(Bj)))*norm(Y-X)
   X=Y;
   n=n+1
   error=abs(A\b-X)
end