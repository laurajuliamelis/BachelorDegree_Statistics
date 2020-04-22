%% Exercici Jacobi 
A=[3 1 -1;1 5 -1 ;-1 1 -4];
b=[2;1;-3];
U=triu(A,1);
L=tril(A,-1);
D=diag(diag(A));
Bj=-(inv(D))*(L+U);
cj=(inv(D))*b;
roBj=max(abs(eig(Bj)))      %% Radi espectral, si (roBj > 1) -> divergent %% %% eig torna els valos propis de la matriu 
r=norm(Bj)                %% norm(Bj) > roBj %%

x=[0;0;0];
iter=10;
error=1
while error>0.0005
    x=Bj*x+cj
    error=norm(b'-A*x);
  if(roBj <1)
  disp('convergente')
  else 
  disp('divergente')
  end
end
x'
error