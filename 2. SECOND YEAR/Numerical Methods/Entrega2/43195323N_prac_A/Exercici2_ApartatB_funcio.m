%% 1.2. Algebra lineal numerica: valors propis.
%  Apartat B - funcio
 
function [rho,x,iter]= Exercici2_ApartatB_funcio(A,tol,q) 

[n,m] = size(A);
q = q/norm(q);
w = A*q;
rho = q'*w;
eps = tol*abs(rho) + 1;
iter = 0;

while eps>tol*abs(rho) & abs(rho)~=0
    x = w; 
    x = x/norm(x);
    pro = A*x; 
    rho_nova = x'*pro; 
    eps = abs(rho_nova - rho);
    rho = rho_nova; 
    iter = iter + 1;
end
return