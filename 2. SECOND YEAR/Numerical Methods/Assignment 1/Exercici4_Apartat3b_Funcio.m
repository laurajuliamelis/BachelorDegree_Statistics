%% 1.4. SOLUCIONS D'EQUACIONS NO LINEALS.
%  Apartat 3a - Funcio

function [ xN ] = Exercici4_Apartat3b_Funcio ( f, g, x0, tol, N)
%Metode  iteratiu del punt fix
%   g: funcio, x= g(x)
%   x=: punt inicial
%   tol: cota error
%   N= cota iteracions

k=0;
xV=x0;
tolx=1;
tolf=abs(f(x0));
while (k<N && tolx>tol && tolf>tol)
    
    xN=g(xV);
    k=k+1;
    tolf=abs(f(xN));
    tolx=abs(xN-x0);
    xV=xN;
    
end

end