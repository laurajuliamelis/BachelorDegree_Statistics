%% 1.4. SOLUCIONS D'EQUACIONS NO LINEALS.
%  Apartat 2 - Newton

clear all
format long

f=@(x)x.^3 + 4*x.^2 - 10;
g=@(x) 3.*x.^2+ 8.*x;
xv(1)=1;
l(1)=xv(1);
eps=0.5*(10.^-6);
tolf= abs(f(xv(1)));
tolx = 1;
k=1

while (tolx>eps && tolf>eps)
   
   xN=xv(k)-(f(xv(k))/g(xv(k)))
   tolf= abs(f(xN));
   tolx= abs(xN-xv(k));
   xv(k+1)=xN;
   l(k+1)=xv(k+1)-xv(k);
   k= k + 1
end

n=1:1:length(xv);
taula_resultats=[n;xv;f(xv);l]'