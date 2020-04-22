%% 1.4. SOLUCIONS D'EQUACIONS NO LINEALS.
%  Apartat 2 - Secant

clear all
format long g
f=@(x)x.^3 + 4*x.^2 - 10;
a(1)=1
b(1)=2
tolx(1)=abs(b(1)-a(1));
tolf(1)=max(abs(f(a(1))),abs(f(b(1))));
eps=0.5*(10.^-6);
k=1
while (tolx(k)>eps && tolf(k)>eps)

   c = a(k);
   a(k+1) = b(k);
   b(k+1) = b(k) + (b(k) - c)/(f(c)/f(b(k))-1);
   tolx(k+1)= abs(b(k)-a(k));
   tolf(k+1)= abs(f(b(k)));
   k=k+1;
   
end


n=1:1:length(a);
taula_resultats=[n;a;b;f(b)]'
