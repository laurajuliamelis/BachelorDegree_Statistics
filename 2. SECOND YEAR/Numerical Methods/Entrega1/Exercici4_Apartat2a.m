%% 1.4. SOLUCIONS D'EQUACIONS NO LINEALS.
%  Apartat 2 - Biseccio

clear all
format long
f = @(x)x.^3 + 4.*x.^2 - 10;
a(1)=1;
b(1)=2;
l(1)=b(1)-a(1);
m(1)=(a(1)+b(1))./2;
eps=0.5*(10.^-6);
k=1;
while abs(b-a)>eps
   
    if f(a(k))*f(m(k))<0
        a(k+1)=a(k);
        b(k+1)=m(k);
        k=k+1;
    else
        a(k+1)=m(k);
        b(k+1)=b(k);
        k=k+1;
    end
    
    m(k+1)=(a(k)+b(k))./2;
    r(k-1)=(m(k)-m(k-1))./m(k);
    l(k+1)=b(k)-a(k);
end

a=[0,a];
b=[0,b];
n=1:1:length(a);
taula_resultats=[n; a; b; m; f(m); l]'