%   Integracio per trapezis
%
%   Calen les dades a, b, n i f(x)
%
a=0;
b=4;
for k=0:20
f=@(x)exp(-(x.^2));
h=2^(-k);
x=a:h:b;      %% punts mitjos
y=f(x);       %% avaluació en punts mitjos
n=length(y);
w=2.*ones([1,n]); w([1,n])=1;
Th(k+1)=w*y'*h/2;
OTh=(h^2)*(b-a)/12;
I=quad(f,0,4);
error(k+1)=abs(Th(k+1)-I);
end
taula=[0:20;Th(1:21);error(1:21)]'