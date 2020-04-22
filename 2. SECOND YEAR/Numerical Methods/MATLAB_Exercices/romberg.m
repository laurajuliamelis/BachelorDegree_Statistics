%   Integracio per romberg
%
%    Calen les dades a b n i f(x)
%
sigma=input('valor de sigma:');
a=-3*sigma;
b=3*sigma;
n=16;
h=(b-a)/n;
x=a:h:b;      %% punts mitjos
t=x/sigma
f=@(x)(1/((x/t(k))*sqrt(2*pi)))*exp(-(t(k)).^2/2);
y=f(x);       %% avaluaci� en punts mitjos
n=length(y);
w=2.*ones([1,n]); w([1,n])=1
Th16=w*y'*h/2
%OTh=(h^2)*(b-a)/12

n=8;
h=(b-a)/n;
x=a:h:b;      %% punts mitjos
t=x/sigma
f=@(x)(1/((x/t(k))*sqrt(2*pi)))*exp(-(t(k)).^2/2);
y=f(x);       %% avaluaci� en punts mitjos
n=length(y);
w=2.*ones([1,n]); w([1,n])=1
Th8=w*y'*h/2
%OTh=(h^2)*(b-a)/12

n=4;
h=(b-a)/n;
x=a:h:b;      %% punts mitjos
t=x/sigma
f=@(x)(1/((x/t(k))*sqrt(2*pi)))*exp(-(t(k)).^2/2);
y=f(x);       %% avaluaci� en punts mitjos
n=length(y);
w=2.*ones([1,n]); w([1,n])=1
Th4=w*y'*h/2
%OTh=(h^2)*(b-a)/12

n=2;
h=(b-a)/n;
x=a:h:b;      %% punts mitjos
t=x/sigma
f=@(x)(1/((x/t(k))*sqrt(2*pi)))*exp(-(t(k)).^2/2);
y=f(x);       %% avaluaci� en punts mitjos
n=length(y);
w=2.*ones([1,n]); w([1,n])=1
Th2=w*y'*h/2
%OTh=(h^2)*(b-a)/12


TT1(2,2)=1/3*(4*Th4-Th2)
TT1(3,2)=1/3*(4*Th8-Th4)
TT1(4,2)=1/3*(4*Th16-Th8)

TT1(3,3)=1/15*(16*TT1(3,2)-TT1(2,2))
TT1(4,3)=1/15*(16*TT1(4,2)-TT1(3,2))

TT1(4,4)=1/63*(64*TT1(4,3)-TT1(4,3))









