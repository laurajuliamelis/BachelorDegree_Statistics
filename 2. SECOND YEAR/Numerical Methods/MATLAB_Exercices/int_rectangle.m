%   Integracio per punt mig compost
%
%    Calen les dades a b n i f(x)
%
a=input('  extrem inferior a:');
b=input('  extrem superior b:');
n=input('  subintervals n:');
h=(b-a)/n;
x=a+h/2:h:b-h/2; %% punts mitjos
y=f(x);          %% avaluaci� en punts mitjos
n=length(y);
w=ones([1,n]);
Rh=w*y'*h
ORh=(h^2)*(b-a)/24