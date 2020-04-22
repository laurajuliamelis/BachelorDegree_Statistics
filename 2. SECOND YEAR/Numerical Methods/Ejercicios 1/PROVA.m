%   Integracio per trapezis
%
%   Calen les dades a, b, n i f(x)
%
a=input('  extrem inferior a:');
b=input('  extrem superior b:');
%% n=input('  subintervals n:');
f=@(x)exp(-x.^2)
I=quad(f,0,4)
for k=0:20;
    h=2.^-k;
%% h=(b-a)/n;
x=a:h:b;      %% punts mitjos
y=f(x);       %% avaluació en punts mitjos
n=length(y);
w=2.*ones([1,n]); w([1,n])=1;
Th(k+1)=w*y'*h/2
OTh=(h^2)*(b-a)/12;
err=abs(I-Th)
end;
taula=[0:20;Th(1:21);err]