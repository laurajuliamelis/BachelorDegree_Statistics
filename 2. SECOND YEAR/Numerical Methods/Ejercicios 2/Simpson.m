f=@(x)((x.^20).*(sin(pi*x)))
%   Integracio per Simpson
%    Calen les dades a b n i f(x)

a=input('  extrem inferior a:');
b=input('  extrem superior b:');
n=input('  subintervals n:');
h=(b-a)/n;
x=a:h/2:b;      %% punts mitjos
y=f(x);       %% avaluació en punts mitjos
n=length(y);
w=ones([1,length(y)]); w(2:2:length(y))=4;
w(3:2:length(y)-1)=2
Sh=w*y'*h/6
OSh=1/90*(h^5)*(b-a)