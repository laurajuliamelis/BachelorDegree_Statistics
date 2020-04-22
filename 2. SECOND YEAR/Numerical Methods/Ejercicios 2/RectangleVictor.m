%   Integracio per punt mig compost
%
%    Calen les dades a b n i f(x)
%
a=-4
b=0
for k=0:20;
f=@(x)exp(-(x.^2))
h=2^(-k);
x=a+h/2:h:b-h/2; %% punts mitjos
y=f(x);          %% avaluaci� en punts mitjos
n=length(y);
w=ones([1,n]);
Rh(k+1)=w*y'*h
ORh=(h^2)*(b-a)/24
I=quad(f,-4,0);
error(k+1)=abs(Rh(k+1)-I);
end
taula=[0:20;Rh(1:21);error(1:21)]'
