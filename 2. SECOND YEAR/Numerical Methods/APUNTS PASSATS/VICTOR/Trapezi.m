%%Exercici 3 
f=@(x)2./(2+sin(10*pi.*x));
ve=1.1547005383792515290183;
a=0; b=1;
n=16;
h=(b-a)/n;
x=a:h:b;
y=f(x);
T=trapz(x,y)
error=abs(T-ve)
