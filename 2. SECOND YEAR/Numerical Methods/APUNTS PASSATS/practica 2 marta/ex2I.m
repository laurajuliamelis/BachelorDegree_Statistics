%FENT ÚS DE F1

f=@(x) atan(x);
x0=sqrt(2)
hk=@(k)10.^(-k);
k=1:1:15;
aprox1=(f(x0+hk(k))-f(x0))./hk(k);

df=@(x)1/(1+x.^2);
ve=df(x0);
ea1=abs(aprox1-ve);
T1=[k; aprox1; ea1]'