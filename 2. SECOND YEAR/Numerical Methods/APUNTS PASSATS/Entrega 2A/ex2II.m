%FENT ÚS DE F2

f=@(x) atan(x);
x0=sqrt(2)
hk=@(k)10.^(-k);
k=1:1:15;
aprox2=(f(x0+hk(k))-f(x0-hk(k)))./(2.*hk(k));

df=@(x)1/(1+x.^2);
ve=df(x0);
ea2=abs(aprox2-ve);
T2=[k; aprox2; ea2]'