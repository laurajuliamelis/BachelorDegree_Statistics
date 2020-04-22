x=[0:0.01:1]
f=@(x)x+log(x);
plot(x,f(x)),grid
aprox=fzero(f,1)
hold on
plot(x,zeros(size(x)),'g')
plot(aprox,0,'^r') % ^ es el simbol d'on es creua amb l'eix 0 tambe pot ser *
% plot(zero2,0,'*r')
hold off
f1=@(x)-log(x);
y=0.5
tol=@(x)abs(aprox-x)
toler=tol(y)
n=1
while (toler>0.0005 && n<100)
    y=f1(y)
    toler=tol(y)
    n=n+1
end
