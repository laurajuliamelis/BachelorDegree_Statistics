t=[0:0.01:2];
f=@(x)exp(x)-2+x;
fp=@(x)exp(x)+1;
plot(t,f(t)),grid
zero1=fzero(f,2) % punts al grafic
% zero2=fzero(f,2)
hold on
plot(t,zeros(size(t)),'g')
plot(zero1,0,'^r') % ^ es el simbol d'on es creua amb l'eix 0 tambe pot ser *
% plot(zero2,0,'*r')
hold off

x1=1,x2=2,n=0
n=n+1, y=x2-f(x2)*(x2-x1)/(f(x2)-f(x1))
x1=x2,x2=y
n=n+1, y=x2-f(x2)*(x2-x1)/(f(x2)-f(x1))
x1=x2,x2=y
n=n+1, y=x2-f(x2)*(x2-x1)/(f(x2)-f(x1))
abs(y-zero1)