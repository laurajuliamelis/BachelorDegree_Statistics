t=[-3:0.01:3];
f=@(x)x.^4-8*x.^2+6*x-10
fp=@(x)4*x.^3-16*x+6
fpp=@(x)12*x.^2-16
plot(t,f(t),'k',t,fp(t),'r',t,fpp(t))



zero1=fzero(f,2) % punts al grafic
% zero2=fzero(f,2)
hold on
plot(t,zeros(size(t)),'g')
plot(zero1,0,'^r') % ^ es el simbol d'on es creua amb l'eix 0 tambe pot ser *
% plot(zero2,0,'*r')
hold off


n=n+1, x=y; y=x-f(x)/fp(x)
