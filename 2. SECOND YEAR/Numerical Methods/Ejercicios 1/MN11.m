t=[0:0.01:2];
f=@(x)x.^6-x-1
plot(t,f(t)),grid
zero1=fzero(f,2) % punts al grafic
zero2=fzero(f,2)
hold on
plot(t,zeros(size(t)),'g')
plot(zero1,0,'^r') % ^ es el simbol d'on es creua amb l'eix 0 tambe pot ser *
% plot(zero2,0,'*r')
hold off