%% 1.2. ERRORS DE CANCEL?LACIO.
%  Apartat 4

clear all
format long g

% Grafica apartat 2 apropada
f=@(x)x.^7-7.*x.^6+21.*x.^5-35.*x.^4+35.*x.^3-21.*x.^2+7.*x-1
x1=0.97:0.00005:1.03;
y1=0.988:0.00005:1.012;
subplot(1,2,1), plot(x1, f(x1),y1, f(y1), 'red'), grid, title('Apartat 2')

% Grafica apartat 3 apropada (Horner)
x2=0.988:0.00005:1.012;
a=[1 -7 21 -35 35 -21 7 -1];
n= 7;
d= Exercici2_Apartat3_Funcio(f,n,a,x2)
y2=0.97:0.00005:1.03;
subplot(1,2,2), plot(y2,f(y2),x2,d,'red'), grid, title('Apartat 3 (Horner)')

