%% 1.2. ERRORS DE CANCEL?LACIO.
%  Apartat 3 - Principal

clear all
format long g

% Algoritme de Horner
f=@(x)x.^7-7.*x.^6+21.*x.^5-35.*x.^4+35.*x.^3-21.*x.^2+7.*x-1;
x=0.988:0.00005:1.012;
a=[1 -7 21 -35 35 -21 7 -1];
n= 7;

d= Exercici2_Apartat3_Funcio(f,n,a,x)

n=1:1:length(d);
taula_resultats=[n;x;d]'

% Grafica
y=0.9:0.05:1.2;
plot(y,f(y),x,d,'red'), grid, title('Regla de Horner')