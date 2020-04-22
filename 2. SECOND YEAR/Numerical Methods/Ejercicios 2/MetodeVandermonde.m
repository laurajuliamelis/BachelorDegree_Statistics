%% Polinomi interpolador (mètode de Vandermonde)
clear all
x=[0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0];
y=[1.1 1.2 1.3 1.4 1.5 1.4 1.3 1.4 1.5 1.6 1.7];

V=vander(x)
cp=V\y'  %coeficients del polinomi interpolador

%% gràfica
t=0:0.1:1;
pt=polyval(cp,t);
plot(t,pt,'r',x,y,'b*')
