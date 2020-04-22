%% Polinomi interpolador (metode de Vandermonde)
clear all
x=[1.0 1.125 1.250 1.375 1.500 1.625 1.750 1.875 2.0];
y=[0 0.169925 0.321928 0.459432 0.584962 0.700440 0.807355 0.906891 1.0];

V=vander(x)
cp=V\y'  %coeficients del polinomi interpolador

valor = polyval(cp,3) %valor del polinomi en 3

%% grafica
t=0.5:0.1:2;
pt=polyval(cp,t);
hold on
plot(t,pt,'g',x,y,'*')

USAR VANDERMONDE PARA LOS COEFICIENTES DE DIF DIV PERO DIF DIV PARA LA TABLA DE 
DIFERENCIAS DIVIDIDAS.