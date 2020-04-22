%% Polinomi interpolador (mètode de Lagrange)
clear all
x=[1 2 4 5];
y=[0 2 12 21];

v = polyinterp(x,y,3)

%% gràfica
u=0:0.1:6;
v=polyinterp(x,y,u);
plot(u,v,'g',x,y,'b*')