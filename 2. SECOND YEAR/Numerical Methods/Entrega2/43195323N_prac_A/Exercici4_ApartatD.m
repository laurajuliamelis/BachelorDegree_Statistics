%% 1.4. Aproximacio de dades.
%  Apartat C

clc
clear all
format short e

x=[1975 1980 1985 1990 1995 2000 2005 2010];
y1=[72.3 73.6 75.1 77.0 77.6 77.9 79.2 80.4];
y2=[45.9 48.9 49.8 48.7 46.2 43.9 44.4 47.5];

c1=polyfit(x,y1,6);
c2=polyfit(x,y2,6);

c3=polyfit(x,y1,7);
c4=polyfit(x,y2,7);

z=linspace(x(1),x(end),100);

p1=polyval(c1,z);
p2=polyval(c2,z);

p3=polyval(c3,z);
p4=polyval(c4,z);

x2=[1970 1992 2007 2015];
estimacio_G_6=polyval(c1,[1970 1992 2007 2015]);
estimacio_R_6=polyval(c2,[1970 1992 2007 2015]);

estimacio_G_7=polyval(c3,[1970 1992 2007 2015]);
estimacio_R_7=polyval(c4,[1970 1992 2007 2015]);

plot(z,p1,z,p2,z,p3,z,p4, x, y1,'o',x,y2,'o',x2,estimacio_G_6,'*',x2,estimacio_R_6,'*',x2,estimacio_G_7,'*',x2,estimacio_R_7,'*');grid on;
legend('Funcio Grecia grau 6','Funcio Republica grau 6','Funcio Grecia grau 7','Funcio Republica grau 7','Dades Grecia','Dades Republica','Estimacions Grecia grau 6', 'Estimacions Republica grau 6','Estimacions Grecia grau 7', 'Estimacions Republica grau 7', 'Location', 'best')