%% 1.4. Aproximacio de dades.
%  Apartat C

clc
clear all
format long g

any=[1975 1980 1985 1990 1995 2000 2005 2010];
grecia=[72.3 73.6 75.1 77.0 77.6 77.9 79.2 80.4];
republica=[45.9 48.9 49.8 48.7 46.2 43.9 44.4 47.5];

poli_G_grau7=polyfit(any,grecia,7);
poli_R_grau7=polyfit(any,republica,7);

estimacio_G_grau7=polyval(poli_G_grau7,2015)
estimacio_R_grau7=polyval(poli_R_grau7,2015)

poli_G_grau6=polyfit(any,grecia,6);
poli_R_grau6=polyfit(any,republica,6);

estimacio_G_grau6=polyval(poli_G_grau6,2015)
estimacio_R_grau6=polyval(poli_R_grau6,2015)