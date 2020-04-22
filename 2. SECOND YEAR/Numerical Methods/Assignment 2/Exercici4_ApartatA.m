%% 1.4. Aproximacio de dades.
%  Apartat A

clc
clear all
format long g

any=[1975 1980 1985 1990 1995 2000 2005 2010];
grecia=[72.3 73.6 75.1 77.0 77.6 77.9 79.2 80.4];
republica=[45.9 48.9 49.8 48.7 46.2 43.9 44.4 47.5];

poli_G=polyfit(any,grecia,7);
poli_R=polyfit(any,republica,7);

estimacio_G=polyval(poli_G,[1970 1992 2007])
estimacio_R=polyval(poli_R,[1970 1992 2007])