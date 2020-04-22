%% 1.4. Aproximacio de dades.
%  Apartat B

clc
clear all
format short 

any=[1975 1980 1985 1990 1995 2000 2005 2010];
grecia=[72.3 73.6 75.1 77.0 77.6 77.9 79.2 80.4];
republica=[45.9 48.9 49.8 48.7 46.2 43.9 44.4 47.5];

n=length(any);
for i = 1:7
poli_G=polyfit(any,grecia,i);
poli_R=polyfit(any,republica,i);

estimacio_G=polyval(poli_G,any);
estimacio_R=polyval(poli_R,any);

ECM1=sqrt(sum((estimacio_G-grecia).^2)/n);
ECM2=sqrt(sum((estimacio_R-republica).^2)/n);

i
taula=[estimacio_G ECM1; estimacio_R ECM2]
estimacio_demanada_G=polyval(poli_G,[1970 1992 2007])
estimacio_demanada_R=polyval(poli_R,[1970 1992 2007])
end


