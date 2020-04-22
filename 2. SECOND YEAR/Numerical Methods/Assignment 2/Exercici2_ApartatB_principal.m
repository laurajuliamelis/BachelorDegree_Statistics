%% 1.2. Algebra lineal numerica: valors propis.
%  Apartat B - Programa principal

clc
clear all
format short

A=wilkinson(7);
q=ones(7,1); 
tol=10*10^(-8); 
[rho_max]=Exercici2_ApartatB_funcio(A,tol,q)
