%% 1.2. Algebra lineal numerica: valors propis.
%  Apartat C 

clc
clear all
format short

A=wilkinson(7);
q=ones(7,1); 
tol=10*10^(-8); 

[rho_max]=Exercici2_ApartatB_funcio(A,tol,q);
A2=A-rho_max*eye(7);

[rho_aux]=Exercici2_ApartatB_funcio(A2,tol,q); 
rho_min=rho_aux+rho_max