%% exercici 5 diapositives Lab12
%% Condicionament d'una matriu 
A=[10 7 8 7 ; 7 5 6 5; 8 6 10 9; 7 5 9 10]
b=[32;23;33;31]
bb=[32.1;22.9;33.1;30.9]
%% solució sistema Ax=b 
x=A\b
%% solució sistema Ax=bb 
xx=A\bb
%% Anàlisi errors 
error_b=norm(b-bb,'inf')
error_x=norm(x-xx,'inf')
%% Es compleix fórmula???
cond1=error_x/norm(x,'inf')
cond2=cond(A,'inf')*error_b/norm(b,'inf')