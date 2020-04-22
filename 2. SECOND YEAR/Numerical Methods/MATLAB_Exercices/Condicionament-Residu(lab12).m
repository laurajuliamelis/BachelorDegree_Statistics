% %% exercici 5 diapositives Lab12
% %% Condicionament d'una matriu 
% A=[10 7 8 7 ; 7 5 6 5; 8 6 10 9; 7 5 9 10]
% b=[32;23;33;31]
% bb=[32.1;22.9;33.1;30.9]
% %% solució sistema Ax=b 
% x=A\b
% %% solució sistema Ax=bb 
% xx=A\bb
% %% Anàlisi errors 
% error_b=norm(b-bb)
% error_x=norm(x-xx)
% %% Es compleix fórmula???
% cond1=error_x/norm(x)
% cond2=cond(A)*error_b/norm(b)
% residu=norm(b-A*xx)
%% Residu petit en 
x1=[6;-7.2;2.9;-.01]
residu1=norm(b-A*x1)
x2=[1.50;0.18;1.19;0.89]
residu2=norm(b-A*x2)
