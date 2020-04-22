%% 1.1. Algebra lineal numerica: metodes iteratius
%  Apartat A

clc
clear all
format long g

for N=3:20
d=linspace(-4,-4,N);
A=diag(d);
for i=1:N
    for j=1:N
        if abs(i-j)==1
            A(i,j)=2;
        end
    end
end
deter(N)=det(A);
if deter(N)==0
    ncond(N)=10.^10;
else
    ncond(N)=cond(A);
end
end

taula_resultats=[3:1:20;deter(3:20);ncond(3:20)]'
