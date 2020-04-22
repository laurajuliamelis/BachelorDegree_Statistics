%% 1.1. Algebra lineal numerica: metodes iteratius
%  Apartat B

clc
clear all
format long g

for N=3:20
d=linspace(-4,-4,N);
A=diag(d);
for i=1:N
    b(i,1)=0;
    b(1,1)=-2;
    b(N,1)=-2;
    for j=1:N
         X(j,1)=1;
        if abs(i-j)==1
            A(i,j)=2;
        end
    end
end
r=b-A*X;
error(N)=norm(r);
end

taula_resultats=[3:20;error(3:20)]'