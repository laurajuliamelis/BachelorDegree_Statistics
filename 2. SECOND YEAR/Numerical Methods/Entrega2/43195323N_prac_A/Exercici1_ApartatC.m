%% 1.1. Algebra lineal numerica: metodes iteratius
%  Apartat C

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
        if abs(i-j)==1
            A(i,j)=2;
        end
    end
end
U=triu(A,1);
L=tril(A,-1);
D=diag(diag(A));
Bj=-(inv(D))*(L+U);
roBj(N)=max(abs(eig(Bj)));
Bgs = -inv(L+D)*U;
cgs = inv(L+D)*b;
roBgs(N)=max(abs(eig(Bgs)));
end

hold on
plot(3:20,roBj(3:20))
plot(3:20,roBgs(3:20))
xlabel('N')
ylabel('Radi espectral')
legend( 'Metode Jacobi','Metode Gauss-Seidel', 'Location', 'southeast')
hold off

taula_resultats=[3:20;roBj(3:20);roBgs(3:20)]'