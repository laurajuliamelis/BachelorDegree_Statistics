%% JACOBI I GAUSS-SEIDEL

clear all
format short g

for N=3:20
D=linspace(-4,-4,N);
A=diag(D);
for i=1:N
    x(i,1)=0;
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
cj=(inv(D))*b;
roBj=max(abs(eig(Bj)));       
Bgs = -inv(L+D)*U;
cgs = inv(L+D)*b;
roBgs=max(abs(eig(Bgs)));


xJ=x;
xG=x;
errorJ=1;
iterJ(N)=0;
while errorJ>0.00000005
   iterJ(N)=iterJ(N)+1;
   xJ=Bj*xJ+cj;
   errorJ = norm(b-A*xJ);
end
residuJ(N)=norm(b-A*xJ);


errorG=1;
iterG(N)=0;
while errorG>0.00000005
   iterG(N)=iterG(N)+1;
   xG=Bgs*xG+cgs;
   errorG = norm(b-A*xG);
end
residuG(N)=norm(b-A*xG);
residuJ(N)>residuG(N)

end

taula_resultats=[(3:20); iterJ(3:20); residuJ(3:20); iterG(3:20); residuG(3:20); residuJ(3:20)>residuG(3:20)]'

