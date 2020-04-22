%% Gaus Seidel (error)
A=[10 -1 2 0 ; -1 11 -1 3; 2 -1 10 -1; 0 3 -1 8]
b=[6 25 -11 15]
U=triu(A,1)
L=tril(A,-1)
D=diag(diag(A))
Bgs=-inv(L+D)*U;
cgs=inv(L+D)*b';

x=[0;0;0;0];
iter=6;
error=1
while error>0.0005
    x=Bgs*x+cgs
    error=norm(b'-A*x);
end
x'
error