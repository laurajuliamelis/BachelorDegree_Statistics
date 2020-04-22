%% gausselim sofi 
A=[10 -1 2 0 ; -1 11 -1 3; 2 -1 10 -1; 0 3 -1 8];
b = [6 25 -11 15]
U=triu(A,1)
L=tril(A,-1)
D=diag(diag(A))
Bgs = -inv(L+D)*U;
cgs = inv(L+D)*b';
roBj=max(abs(eig(Bgs)))

x=[0;0;0;0];
iter=6;
for i=1:iter
    x=Bgs*x + cgs;
end
x'