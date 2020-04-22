function [sol,niteraciones,error]= gauss_seidel(A,B,x0,cotaerror)
D=diag(diag(A));
U=triu(A)-D;
L=tril(A)-D;
Bgs=-inv(D+L)*U;
Cgs=inv(D+L)*B';
iter=1;
xant=x0;
xsig=Bgs*xant+Cgs;
while norm(xsig-xant,inf)>cotaerror
iter=iter+1;
xant=xsig;
xsig=Bgs*xant+Cgs;
end
sol=xsig(:,end);
niteraciones=iter;
error=norm(xsig-xant,inf);
end
