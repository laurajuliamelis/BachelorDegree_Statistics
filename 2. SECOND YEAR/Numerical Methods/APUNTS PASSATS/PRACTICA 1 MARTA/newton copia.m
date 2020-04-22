clear all
format short
f = @(x) x.^3 - x.^2 - x - 1;
df = @(x) 3.*x.^2 - 2.*x - 1;
x(1)=2;
x(2)=x(1)- f(x(1))./df(x(1));
eps=0.5*(10.^-6);
dif(1)=x(2)-x(1);
r(1)=(x(2)-x(1))./x(2)

n=2
while (abs(x(n)-x(n-1)) > eps) & (abs(f(x(n))) > eps)
   dif(n)=x(n)-x(n-1);
   x(n+1)= x(n)- f(x(n))./df(x(n));
   r(n)=(x(n)-x(n-1))./x(n)
   n=n+1;
end
r(n)=x(n)-x(n-1)

n=[1:n];
dif=[0,dif];
taula_resultats=[n;x;f(x);dif]'

plot(n,r)