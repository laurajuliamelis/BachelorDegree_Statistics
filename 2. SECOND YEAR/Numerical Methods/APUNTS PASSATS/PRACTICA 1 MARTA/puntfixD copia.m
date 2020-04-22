clear all
format long g
g = @(x)x + 2/7*(x.^3 - x.^2 - x - 1);
x(1)=1.5;
x(2)=g(x(1));
eps=0.5*(10.^-6);
dif(1)=x(2)-x(1);

n=2
while (abs(x(n)-x(n-1)) > eps) & (abs(g(x(n))) > eps)
    dif(n)=x(n)-x(n-1)
   x(n+1)=g(x(n));
   n=n+1
end

n=[1:n];
dif=[0,dif];
taula_resultats=[n;x;g(x);dif]'
