clear all
format long g

g = @(x)x+1/9.*(x.^3 - x.^2 - x - 1);
x(1)=1.5;
x(2)=g(x(1));
eps=0.5*(10.^-6);
dif(1)=x(2)-x(1);
r(1)=(x(2)-x(1))./x(2);

m=2
while (abs(x(m)-x(m-1)) > eps) & (abs(g(x(m))) > eps)
    dif(m)=x(m)-x(m-1);
    r(m)=(x(m)-x(m-1))./x(m);
   x(m+1)=g(x(m));
   m=m+1
end

lpfC=log(r);
c=m;
plot(1:(c-1),lpfC,'g')
m=[1:m];
dif=[0,dif];
taula_resultats=[m;x;g(x);dif]'
