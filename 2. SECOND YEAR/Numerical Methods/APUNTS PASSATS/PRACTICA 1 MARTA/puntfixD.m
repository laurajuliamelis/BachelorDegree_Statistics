clear all
format long g

g = @(x)x + 2/7*(x.^3 - x.^2 - x - 1);
x(1)=1.5;
x(2)=g(x(1));
eps=0.5*(10.^-6);
dif(1)=x(2)-x(1);
r(1)=(x(2)-x(1))./x(2);

n=2
while (abs(x(n)-x(n-1)) > eps) & (abs(g(x(n))) > eps)
    dif(n)=x(n)-x(n-1)
     r(n)=(x(n)-x(n-1))./x(n);
   x(n+1)=g(x(n));
   n=n+1
end

lpfD=log(r);
d=n;
plot(1:(d-1),lpfD,'black')
xlabel('Iteració')
ylabel('Logaritmes errors relatius')
legend( 'Bisecció','Newton','Punt fix landa=1/9','Punt fix landa=2/7')
hold off
n=[1:n];
dif=[0,dif];
taula_resultats=[n;x;g(x);dif]'
