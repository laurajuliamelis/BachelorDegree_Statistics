%% 1.4. SOLUCIONS D'EQUACIONS NO LINEALS.
%  Apartat 4

clc
clear all
format long g

%% Metode iteratiu convergent ii).
g=@(x)0.5*sqrt(10 - x.^3);
x(1)=1;
x(2)=g(x(1));
eps=0.5*(10.^-6);
dif(1)=x(2)-x(1);
r(1)=(x(2)-x(1))./x(2);

m=2
while (abs(x(m)-x(m-1)) > eps) && (abs(g(x(m))) > eps)
    dif(m)=x(m)-x(m-1);
    r(m)=(x(m)-x(m-1))./x(m);
   x(m+1)=g(x(m));
   m=m+1;
end

l1=log(r);

%% Metode iteratiu convergent iii).
g=@(y) y-((y.^3 + 4.*y.^2 - 10)/(3.*y.^2 + 8.*y));
y(1)=1;
y(2)=g(y(1));
dif2(1)=y(2)-y(1);
r2(1)=(y(2)-y(1))./y(2);
n=2;

while (abs(y(n)-y(n-1)) > eps) && (abs(g(y(n))) > eps)
    dif2(n)=y(n)-y(n-1);
    r2(n)=(y(n)-y(n-1))./y(n);
   y(n+1)=g(y(n));
   n=n+1;
end

l2=log(r2);

%% Grafic dels logaritmes dels valors absoluts dels errors relatius aproximats
plot(1:(m-1),l1,'blue', 1:(n-1),l2,'red')
xlabel('Iteracio')
ylabel('Logaritmes errors relatius')
legend('Punt fix ii)','Punt fix iii)')
