%% 1.3. Integraci? num?rica: f?rmules compostes. 
%  Apartat B

clc
clear all
format long g

f=@(t)(1./(sqrt(2*pi)))*exp(-(t.^2)./2);

for n=1:8;
    N=2.^n;
    a=-3;
    b=3;
    h=(b-a)/N;
    s=f(a)+f(b);
    for i=1:(N-1);
        s=s+2*f(a+i*h);  
    end
    T(n)=(h/2).*s;
end

p=1:8;
taula_resultats=[2.^p;T]'