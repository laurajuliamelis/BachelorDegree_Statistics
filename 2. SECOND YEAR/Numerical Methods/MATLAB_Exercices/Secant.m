function [ x,k ] = secant( f,a,b )
% mètode secant del llibre de moler
% f=funcio
% a i b valors inicials del mètode
% x valor obtingut amb fita error
% abs(b-a)/abs(x) > eps
k=0;
while abs(b-a) > eps*abs(b)
c = a;
a = b;
b = b + (b - c)/(f(c)/f(b)-1);
k = k + 1;
end
x=b;
end

