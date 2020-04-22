function [ x, k ] = newton( f, fprime, xprev )
% mètode newton del llibre de moler
% f=funcio
% fprime = la derivada de la funcio
% xprev valor inicial
% x valor obtingut amb fita error
% abs(x-xprev)/abs(x) > eps
x = 2*xprev;
k = 0;
%while abs(x - xprev) > eps*abs(x)
while k < 5
    xprev = x;
    x = x - f(x)/fprime(x);
    k = k+1;
end

end

