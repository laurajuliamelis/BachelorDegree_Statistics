function [ x ] = bisec(f,a,b)
% mètode bisec del llibre de moler
% f=funcio
% [a,b] interval on la funcio canvia de signe
% x punt mig obtingut amb fita error
% abs(b-a)/abs(b) > eps
k =0;
while abs(b-a) > eps*abs(b)
    x= (a+b)./2;
    if sign(f(x)) == sign(f(b))
        b=x;
    else 
        a=x;
    end
    k=k+1;
end

end

%f([a,b]) em dona el valor de f en a i b