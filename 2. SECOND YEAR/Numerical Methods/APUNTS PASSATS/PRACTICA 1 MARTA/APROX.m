function [ output ] = APROX( n )
% funció demanada al apartat 3 del primer exercici
num=factorial(n);
denom=(n.^n)*(exp(1).^(-n))*sqrt(2.*pi.*n);
output=num./denom
end

