function [ output ] = STIRLING( n )
% funci� demanada al apartat 2 del primer exercici
output=(n.^n)*(exp(1).^(-n))*sqrt(2.*pi.*n)
end

