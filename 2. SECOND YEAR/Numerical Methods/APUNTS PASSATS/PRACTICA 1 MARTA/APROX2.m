function [ output ] = APROX2( n )
% funció demanada al apartat 3 del primer exercici
r = [1:n];
t=1;
for i = 1:length(r)
    t = t*(exp(1)*r(i))/n;
end
output=t/sqrt(2*pi*n)
end

