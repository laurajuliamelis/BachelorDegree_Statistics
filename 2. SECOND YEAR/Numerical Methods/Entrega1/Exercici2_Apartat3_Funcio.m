%% 1.2. ERRORS DE CANCEL?LACIO.
%  Apartat 3 - Funcio

function[d]=Exercici2_Apartat3_Funcio(f,n,a,x)
%   f: polinomi a evaluar
%   n: nombre de graus del polinomi
%   a: coeficients del polinomi
%   x: valor en el que es vol evaluar el polinomi

d=a(n+1);

for i= 1:n

    d=a(n+1-i)+d.*x;
    
end
end
