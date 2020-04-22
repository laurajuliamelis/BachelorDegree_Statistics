%% 1.3. ERROR DE TRUNCAMENT.
%  Apartat 1 - Funcio

function [pi] = Exercici3_Apartat1_Funcio (N)

pi=0;
for n=0:N
pi= 16^(-n)*(4/(8*n+1)-2/(8*n+4)-1/(8*n+5)-1/(8*n+6)) + pi;

end
pi;
end

