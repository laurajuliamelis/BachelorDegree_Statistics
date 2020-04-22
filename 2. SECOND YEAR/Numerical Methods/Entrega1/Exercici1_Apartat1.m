%% 1.1. ALGORITMES.
%  Apartat 1 - Funcio

function [xN] = Exercici1_Apartat1 (n)

k=10;

for i=1:k
    
    n(i)=5^i;
    x=rand(n(i),1);
    y=rand(n(i),1);
    z = x.^2+y.^2;
    v = (z <= 1);
    m=sum(v); 
    pi_n(i) =4* m/n(i);
    error_absolut(i)=abs(pi-pi_n(i));
    error_relatiu(i)=error_absolut(i)/abs(pi);
end
  xN= pi_n(k)
  taula_resultats= [n',pi_n',error_absolut',error_relatiu']
end

