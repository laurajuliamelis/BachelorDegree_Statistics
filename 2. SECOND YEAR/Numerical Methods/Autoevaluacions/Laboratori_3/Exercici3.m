%Funci� que calculi e^x a partit de la s�roe de Taylor en x=0.
x= input('Introdueix un valor x:');
n= input('Introdueix un valor n:');

taylor(x,n);

function [resultat] = taylor(x,n)
% Aquesta funci� calcula e^x per a tot x.
resultat = 0; i=0;
for i=0:n
    resultat = resultat + (x^i)/(factorial(i));
end

disp(resultat)
end



  
  