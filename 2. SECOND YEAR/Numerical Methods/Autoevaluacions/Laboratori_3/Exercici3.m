%Funció que calculi e^x a partit de la sèroe de Taylor en x=0.
x= input('Introdueix un valor x:');
n= input('Introdueix un valor n:');

taylor(x,n);

function [resultat] = taylor(x,n)
% Aquesta funció calcula e^x per a tot x.
resultat = 0; i=0;
for i=0:n
    resultat = resultat + (x^i)/(factorial(i));
end

disp(resultat)
end



  
  