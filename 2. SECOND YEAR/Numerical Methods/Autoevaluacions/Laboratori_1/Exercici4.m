%Obtindre una matriu quadrada d'ordre la suma dels digits del DNI.

dni = input('Escriu el teu DNI en format vector: ');

format short g
A= rand(sum(dni));

disp('La seva inversa és')
inv(A)

disp('Si esborrem les columnes 2 i 4 ens queda:')
A(:,[1 3 5 6])

disp('L arrel quadrada de les dades de ma matriu és:')
sqrt(sum(A(:)))

disp('Les mitjanes per files són:')
mean(A')   % Per defecte mean realitza la mitjana per columnes

disp('Les mitjanes per files són:')
std(A')

