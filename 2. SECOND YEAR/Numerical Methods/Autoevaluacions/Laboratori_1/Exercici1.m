%Comprovar les desigualtats seg�ents:

A=[3 1;0 4];
B=[1 2;-2 6];
C=[2 -5;3 4];

disp('La desigualtat 1 �s: ')
(A*B)*C==A*(B*C)

disp('La desigualtat 2 �s: ')
A*(B+C)==(A*B)+(A*C)

disp('La desigualtat 3 �s: ')
(A*B)'==B'*A'

disp('La desigualtat 4 �s: ')
(A+B)*C == (A*C)+(B*C)
