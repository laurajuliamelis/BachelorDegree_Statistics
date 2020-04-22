%Comprovar les desigualtats següents:

A=[3 1;0 4];
B=[1 2;-2 6];
C=[2 -5;3 4];

disp('La desigualtat 1 és: ')
(A*B)*C==A*(B*C)

disp('La desigualtat 2 és: ')
A*(B+C)==(A*B)+(A*C)

disp('La desigualtat 3 és: ')
(A*B)'==B'*A'

disp('La desigualtat 4 és: ')
(A+B)*C == (A*C)+(B*C)
