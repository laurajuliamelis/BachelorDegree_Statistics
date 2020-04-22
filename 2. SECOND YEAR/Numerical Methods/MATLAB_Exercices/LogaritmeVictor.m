%% Exercici 2.1.b %%
A=[   1           log(0.25)
      1           log(0.5)           
      1           log(0.75 )       
      1           log(1)           
      1           log(1.25)         
      1           log(1.5)          
      1           log(1.75)   ];
b=[log(0.4);log(0.5);log(0.9);log(1.28);log(1.6);log(1.66);log(2.02)];
C=A'*A;
d=A'*b;
xl=C\d;
x=[exp(xl(1,1));xl(2,1)]%% solucion

disp('residu')
residu=norm(b-A*xl)