%% ejercicio 11%% definimos A y b . APARTADO 1disp('sistema lineal')A=[1 1; 2 1; 1 2; 2 3; 2 5; 2 4]b=[30.006; 44.013; 46.006; 76.012; 108.010; 92.011]%%ecuaciones normales en forma Bx=c. APARTADO 2disp('ecuaciones normales')rank(A)B=A'*Ac=A'*b%%vector residuo. APARTADO 3disp('solucion ecuacion normal')xn=B\c%%comparamos residuosdisp('residuo minimo')y=[14 16]'rn=norm(b-A*xn)r=norm(b-A*y)