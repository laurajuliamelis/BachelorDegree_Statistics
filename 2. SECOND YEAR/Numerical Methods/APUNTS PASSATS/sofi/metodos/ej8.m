%%ejercicio 8 . numero de condicion de una matriz (ap.2.2)clear allclcA=[10 7 8 7;7 5 6 5;8 6 10 9;7 5 9 10];disp('para x1')b1=[32; 23; 33; 31];x1=A\b1b2=[6; -7.2 ; 2.9; -0.1];disp('para x2')x2=A\b2r2=norm(b2-A*x2)b3=[1.50; 0.18; 1.19; 0.89];disp('para x3')x3=A\b3r3=norm(b3-A*x3)disp('numero de condicion')cond(A)