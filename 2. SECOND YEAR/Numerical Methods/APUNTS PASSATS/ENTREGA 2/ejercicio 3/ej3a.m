     for N=6:30  A = zeros(N,N);  B = zeros(1,N);    for i = 1:N          %for matrix B          if (i == 1 || i == N)              B(i) = 3;          elseif (i == 2 || i == N-1)              B(i) = 2;          else              B(i) = 1;          end      for j = 1:N          %for matrix A          if (abs(i-j) <= 2) && (i ~= j)              A(i,j) = -1;          elseif i == j              A(i,j) = 5;          elseif abs(i-j) > 2              A(i,j) = 0;          end                     end      end    determinante(N)=det(A);  if determinante(N)==0      ncond(N)=10.^10;  else      ncond(N)=det(A).*det(inv(A));  end  end    tabla= [6:1:30; determinante(6:30); ncond(6:30)]'