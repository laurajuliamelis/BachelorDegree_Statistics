A=[3 -1 0; -2 1 1; 2 -1 4];b=[5;0;15];n=length(b);d=det(A);x=zeros(n,1);for i=1:n    Ab=[A(:,1:i-1),b,A(:,i+1:n)];    x(i)=det(Ab)/d;enddisp('Solución')disp(x);