%%x=[1 1.125 1.250 1.375 1.5 1.625 1.750 1.875 2.0];%%y=[0 0.169925 0.321928 0.459432 0.584962 0.700440 0.807355 0.906891 1];function taula = difdiv(x,y)n=length(x');a=zeros(n);a(1:n,1)=y';for j=1:n-1for i=1:n-ja(i,j+1)=(a(i+1,j)-a(i,j))/(x(i+j)-x(i));endendtaula=[x',a]'endCOMPLEMENTAR CON VANDERMONDE PARA RESULTADOS