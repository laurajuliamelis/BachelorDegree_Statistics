%% Exercici 2
%% Omplir la matriu triangular superior 
n=20;
U=zeros(n); 
for j=1:n
    for i=1:j
        U(i,j)=cos(i+j)
    end
    b(j,1)=tan(j);
end
%[U,b]