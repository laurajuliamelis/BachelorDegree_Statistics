%% Exercici 3
%% Omplir la matriu triangular inferior
n=5;
L=zeros(n); 
for j=1:n
    for i=j:n
        L(i,j)=i+j;
    end
    b(j,1)=j;
end 
[L,b]