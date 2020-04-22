%Calcular 1+2+...+n per a diferents valors de n.
n= input('Introdueix un valor n:');
sum=0;
for i=1:1:n
    sum=sum + i;
 
end
disp(sum)

%Calcular 1^p+2^p+...+m^p per a diferents valors de m i p.
m= input('Introdueix un valor m:');
p= input('Introdueix un valor p:');

suma=0;
for j=1:1:m
    suma=suma + (j^p);
   
end
disp(suma)