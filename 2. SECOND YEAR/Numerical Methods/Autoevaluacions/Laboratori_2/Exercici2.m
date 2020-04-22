%Decimals exactes i xifres significatives:
x = input('Escriu el valor de x:');
y = input('Escriu el valor de y:');

error_abs = abs(x-y);
error_r = error_abs/abs(x);

disp('Els decimals exactes (de l.error absolut) són:')
i=1;resultat1=0;
while error_abs<(0.5*(10^-i))
    resultat1 = i;
    i=i+1;
end
disp(resultat1)

disp('Les xifres significatives (de l.error relatiu) són:')
j=1;resultat2=0;
while error_r<(0.5*(10^-j))
    resultat2 = j;
    j=j+1;
end
disp(resultat2)
