%Generar v.a. de mostra n. Fer histograma i calcular mean i std.

n=input('Introdueixi la mida de la mostra:');
x=round(100*rand(1,n));

histogram(x,n)

mean(x)
std(x)




