%% calcul de 1^p+2^p+....n^p
n=input('dona el valor de n=')
p=input('dona el valor de p=')
x=1:n; xp=x.^p;
sp=sum(xp)