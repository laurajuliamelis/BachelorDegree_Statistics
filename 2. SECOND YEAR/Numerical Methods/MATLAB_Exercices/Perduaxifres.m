%%exemple algorisme amb pèrdues de xifres significatives
x=1./(5.^[0:25])'
F=@(x)sqrt(x.^2+1)-1;
y=F(x);
G=@(x)x.^2./(sqrt(x.^2+1)+1);
z=G(x);
taula_resultats=[[0:25]',x,y,z]