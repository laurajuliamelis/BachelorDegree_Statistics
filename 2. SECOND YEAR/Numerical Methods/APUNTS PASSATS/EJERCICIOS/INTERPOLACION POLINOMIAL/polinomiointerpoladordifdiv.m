%% polinomi interpolador
% (metodo diferencias divididas de Newton)


clear all
x=[0 1 2 4 8];
y=[0 5 10 24 50];

t = difdiv(x,y)


p1=[1 -0];
p2=[1 -1];
p3=[1 -2];
p4=[1 -4];
p5=[1 -8];

q1=conv(p1,p2);
q2=conv(q1,p3)
q3=conv(q2,p4)
q4=conv(q3,p5)



