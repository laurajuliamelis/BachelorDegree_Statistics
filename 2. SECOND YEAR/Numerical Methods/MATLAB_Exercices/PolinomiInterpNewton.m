%% polinomi interpolador
% (m�tode difer�ncies dividides de Newton)

clear all
%%x=[1 1.125 1.250 1.375 1.5 1.625 1.750 1.875 2.0];
%%y=[0 0.169925 0.321928 0.459432 0.584962 0.700440 0.807355 0.906891 1];

function taula = difdivv(x,y)
n=length(x');
a=zeros(n);
a(1:n,1)=y';
for j=1:n-1
for i=1:n-j
a(i,j+1)=(a(i+1,j)-a(i,j))/(x(i+j)-x(i));
end
end
taula=[x',a]
end

p1=[1 -0];
p2=[1 -1];
p3=[1 -2];
p4=[1 -4];
p5=[1 -8];

q1=conv(p1,p2);
q2=conv(q1,p3)
q3=conv(q2,p4)
q4=conv(q3,p5)



