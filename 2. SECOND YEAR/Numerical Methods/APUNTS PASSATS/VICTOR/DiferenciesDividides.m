%% Taula de defirencies dividides

function taula = difdiv(x,y)
n=length(x);
a=zeros(n);
a(1:n,1)=y';
for j=1:n-1
for i=1:n-j
a(i,j+1)=(a(i+1,j)-a(i,j))/(x(i+j)-x(i));
end
end
taula=[x',a]
end