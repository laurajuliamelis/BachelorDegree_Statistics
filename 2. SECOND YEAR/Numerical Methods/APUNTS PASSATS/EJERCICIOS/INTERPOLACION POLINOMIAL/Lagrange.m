%% Polinomi interpolador (metodo de Lagrange)



x=[1 2 4 5];
y=[0 2 12 21];
u=4;
n = length(x);
v = zeros(size(u));
for k = 1:n
w = ones(size(u));
for j = [1:k-1 k+1:n]
w = (u-x(j))./(x(k)-x(j)).*w;
end
v = v + w*y(k);
end
u=0:0.1:6;
plot(u,v,'g',x,y,'b*')
u


