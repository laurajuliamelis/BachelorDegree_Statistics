%% 1.4. SOLUCIONS D'EQUACIONS NO LINEALS.
%  Apartat 3a

clc
clear all
format long g

%% Metode iteratiu i).
f = @(x)x.^3 + 4.*x.^2 - 10;
x(1)=2; % Arrel positiva de l'equaci? donada
for i=2:10
    x(i)=x(i-1)-x(i-1).^3-4.*x(i-1).^2+10;
    tolx(i)=abs(x(i)-x(i-1));
end


% Estudi convergencia:
tolf1=abs(f(x))';  % Divergent
sol1=[x',tolx',tolf1] 

%% Metode iteratiu ii).
f = @(x)x.^3 + 4.*x.^2 - 10;
y(1)=2;
for i=2:10
    y(i)=0.5*sqrt(10 - y(i-1).^3);
    toly(i)=abs(y(i)-y(i-1));
end 

% Estudi convergencia:
tolf2=abs(f(y))';  % Convergent
sol2=[y',toly',tolf2]

%% Metode iteratiu iii).
f = @(x)x.^3 + 4.*x.^2 - 10;
z(1)=2; 
for i=2:10
    z(i)=z(i-1)-((z(i-1).^3 + 4.*z(i-1).^2 - 10)/(3.*z(i-1).^2 + 8.*z(i-1)));
    tolz(i)=abs(z(i)-z(i-1));
end

% Estudi convergencia:
tolf3=abs(f(z))';  % Convergent
sol3=[z',tolz',tolf3]