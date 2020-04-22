%% 1.2. ERRORS DE CANCEL?LACIO.
%  Apartat 2

% Grafica
clear all
format long g
f=@(x)x.^7-7.*x.^6+21.*x.^5-35.*x.^4+35.*x.^3-21.*x.^2+7.*x-1
x=0.9:0.05:1.2;
y=0.988:0.00005:1.012;
z=zeros(size(x));
x0=fzero(f,0);


plot(x, f(x),y, f(y), 'red',x0, 0, 'c*'), grid, title('Representacio grafica del polinomi')

% Avaluacio del polinomi
a=0.988:0.00005:1.012;
[a;f(a)]'

