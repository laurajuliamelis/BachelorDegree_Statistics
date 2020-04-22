%% 1.4. SOLUCIONS D'EQUACIONS NO LINEALS.
%  Apartat 1
f = @(x)x.^3 + 4.*x.^2 - 10;
x=-4:0.05:5;
z=zeros(size(x));
x0=fzero(f,0);
plot(x, f(x), x, z, 'r',x0,0, '*'),grid,title('Grafica del polinomi')
