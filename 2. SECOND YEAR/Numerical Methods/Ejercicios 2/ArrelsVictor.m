f=@(x)x.^3+x-9;
x=[-5:0.0001:5];
arrel=fzero(f,2)
plot(x,f(x),x,zeros(size(x)),arrel,0,'+b')
