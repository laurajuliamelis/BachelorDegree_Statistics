% Exercici
x=[0:0.02:2];
f=@(x)x.^6-x-1;
plot(x,f(x)),grid
% On creua ambel 0?
aprox=fzero(f,1)
% Veure si iterant 5 vegades aquest valor s'aproxima al aprox
f1=@(x)x.^6-x-1;
f2=@(x)x.^6-1;
y1=1
y2=1
for n=1:5
    y1=f1(y1)
    y2=f2(y2)
end