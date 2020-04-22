%Escriu una funció i dibuixa-la. A més, troba els punts amb els eixos.

x=[0:0.01:2];
f=@(x)x.^6-x-1;
plot(x,f(x)), title('Exercici 3'), xlabel('x'), ylabel('f(x)'), grid

%Talls amb els eixos:
tall = fzero(f,[0 2])

%Afegim el tall al gràfic:
hold on
plot(tall,0, 'g*')
hold off
