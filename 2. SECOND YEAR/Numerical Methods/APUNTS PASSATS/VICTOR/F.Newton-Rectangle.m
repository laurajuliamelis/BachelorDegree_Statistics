%% Fórmules de Newton-Cotes (composta)
f=@(x)log(x);
a=1; b=2;
ve=2*log(2)-1;
%% Fórmula del rectangle
n=input('nombre intervals')
h=(b-a)/n; 
for i=0:n
    x(i+1)=a+i*h;
end
for i=1:n
    R(i)=f((x(i)+x(i+1))/2);
end
IR=h*sum(R)
%R=h*(f((a+b)/2)) %valor aproximat
%eR=abs(R-ve) % error