%% Fórmules de Newton-Cotes (bàsiques)
f=@(x)log(x);
a=1; b=2;
ve=2*log(2)-1;
%% Fórmula del rectangle
h=b-a; 
R=h*(f((a+b)/2)) %valor aproximat
eR=abs(R-ve) % error
%% Fórmula del trapeci
h=b-a; 
T=(h/2)*(f(a) + f(b)) %valor aproximat
eT=abs(T-ve) % error
%% Fórmula de Simpson
h=(b-a)/2; 
S=(h/3)*(f(a) +4*f((a+b)/2)+  f(b)) %valor aproximat
eS=abs(S-ve) % error
%% Relació Simpson amb les altres
SS=2/3*R +1/3*T
