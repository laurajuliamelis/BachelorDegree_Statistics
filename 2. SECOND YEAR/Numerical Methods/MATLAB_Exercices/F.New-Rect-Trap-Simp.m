%% F�rmules de Newton-Cotes (b�siques)
f=@(x)log(x);
a=1; b=2;
ve=2*log(2)-1;
%% F�rmula del rectangle
h=b-a; 
R=h*(f((a+b)/2)) %valor aproximat
eR=abs(R-ve) % error
%% F�rmula del trapeci
h=b-a; 
T=(h/2)*(f(a) + f(b)) %valor aproximat
eT=abs(T-ve) % error
%% F�rmula de Simpson
h=(b-a)/2; 
S=(h/3)*(f(a) +4*f((a+b)/2)+  f(b)) %valor aproximat
eS=abs(S-ve) % error
%% Relaci� Simpson amb les altres
SS=2/3*R +1/3*T
