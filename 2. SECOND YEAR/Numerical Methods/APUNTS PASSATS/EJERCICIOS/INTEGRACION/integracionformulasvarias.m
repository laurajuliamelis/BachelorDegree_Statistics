%% Formules de Newton-Cotes (basicas)
f=@(x)7.73688888888834.*x.^6-44.3733333333306.*x.^5+101.688888888884.*x.^4-119.679999999995.*x.^3+76.3342222222199.*x.^2 -23.4266666666661.*x + 2.99999999999996;
a=0.25; b=1.75;
ve=2*log(2)-1;

%% Formula del rectangulo
h=b-a;
R=h*(f((a+b)/2))   % valor aproximado
eR=abs(R-ve)       % error

%% Formula del trapecio
h=b-a;
T=(h/2)*(f(a)+f(b))  % valor aproximado
eT=abs(T-ve)       % error

%% Formula del Simpson
h=(b-a)/2;
S=(h/3)*(f(a)+4*f((a+b)/2)+f(b))  % valor aproximado
eS=abs(S-ve)       % error

%%Relacion Simpson con las otras
SS =2/3*R+1/3*T


f=@(x)log(x);
b=exp(1);
a=1;
ve=1;
%% F�rmula del rectangle
h=b-a;
R=h*(f((a+b)/2))   % valor aproximat
eR=abs(R-ve)
%% F�rmula del trapezi
h=b-a;
T=(h/2)*(f(a)+f(b))  % valor aproximat
eT=abs(T-ve)       % error
%% F�rmula del Simpson
h=(b-a)/2;
S=(h/3)*(f(a)+4*f((a+b)/2)+f(b))  % valor aproximat
eS=abs(S-ve)       % error
%%Relaci� Simpson amb les altres
SS =2/3*R+1/3*T
