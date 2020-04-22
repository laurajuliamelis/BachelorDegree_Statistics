% cal la funcio i
% la seva derivada derivada
format compact, format long
clear x
f=@(x)x.^3+x-9
fp=@(x)3*x.^2+1
x(1) = input('doneu x0 ');
d = 1000000;
p=1; m=1;r(1)=0;
y=f(x(p))       %%% tolf
s=y/fp(x(p));   %%% tolx
while (abs(y) > 0.5*(10^(-d)));
        p=p+1;
        x(p)=x(p-1)-s;
        y=f(x(p));
        s=y/fp(x(p));
if (abs(s) < 0.5*(10^(-d))), break, end
if ( p > 15), break, end
if(p>1)
            t(p-1)=(x(p)-x(p-1))/x(p)
end
end
zero_n=x(p);
lnt=log(t);
iterats_n = x'