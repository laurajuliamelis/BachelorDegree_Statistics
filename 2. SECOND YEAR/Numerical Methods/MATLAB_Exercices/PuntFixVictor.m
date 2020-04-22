t=@(y)y+lam(x.^3 - x.^2 - x - 1)
f = @(x)x.^3 - x.^2 - x - 1;
exacte=fzero(f,2)
t=2;
%l=-1/13
l=-1/26;
i=1;
while abs(t-exacte)>0.000001 
    t=t+l*(t^3+t-9);
    w(i)=t;
    if i>1
    y(i)=log(abs((w(i)-w(i-1)))/w(i));
    end
    i=i+1;
end
t
