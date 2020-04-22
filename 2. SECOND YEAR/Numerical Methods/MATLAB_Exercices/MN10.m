x=[-2:0.02:2];
f=@(x)x.^2-2
plot(x,f(x)),grid
zero1=fzero(f,1)
zero2=fzero(f,-1)
hold on
plot(x,zeros(size(x)),'g')
