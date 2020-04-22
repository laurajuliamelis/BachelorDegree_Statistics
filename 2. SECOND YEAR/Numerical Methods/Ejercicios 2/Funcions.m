F=@(x,n)(x.^n.*exp(x-1));
x=0:0.01:1;
y1=F(x,1);y10=F(x,10);
y2=F(x,5);y20=F(x,20);
y3=F(x,15);y30=F(x,30);
plot(x,y1,x,y2,x,y3,x,y10,x,y20,x,y30)
title('n=1,5,10,15,20,30') 