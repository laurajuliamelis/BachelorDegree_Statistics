%grafica
x=[-2:0.02:2]; %abscises
y=x.^2+1 %ordenades
z=2*ones(size(x));
plot(x,y,x,z,'*'), title('paràbola')
pause(5)
plot(x,sin(pi*x)),title('sinus')
figure(2)
plot(x,cos(pi*x),'c'),title('cosinus')
figure(3)
plot(x,sin(pi*x),'m'),title('sinus')