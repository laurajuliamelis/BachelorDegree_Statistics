% 1)
y=[1,0.916,0.836,0.741,0.624,0.224,0.265,0.291,0.316,0.429]
x=[0 0.2 0.4 0.8 1 1.4 1.6 1.8 2.0 2.2]
% a)
[m,n]=size(x);
a=zeros(n);
 a(1:n,1)=y';
 for j=1:n-1
   for i=1:n-j
       a(i,j+1)=(a(i+1,j)-a(i,j))/(x(i+j)-x(i));
   end
 end
 taula=[x',a]


%% a(:,:) taula de difer?ncies dividides
%% x(:) abscises dels nodes d'interpolaci?

c=a(1,1:n);
l=zeros(n);
l(1,n)=1;
l(1,:)=c(1).*l(1,:);
for i=1:n-1
  p=x(1:i);
  l(i+1,n-i:n)=poly(p);
  l(i+1,:)=c(i+1).*l(i+1,:);
end
display('polinomi interpolador')
pd=sum(l)  %polinomi interpolador
% fx0=polyval(pd,3) % avaluaci? polinomi interpolador 
% b)
polyval(pd,1.25)
% c)
deriv=pd.*[9:-1:0]
deriv=deriv(1:9)
polyval(deriv,1.25)
% d)
xx=[-0.05:0.01:2.25]
plot(x,y,'*')
pol=polyval(pd,xx);
plot(x,y,'*',xx,pol)
% 2)
% a)
x=[0:0.05:1];
f=@(x)exp(-x);
g=@(x)x;
plot(x,f(x),x,g(x))
% b)
f=@(x)x-exp(-x);


