format short
f = @(x)x.^3-x.^2-x-1;
a(1)= 1;
b(1)= 2;
l(1)=b(1)-a(1);
m(1)=(a(1)+b(1))./2;
eps=0.5.*10.^-6
while abs(b-a)>eps
   if f(a(k))*f(m(k))<0
       a(k+1)=a(k);
       b(k+1)=m(k);
   else
       a(k+1)=m(k);
       b(k+1)=b(k);
      
   end
   m(k+1)=(a(k+1)+b(k+1))./2;
   l(k+1)=b(k+1)-a(k+1);
       
end

taula_resultats=[a; m; b; f(m); l]'