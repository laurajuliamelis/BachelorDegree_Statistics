% cal la funcio i 
% calen a(1) i b(1) tal que a*b <0
format long g, format compact
f = @(x)x.^3 - x.^2 - x - 1;
disp('  doneu a i b tal que f(a)*f(b)<0  ')
a = input('  a = ');
b = input('  b = ');
tol = 0.0000001;
a(1)=a; b(1)=b; r(1)=0;
k=1;
er=abs(b(k)-a(k))/2;
while ( er >= tol)
        if (f(a(k))*f(b(k))<0)
        x(k)=(b(k)+a(k))./2;
        if (f(x(k))*f(a(k))<0)
                a(k+1)=a(k);
                b(k+1)=x(k);
        else
                a(k+1)=x(k);
                b(k+1)=b(k);
        end
        if(k>1)
            r(k)=(x(k)-x(k-1))/x(k);
        end
        er=abs(b(k+1)-a(k+1))/2;
        k=k+1;
        else er=0;
        disp('no es verifiquen les condicions de Bolzano')
        end
if k > 50, break, end
end 
x(k)=(b(k)+a(k))./2;
zero_b=x(k)
lr=log(r);
iterats_b = x'