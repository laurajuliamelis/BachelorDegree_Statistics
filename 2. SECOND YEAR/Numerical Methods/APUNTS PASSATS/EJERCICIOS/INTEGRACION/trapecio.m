%% Exercici 3
f=@(x)2./(2+sin(10.*pi.*x));
vd=1.1547005383792515290183;
a=0; b=1;
n=16;

format long
for k=1:4
    n=2^k;
    h=(b-a)/n
    x=a:h:b;
    y=f(x);
    T(k)=trapz(x,y)
    errr=abs(T(k)-vd)
end
T2(2)=T(2)+(T(2)-T(1))/3    
T2(3)=T(3)+(T(3)-T(2))/3    
T2(4)=T(4)+(T(4)-T(3))/3  
errr2=abs(T2-vd)
T3(3)=T2(3)+(T2(3)-T2(2))/15
T3(4)=T2(4)+(T2(4)-T2(3))/15
errr3=abs(T3-vd)
T4(4)=T3(4)+(T3(4)-T3(3))/63
errr4=abs(T4-vd)

%% Via Quad de Matlab
Quad(f,a,b,10.^(-6))

