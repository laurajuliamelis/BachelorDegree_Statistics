%%Exercici 3 
f=@(x)2./(2+sin(10*pi.*x));
ve=1.1547005383792515290183;
a=0; b=1;
format long g 
for k=1:4
 n=2^k;
 h=(b-a)/n
 x=a:h:b;
y=f(x);
T(k)=trapz(x,y)
error(k)=abs(T(k)-ve)
end 
T2(2)=T(2)+(T(2)-T(1))/3
T2(3)=T(3)+(T(3)-T(2))/3
T2(4)=T(4)+(T(4)-T(3))/3
error2=abs(T2-ve) %% per veure si han millorat els decimals 
T3(3)=T2(3)+(T2(3)-T2(2))/15
T3(4)=T2(4)+(T2(4)-T2(3))/15
error3=abs(T3-ve)
T4(4)=T3(4)+(T3(4)-T3(3))/63
error4=abs(T4-ve)
%%Via Quad de Matlab 
quad(f,a,b,10.^(-6))