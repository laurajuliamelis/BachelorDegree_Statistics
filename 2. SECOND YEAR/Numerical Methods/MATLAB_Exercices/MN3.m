%%% Exemple algorisme estable/inestable
format short g

disp(' Algorisme inestable')
I(1)=1/exp(1);
IE(1)=quad(@(x)x.*exp(x-1),0,1,1.0e-16);
for n=2:50
I(n)=1-n*I(n-1);
IE(n)=quad(@(x)x.^n .*exp(x-1),0,1,1.0e-16);
end
break
disp('Algorisme estable')
J(50)=0;
for n=50:-1:2
J(n-1)=(1-JNo)/n;
end
R=[I',J',IE' abs(I-IE)',abs(J-IE)']
plot(1:50,R(:,4),'r',1:50,R(:,5),'g')