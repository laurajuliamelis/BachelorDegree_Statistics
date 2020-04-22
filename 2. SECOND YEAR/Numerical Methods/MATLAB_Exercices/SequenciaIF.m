%%exemple (no funciona) sentencia IF 
%% una mica rar 
format short g
z=2; k=0;
for k=1:10
if k<10
    k=k+1
    z=1/2*(z+2/z);
else
    disp('he acabat')
end 
end
error=abs(z-sqrt(2))
%%sense el for no ho fa deu vegades, l'ahuria d'ajudar amb un while per fer les 10
%%iteracions per això no es un bon exemple per a la sentencia IF. 