%%bucle for
%% el més senzill 
% x=2;
% for k=1:1:10
%         x=1/2*(x+2/x);
% end 
% error=abs(x-sqrt(2))
%creació vector per guardar les iteracions 
format long g
x(1)=2;
for k=1:10
        x(k+1)=1/2*(x(k)+2/x(k));
end 
error=abs(x-sqrt(2));
taula_resultats=[0:length(x)-1; x; error]'