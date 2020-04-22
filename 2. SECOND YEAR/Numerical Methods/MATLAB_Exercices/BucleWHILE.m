%%exemple bucle WHILE
%% el més senzill 
% format short g
% y=2; k=0;
% while k<10
%      y=1/2*(y+2/y);
%      k=k+1
% end 
% error=abs(y-sqrt(2))
%creació vector per guardar les iteracions 
y(1)=2; k=1;
while k<11
       k=k+1
       y(k)=1/2*(y(k-1)+2/y(k-1))
end
error=abs(y-sqrt(2));
taula_resultats=[0:length(y)-1; y; error]'