function y=arrodonir(x,t)
%% Arrodonim el nombre donat a t xifres en coma flotant
 if (x==0),
    y=0;
    return;
end
e=fix(log10(abs(x)));
if e>0,                    %e>=0??
   e=e+1;
end
 y1=floor(x*10^(t-e))*10^(e-t);
 y2=ceil(x*10^(t-e))*10^(e-t);
 aux = abs(y1-x)*10^(t-e);
 if (aux < 0.5) 
     y=y1;
     return;
 end
 if (aux > 0.5)
     y=y2;
     return;
 end
 if (aux == 0.5 &&  rem(t-earro,2)==1)
     y=y1;
     return;
 end
 if (aux == 0.5 &&  rem(t-e,2)==0)
     y=y2;
     return;
 end
return
%% Exemples, format short (punt fix)
% arrodonir(124.036,5)
% arrodonir(124.031,5)
% arrodonir(-0.00656,2)
% arrodonir(-0.00651,2)
% arrodonir(0.00003456798765,5)
% arrodonirr(1.00003456798765,5)