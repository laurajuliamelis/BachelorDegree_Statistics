function y=tallar(x,t)
%% Tallem el nombre donat a t xifres en coma flotant
if (x==0),
    y=0;
    return;
end
e=fix(log10(abs(x)));
if e>0,    %e>0??
   e=e+1;
end;
y=fix(x*10^(t-e))*10^(e-t);
return
%% Exemples, format short (punt fix)
% tallar(124.036,5)
% tallar(124.031,5)
% tallar(-0.00656,2)
% tallar(-0.00651,2)
% tallar(0.00003456798765,5)
% tallar(1.00003456798765,5)
% tallar(pi,5)