%%aproximando con F2, apartado II ejercicio 2for k=1:1:15h2=10^(-k);df2(k)=((atan((sqrt(5)+h2)/5))-atan((sqrt(5)-h2)/5)) / (2*h2) ;enda=1/6*ones(1,15); b=df2;      errorabs2= abs(a-b)T2= [1:15 ; df2 ; errorabs2]'