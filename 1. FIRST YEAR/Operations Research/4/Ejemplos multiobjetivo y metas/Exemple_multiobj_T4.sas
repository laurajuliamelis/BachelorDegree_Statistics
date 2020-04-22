libname mult '.';
data mult.exemple_multobj;
      input _row_ $19. White Giles _type_ $ _rhs_;
      datalines;
Coste de producció   40   32   MIN     1
Aigües tòxiques     800 1250   MIN     2
Accidents mortals  0.20 0.45   MIN     3
Alta graduació       12    4   GE     48
Mitjana graduació     4    4   GE     28
Baixa graduació      10   20   GE    100
;
run;
proc print  data=mult.exemple_multobj;
run;
proc lp data=mult.exemple_multobj goalprogram; 
run;
