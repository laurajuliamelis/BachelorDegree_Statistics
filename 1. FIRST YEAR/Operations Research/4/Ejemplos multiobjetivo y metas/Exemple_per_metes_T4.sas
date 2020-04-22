libname t4 '.';
data t4.metes_pa;
 input _row_ $12. x1     x2 d1me  d1ma  d2me  d3me  d4me    _type_ $ _rhs_;
     datalines;
Objectius         0     0 0.0125 0.0125 0.01 0.0833 0.003   MIN       .
Horas           0.33   0.5   1     -1    0     0     0       EQ      80
Pedido1          1      0    0      0    1     0     0       EQ     100
Pedido2          0      1    0      0    0     1     0       EQ     120
M.Prima          1      1    0      0    0     0     1       EQ     300
;
run;

proc print data=t4.metes_pa;
run;

proc lp data=t4.metes_pa;
run;

