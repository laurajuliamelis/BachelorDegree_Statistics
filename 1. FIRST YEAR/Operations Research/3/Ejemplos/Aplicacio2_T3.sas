libname t3 '.';
data t3.exem_inv;
 input _row_ $12. x1 x2 x3 x4 x5 x6 _type_ $ _rhs_;
     datalines;
rentabilitat   0.0865 0.0950 0.1000 0.0875 0.0925 0.0900 MAX .
inversio1      1      0      0      0      0      0      LE  187500
inversio2      0      1      0      0      0      0      LE  187500
inversio3      0      0      1      0      0      0      LE  187500
inversio4      0      0      0      1      0      0      LE  187500
inversio5      0      0      0      0      1      0      LE  187500
inversio6      0      0      0      0      0      1      LE  187500
total          1      1      1      1      1      1      EQ  750000
largtermini3   1      1      0      1      0      1      GE  375000
risc           0      1      1      0      1      0      LE  262500
;
run;

proc print data=t3.exem_inv;
run;

proc lp data=t3.exem_inv tableauout=t3.taula_opt RANGERHS RANGEPRICE;
run;

proc print data=t3.taula_opt;
run;
