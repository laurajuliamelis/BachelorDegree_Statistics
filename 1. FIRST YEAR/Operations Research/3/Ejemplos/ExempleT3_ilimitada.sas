libname t3 '.';
data t3.exem_barreja;
 input _row_ $ I II III _type_ $ _rhs_;
      datalines;
cost   40  30 10  MIN     .
A       4   2  4  GE    100
B       5   4  2  GE    100
Pes     3   1  3  LE    200
;
run;

proc print data=t3.exem_barreja;
run;
proc lp data=t3.exem_barreja tableauout=t3.taula_opt noprint;
run;
proc print data=t3.taula_opt;
run;

proc lp data=t3.exem_barreja rangeprice;
run;
