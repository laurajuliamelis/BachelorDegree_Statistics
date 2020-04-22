libname t3 '.';
data t3.exem_nosol;
 input _row_ $4. x1 x2 _type_ $ _rhs_;
      datalines;
z       5 -3   MAX    .
res1    2  1    LE   1
res2    4  2    GE   6
;
run;
proc lp data=t3.exem_nosol tableauout=t3.taula_mult noprint;
run;

proc print data=t3.taula_mult;
run;
