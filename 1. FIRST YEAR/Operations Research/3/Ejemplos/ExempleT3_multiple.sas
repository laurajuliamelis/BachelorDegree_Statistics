libname t3 '.';
data t3.exem_multiple;
 input _row_ $4. x1 x2 _type_ $ _rhs_;
      datalines;
z       6 10   MAX    .
res1    5  2    LE   10
res2    3  5    LE   15
;
run;
proc lp data=t3.exem_multiple tableauotu=t3.taula_mult;
run;
proc print data=t3.taula_mult;
run;
