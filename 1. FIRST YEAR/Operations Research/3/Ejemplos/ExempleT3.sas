libname t3 '.';
data t3.exemBRHT;
 input _row_ $9. Aqua Hydro _type_ $ _rhs_;
      datalines;
benefici   350  300   MAX     .
bombes       1    1   LE    200
treball      9    6   LE   1566
canonades   12   16   LE   2880
;
run;

proc print data=t3.exemBRHT;
run;
proc lp data=t3.exemBRHT tableauotu=t3.taula_opt;
run;
proc print data=t3.taula_opt;
run;


proc lp data=t3.exemBRHT rangerhs;
run;
