libname t3 '.';
data t3.exem_f_c;
 input _row_ $9. m1 m2 m3 b1 b2 b3 _type_ $ _rhs_;
     datalines;
cost      50 83 130 61 97 145 MIN .
demanda1   1  0   0  1  0   0 EQ  3000
demanda2   0  1   0  0  1   0 EQ  2000
demanda3   0  0   1  0  0   1 EQ   900
cablejat   2 1.5  3  0  0   0 LE 10000
trenat     1 2    1  0  0   0 LE  5000
;
run;

proc print data=t3.exem_f_c;
run;

proc lp data=t3.exem_f_c tableauotu=t3.taula_opt RANGERHS RANGEPRICE;
run;

proc print data=t3.taula_opt;
run;

*SPARSE DATA;
data t3.exem_f_c_sparse;
 input _type_ $5. _col_ $5. _row_ $11. _coef_;
     datalines;
min   .      cost           .
eq    .      demanda1       .
eq    .      demanda2       .
eq    .      demanda3       .
le    .      cablejat       .
le    .      trenat         .
.     m1     cost          50
.     m1     demanda1       1
.     m1     demanda2       0
.     m1     demanda3       0
.     m1     cablejat       2
.     m1     trenat         1
.     m2     cost          83
.     m2     demanda1       0
.     m2     demanda2       1
.     m2     demanda3       0
.     m2     cablejat       1.5
.     m2     trenat         2
.     m3     cost          130
.     m3     demanda1       0
.     m3     demanda2       0
.     m3     demanda3       1
.     m3     cablejat       3
.     m3     trenat         1
.     b1     cost          61
.     b1     demanda1       1
.     b1     demanda2       0
.     b1     demanda3       0
.     b1     cablejat       0
.     b1     trenat         0
.     b2     cost          97
.     b2     demanda1       0
.     b2     demanda2       1
.     b2     demanda3       0
.     b2     cablejat       0
.     b2     trenat         0
.     b3     cost          145
.     b3     demanda1       0
.     b3     demanda2       0
.     b3     demanda3       1
.     b3     cablejat       0
.     b3     trenat         0
.    _rhs_   cost           .
.    _rhs_   demanda1       3000
.    _rhs_   demanda2       2000
.    _rhs_   demanda3       900
.    _rhs_   cablejat       10000
.    _rhs_   trenat         5000
;
run;
proc lp data=t3.exem_f_c_sparse sparsedata;
run;

data t3.exem_f_c;
 input _row_ $9. m1 m2 m3 b1 b2 b3 _type_ $ _rhs_ _rhssen_;
     datalines;
cost      50 83 130 61 97 145 MIN .
demanda1   1  0   0  1  0   0 EQ  3000  .
demanda2   0  1   0  0  1   0 EQ  2000  .
demanda3   0  0   1  0  0   1 EQ   900  .
cablejat   2 1.5  3  0  0   0 LE 10000  .
trenat     1 2    1  0  0   0 LE  5000 10
;
run;

proc print data=t3.exem_f_c;
run;
proc lp data=t3.exem_f_c;
run;


*SPARSE DATA;
data t3.exem_f_c_sparse;
 input _type_ $9. _col_ $5. _row_ $12. _coef_;
     datalines;
min       .      cost           .
eq        .      demanda1       .
eq        .      demanda2       .
eq        .      demanda3       .
le        .      cablejat       .
le        .      trenat         .
.         m1     cost          50
.         m1     demanda1       1
.         m1     demanda2       0
.         m1     demanda3       0
.         m1     cablejat       2
.         m1     trenat         1
.         m2     cost          83
.         m2     demanda1       0
.         m2     demanda2       1
.         m2     demanda3       0
.         m2     cablejat       1.5
.         m2     trenat         2
.         m3     cost          130
.         m3     demanda1       0
.         m3     demanda2       0
.         m3     demanda3       1
.         m3     cablejat       3
.         m3     trenat         1
.         b1     cost          61
.         b1     demanda1       1
.         b1     demanda2       0
.         b1     demanda3       0
.         b1     cablejat       0
.         b1     trenat         0
.         b2     cost          97
.         b2     demanda1       0
.         b2     demanda2       1
.         b2     demanda3       0
.         b2     cablejat       0
.         b2     trenat         0
.         b3     cost          145
.         b3     demanda1       0
.         b3     demanda2       0
.         b3     demanda3       1
.         b3     cablejat       0
.         b3     trenat         0
.        _rhs_   cost           .
.        _rhs_   demanda1       3000
.        _rhs_   demanda2       2000
.        _rhs_   demanda3       900
.        _rhs_   cablejat       10000
.        _rhs_   trenat         5000
pricesen  b1     change         5
;
run;
proc lp data=t3.exem_f_c_sparse sparsedata;
run;
