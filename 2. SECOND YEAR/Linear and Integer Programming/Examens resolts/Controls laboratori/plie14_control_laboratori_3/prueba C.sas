proc optmodel;

set <num> ARTIC = 1..9;
set <num> PAQ = 1..4;
set <num> NTOG = 1..3;
set <num> NSEP = 1..1;

number Valor {ARTIC} = [56, 71, 69, 91, 70, 85, 65, 80, 75];
number Peso {ARTIC} = [7, 11, 4, 14, 9, 2, 12, 8, 13];
number Volum {ARTIC} = [21, 16, 17, 28, 12, 31, 19, 24, 29];
number Together {1..2, NTOG} = 
 [ 1 6 8 
   2 7 9];
number Separated {1..2, NSEP} = 
 [ 3 
   5];
number PesoMax = 36;
number VolMax = 90;
number FixCst = 3;
number LinCst = 2;

/* variables decisión parte 1 
var X {ARTIC} binary;               /* artic j se incluye en el paquete */

/* variables decisión parte 2 */
var X {PAQ, ARTIC} binary;   /* artic j aparece en paquete i */
var p {PAQ} <= PesoMax;                 /* peso total del paquete */
var q {PAQ};                 /* precio del paquete (prescindible facilmente) */
var r {PAQ} binary;          /* si el paquete se despacha (no vacío) */


min Costetotal = sum {i in PAQ} q[i];

con pesopaq {i in PAQ}: p[i] = sum {j in ARTIC} Peso[j]*X[i,j];

con preciopaq {i in PAQ}: q[i] = FixCst*r[i] + LinCst*p[i];

con despacho {i in PAQ}: CARD(ARTIC)*r[i] >= sum {j in ARTIC} X[i,j];  /* si X>0, r=1 */

con despacho2 {i in PAQ}: r[i] <= sum {j in ARTIC} X[i,j]; /* si X=0, r=0 */

/* con weight {i in PAQ}: p[i] <= PesoMax; */

con volumen {i in PAQ}: sum {j in ARTIC} Volum[j]*X[i,j] <= VolMax;

con todos {j in ARTIC}: sum {i in PAQ} X[i,j] = 1;

con todos2 : sum {i in PAQ, j in ARTIC} X[i,j] = CARD(ARTIC);

con juntitos {k in NTOG, i in PAQ} : X[i,Together[1,k]] = X[i,Together[2,k]];

con separados {k in NSEP, i in PAQ} : X[i,Separated[1,k]] + X[i,Separated[2,k]] <= 1;

solve;

print X p q r;
