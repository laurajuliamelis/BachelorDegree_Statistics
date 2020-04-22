proc optmodel;

set <num> ARTIC = 1..9;
set <num> PAQ = 1..4;

number Valor {ARTIC} = [56, 71, 69, 91, 70, 85, 65, 80, 75];
number Peso {ARTIC} = [7, 11, 4, 14, 9, 2, 12, 8, 13];
number Volum {ARTIC} = [21, 16, 17, 28, 12, 31, 19, 24, 29];
number PesoMax = 36;
number VolMax = 90;
number FixCst = 3;
number LinCst = 2;

/* variables decisión parte 1 */
var X {ARTIC} binary;               /* artic j se incluye en el paquete */

/* variables decisión parte 2
var X {PAQ, ARTIC} binary;   /* artic j aparece en paquete i * /
var p {PAQ};                 /* peso total del paquete * /
var q {PAQ};                 /* precio del paquete (prescindible facilmente) * /
var r {PAQ} binary;          /* si el paquete se despacha (no vacío) * /
*/

max mochila = sum {j in ARTIC} Valor[j]*X[j];

con weight : sum {j in ARTIC} Peso[j]*X[j] <= PesoMax;

con volumen : sum {j in ARTIC} Volum[j]*X[j] <= VolMax;

solve;

print X weight.body volumen.body;
