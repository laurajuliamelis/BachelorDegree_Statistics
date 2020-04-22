/* PRÀCTICA 1
   Laura Julià Melis
   21.02.2018 */

*1. LECTURA DE LA INFORMACIÓ;
LIBNAME d 'E:\ECONOMETRIA\Pràctiques\Pràctica_1';

* Lectura de l'arxiu de dades en format XLS;
proc import out= d.mydata 
 datafile= 'E:\ECONOMETRIA\Pràctiques\Pràctica_1\gpa1.xls'
dbms=XLS replace;
getnames=yes;
run;

*Selecció de variables dsprés de la lectura de l'Excel:;
Data d.mydata;
SET d.mydata;
KEEP colGPA hsGPA ACT SKIPPED;
RUN;

* Lectura de l arxiu de dades en format ASCII
DATA mydata
INFILE "E:\ECONOMETRIA\Pràctiques\Pràctica_1\gpa1.txt" FIRSTOBS=2 DELIMITER ='09'x
INPUT colGPA soph junior senior	senior5	male campus	business engineer age hsGPA	ACT	job19 job20 drive bike walk voluntr	PC greek car siblings bgfriend clubs skipped alcohol gradMI	fathcoll mothcoll
KEEP colGPA hsGPA ACT
run;

*2. DESCRIPTIU UNIVARIANT DE LES VARIABLES;
PROC MEANS DATA = d.mydata mean std var min q1 median q3 max;
 VAR colGPA hsGPA ACT SKIPPED;
RUN;


*2a. Gràfic de dispersió.;
PROC GPLOT DATA = d.mydata;
	PLOT colGPA*(hsGPA ACT SKIPPED);
RUN;
QUIT;

* Arguments per a gràfics;
SYMBOL1 VALUE=dot Color=red HEIGHT=2;
AXIS1 label=('Nota mitjana en institut');
AXIS2 label=(angle=90 'Nota mitjana en la universitat');
TITLE Height=2 'Rendiment acadèmic a la universitat';

PROC GPLOT DATA = d.mydata;
	PLOT colGPA*hsGPA / haxis = axis1 vaxis=axis2;
RUN;
QUIT;

* Eliminar totes les opcions gràfiques indicades amb anterioritat;
GOPTIONS RESET = all;

*3. ESPECIFICACIÓ DEL MODEL;
* Veiem que hi ha una certa relació, una pendent positiva en els núvuls dels gràfics de dispersió.

* colGPA = B0 + B1*hsGPA + B2*ACT + B3*SKIPPED + U;
* Best_MQO = (X'X)-1*(X'Y);
* e=Y-Yest=Y-XBest;
* varest^2=e'e/n-k;

**** MATRIUS EN SAS ****;
PROC IML;
A={1 2 3, 4 5 16, 7 8 9};
print A;

b= J(3,3,2); * J(nre de files, nre columndes, nre que vols que ompli la matriu  ;
print b;

c=A*B;   * Producte matricial;
print c;

d=A#B;	 * Producte element a element;
print d;

invA = inv(A); * Per invertir matrius;
at=t(A); * Trasposta de A;
bt=b`;  * Trasposta de A d'una altra forma (accent obert);
print invA, at, bt;

deta= det(A); * Determinant de A;
diagA=diag(A); * Matriu amb la diagonal i la resta 0;
vdiag=vecdiag(A); * Vector amb la diagonal;
print detA, diagA, vdiag;

AB= A||B; * Per concatenar les matrius una a la dreta de l'altra;
hAB= A//B; * Per concatenar les matrius una sobre de l'altra;
print AB, hAB;

subA23=A[2,3]; * Element de la matriu A de la fila 2, columna 3;
Arow2=A[2,]; * Tots els elements de la fila 2 de la matriu A;
Acol3= A[,3]; * Tots els elements de la columna 3 de la matriu A;
print subA23,Arow2, Acol3;

sumcol=A[,+]; * Mantindrà totes les files i sumarà els valors per columnes;
sumrow=A[+,]; * Mantindrà totes les columnes i sumarà els valors per files;
sumA= A[+,+]; * Suma TOT;

meancol=A[,:]; * Fa la mitjana per columnes;
meanrow=A[:,]; * Mitjanes per files;

prodcol=A[,#]; * Fa el producte per columnes;
prodrow=A[#,]; * Fa el producte per files;
QUIT; 


*En el nostre cas, usant la base da dades;
PROC IML;
use d.mydata; * obrim la base de dades anteriors i fixxem les variables;
read all var {hsGPA ACT SKIPPED} into X0;
read all var {colGPA} into Y;

print X0;
print Y; 

n=nrow(Y); * Nombre d'observacions;
X=J(n,1,1)||X0; * Creem un vector (matriu nx1)de uns i el concatenem horitzontalment a la matriu X0;
k=ncol(X); * Nombre de paràmetres;

print n,k,X;

BMQO=inv(t(X)*X)*(t(X)*Y);
e= Y - (X*BMQO);
VAR = (t(e)*e)/(n-k);
print BMQO, e, VAR; 

QUIT; 


*4. ESTIMACIÓ PER MÍNIMS QUADRATS ORDINARIS;

*5. CÀLCUL DE LA VARIÀNCIA DEL TERME DE PERTORBACIÓ I DE LA VARIÀNCIA DE L'ESTIMADOR;

*6. SIGNIFICACIÓ DELS PARÀMETRES I BONDAT D'AJUST;

*7. INTERPRETACIÓ I VALORACIÓ DELS RESULTATS;
