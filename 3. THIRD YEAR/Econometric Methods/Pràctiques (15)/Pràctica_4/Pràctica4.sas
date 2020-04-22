/* PR�CTICA 4
   Laura Juli� Melis
   21.03.2018 */

LIBNAME d 'E:\ECONOMETRIA\Pr�ctiques\Pr�ctica_4';

* LECTURA DE LA INFORMACI�;
proc import out= d.mydata 
 datafile= 'E:\ECONOMETRIA\Pr�ctiques\Pr�ctica_4\auto1.xlsx'
dbms=XLSX replace;
getnames=yes;
RUN;

* 1. Representaci� gr�fica de la variable PRICE respecte el WEIGHT, i tamb� considerant el pa�s de fabricaci�;

* Gr�fica de l'ajust lineal;
GOPTIONS RESET=all;

symbol1 value=dot height=1.5 interpol=RL cv=red ci=blue width=5;
axis1 label=('Pes del cotxe');
axis2 label=(angle=90 'Preu del cotxe');
title 'AJUST LINEAL';

PROC GPLOT DATA= d.mydata;
plot price*weight / haxis=axis1 vaxis=axis2 REGEQN;
RUN;
QUIT;

* El que ens demanen;
GOPTIONS RESET=all;

symbol1 value=dot color=red width=2;
symbol2 value=dot color=blue width=2;
axis1 label=('Pes del cotxe');
axis2 label=(angle=90 'Preu del cotxe');
title 'Gr�fic de PRICE respecte WEIGHT considerant FOREIGN';

PROC GPLOT DATA= d.mydata;
plot price*weight=foreign / haxis=axis1 vaxis=axis2;
RUN;
QUIT;


* 2. Especificaci� i estimaci� del model "PRICE =  B1+ B2*WEIGHT + U";

PROC REG DATA=d.mydata;
MODEL Price = weight;
RUN;

* 3. Contrast RESET (Volem veure si la relaci� �s o no lineal);
* 3.1. Guardem els resultats de l'estimaci� del model per totes les observacions;
PROC REG DATA=d.mydata;
MODEL Price = weight;
OUTPUT OUT=mydataest P=Yfit R=resid;
RUN;

* 3.2. Estandaritzem Yfit per poder realitzar les regressions auxiliars per tal d'evitar problemes de autocorrelaci� i de dimensions massa grans;
PROC standard data=mydataest out=std mean=0 std=1;
var Yfit; 
RUN;

DATA mydataest;
merge mydataest (rename=(Yfit=Yajus)) std;
	Yfit2= Yfit**2;
	Yfit3= Yfit**3;
	Yfit4= Yfit**4;
RUN;

PROC reg  data= mydataest;
model price= weight Yfit2;
RUN; 
* Nom�s ens interessa mirar la significaci� del par�metre (Taula "Parameter Estimates");

PROC reg  data= mydataest;
model price= weight Yfit2 Yfit3;
TEST Yfit2=0, Yfit3=0;
RUN; 
* Volem validar (o no) la Ho de que tots els par�metres que acompanyen les var auxiliars son iguals a 0;

PROC reg  data= mydataest;
model price= weight Yfit2 Yfit3 Yfit4;
TEST Yfit2=0, Yfit3=0, Yfit4=0;
RUN; 
* En els dos �ltims casos afirmem la no linealitat (observar p-valor <0.05);


* TEST RESET AMB PROC AUTOREG; 
PROC AUTOREG data= mydataest;
MODEL price=weight /RESET;
RUN;
* Ens torna la mateixa sortida que el PROC REG  del model no ajustat per�, a m�s, inclou un quadre amb els resultats del test de Ramsey;
* Interpretaci� test: Power 2,3 i 4 son el Yfit al quadrat, al cub i a la quarta. Surten tamb� els seus respectius p-valors i el valor de l'estad�stic. 
  Observar que l'Estad�stic de Yfit2 �s diferent a com ho ho hem fit abans, perqu� el valor t del PROC REG cal elevarlo al quadrat (3.49^2 =12.18) 

* 4. Especificar model quadr�tic i el model quadr�tic considerant la variable FOREIGN;

* Model "PRICE =  B1+ B2*WEIGHT + U" afegint la ficticia additiva i multiplicativa
  La ficticia ja la tenim creada, �s la variable FOREIGN. Necessitem crear la ficticia multiplicativa; 

TITLE1 "Model QUADR�TIC" height=2;
DATA d.mydata;
set d.mydata;
for_wei= foreign*weight;
weight2=weight**2; * Forma quadr�tica;
for_wei2= foreign*weight2; * Fict�cia multiplicativa per la forma quadr�tica;
RUN; 

PROC REG data = d.mydata;
MODEL price = weight foreign for_wei; 
MODEL price = weight weight2; *Forma quadr�tica sense distingir entre el foreig;
MODEL price = weight weight2 foreign for_wei2; * Inclou el weight i weight^2 per� tamb�;
	OUTPUT OUT=mydataestQ P=YfitQ;
RUN; 
* Interepretaci� resultats: 
  MODEL 1
  F gran, pvalor quasi 0, per tant estimaci� correcta. Veiem que l'ajust ha millorat molt: som capa�os d'explicar un 50% del model. Si mirem "for_wei", 
  veiem que l'estimaci� del par�metre �es 2.36, amb un estad�stic t de 2.11 i un p valor inferior a 0.05 (no seria significatiu al 1%). Per tant, diem que 
  estad�sticament �s diferent a 0 i que el pendent de la recta dep�n del pa�s de fabricaci� del cotxe. 

  MODEL 3
  Recull la no linealitat (forma quadr�tica) o, a m�s a m�s, distingeix entre cotxes americans i no americans. T� un ajust molt m�s b�. L'�nic par�metre
  que no seria significatiu al 1% seria el "foreig", per tant, tots els altres par�metres s�n, amb total seguretat, estad�sticament diferents a 0. 


* GR�FICAMENT;
* 1er. Ordenem les dades;

PROC SORT data=mydataestQ;
by weight;
RUN; 

* Ho feiem per unir els punts i veure la l�nia recte que formen;

* 2n. Creem variables PRICE1, PRICE2, YFITQ1 i YFITQ2, segons si foreign=1 o b� foreign=0;
DATA mydataestQ;
    set mydataestQ;
if foreign=1 then Yfitq1=Yfitq; else Yfitq1=.;
if foreign=1 then Price1=Price; else Price1=.; * Price1 contindr� els preus del cotxes que tenen foreign=1;
if foreign=0 then Yfitq2=Yfitq; else Yfitq2=.;
if foreign=0 then Price2=Price; else Price2=.;
RUN; 

* 3er. Gr�fic de la variable PRICE vs PES amb ajust quadr�tic;
GOPTIONS RESET=all;

symbol1 value=dot height=1.5 cv=blue width=5;
symbol2 value=dot height=1.5 cv=red width=5;
symbol3 value=dot height=0 interpol=join cv=blue ci=blue width=5; * No volem que ens dibuixi els punts, sin� que els uneixi -> interpol=join;
symbol4 value=dot height=0 interpol=join cv=red ci=red width=5;
axis1 label=('Pes del cotxe');
axis2 label=(angle=90 'Preu del cotxe');
title 'Ajust lineal i quadr�tic';

PROC GPLOT DATA= mydataestQ;
plot (price1 price2 Yfitq1 Yfitq2)*weight / haxis=axis1 vaxis=axis2 OVERLAY; * Amb overlay no tendrem 4 gr�fics separats sino que estaran superposats;
RUN;
QUIT;


* 5. Especificar model lineal-log, log-lineal i  log-log, considerant tamb� la variable FOREIGN;
* ESTIMACI� DEL MODEL; 
TITLE1 "Model LOG-Lineal" height=2;
Data d.mydata;
 set d.mydata;
 lprice=log(price);
 for_wei=foreign*weight;
RUN;

PROC REG DATA=d.mydata;
MODEL lprice = weight;
MODEL lprice = weight for_wei;
  OUTPUT OUT=mydataLoLi P=YfitLoLi;
RUN; 
* NO podem comparar el R^2 ajustat amb els altres models perqu� aqui la variable dependent no es price, sin� el logaritme de price. 
  Observam estadis�stics molt grans i p-valors molt propers a 0. El model representa b� l'estimaci� del preu aix� com l'efecta sobre el preu
  que te la variable FOREIGN;

* GR�FICAMENT; 
PROC SORT data= mydataLoLi;
 by foreign weight;
 RUN;

 DATA mydataloli;
    set mydataloli;
if foreign=1 then Yfit1=Yfitloli; else Yfit1=.;
if foreign=1 then lprice1=lprice; else lprice1=.; 
if foreign=0 then Yfit2=Yfitloli; else Yfit2=.;
if foreign=0 then lprice2=lprice; else lprice2=.;
RUN; 


GOPTIONS RESET=all;

symbol1 value=dot height=1.5 cv=blue width=5;
symbol2 value=dot height=1.5 cv=red width=5;
symbol3 value=dot height=0 interpol=join cv=gray ci=blue width=5; * No volem que ens dibuixi els punts, sin� que els uneixi -> interpol=join;
symbol4 value=dot height=0 interpol=join cv=gray ci=red width=5;
axis1 label=('Pes del cotxe');
axis2 label=(angle=90 'Logaritme Preu del cotxe');
title 'Ajust LOG-lineal';

PROC GPLOT DATA= mydataloli;
plot (lprice1 lprice2 Yfit1 Yfit2)*weight / haxis=axis1 vaxis=axis2 OVERLAY; * Amb overlay no tendrem 4 gr�fics separats sino que estaran superposats;
RUN;
QUIT;

* Interpretaci� del model Log_Lineal: multipliquem el valor de l'estimaci� del par�metre per 100 i ho interpretem com un percenttge. 
  Interpretaci� del model Lineal_Log: dividirem el valor de l'estimaci� del parametre entre 1000 i diem: 1 lliure m�s de pes far� augmentar (X/100) d�llars
  el preu del cotxe. 
