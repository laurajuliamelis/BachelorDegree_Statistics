/* PRÀCTICA 4
   Laura Julià Melis
   21.03.2018 */

LIBNAME d 'E:\ECONOMETRIA\Pràctiques\Pràctica_4';

* LECTURA DE LA INFORMACIÓ;
proc import out= d.mydata 
 datafile= 'E:\ECONOMETRIA\Pràctiques\Pràctica_4\auto1.xlsx'
dbms=XLSX replace;
getnames=yes;
RUN;

* 1. Representació gràfica de la variable PRICE respecte el WEIGHT, i també considerant el país de fabricació;

* Gràfica de l'ajust lineal;
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
title 'Gràfic de PRICE respecte WEIGHT considerant FOREIGN';

PROC GPLOT DATA= d.mydata;
plot price*weight=foreign / haxis=axis1 vaxis=axis2;
RUN;
QUIT;


* 2. Especificació i estimació del model "PRICE =  B1+ B2*WEIGHT + U";

PROC REG DATA=d.mydata;
MODEL Price = weight;
RUN;

* 3. Contrast RESET (Volem veure si la relació és o no lineal);
* 3.1. Guardem els resultats de l'estimació del model per totes les observacions;
PROC REG DATA=d.mydata;
MODEL Price = weight;
OUTPUT OUT=mydataest P=Yfit R=resid;
RUN;

* 3.2. Estandaritzem Yfit per poder realitzar les regressions auxiliars per tal d'evitar problemes de autocorrelació i de dimensions massa grans;
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
* Només ens interessa mirar la significació del paràmetre (Taula "Parameter Estimates");

PROC reg  data= mydataest;
model price= weight Yfit2 Yfit3;
TEST Yfit2=0, Yfit3=0;
RUN; 
* Volem validar (o no) la Ho de que tots els paràmetres que acompanyen les var auxiliars son iguals a 0;

PROC reg  data= mydataest;
model price= weight Yfit2 Yfit3 Yfit4;
TEST Yfit2=0, Yfit3=0, Yfit4=0;
RUN; 
* En els dos últims casos afirmem la no linealitat (observar p-valor <0.05);


* TEST RESET AMB PROC AUTOREG; 
PROC AUTOREG data= mydataest;
MODEL price=weight /RESET;
RUN;
* Ens torna la mateixa sortida que el PROC REG  del model no ajustat però, a més, inclou un quadre amb els resultats del test de Ramsey;
* Interpretació test: Power 2,3 i 4 son el Yfit al quadrat, al cub i a la quarta. Surten també els seus respectius p-valors i el valor de l'estadístic. 
  Observar que l'Estadístic de Yfit2 és diferent a com ho ho hem fit abans, perquè el valor t del PROC REG cal elevarlo al quadrat (3.49^2 =12.18) 

* 4. Especificar model quadràtic i el model quadràtic considerant la variable FOREIGN;

* Model "PRICE =  B1+ B2*WEIGHT + U" afegint la ficticia additiva i multiplicativa
  La ficticia ja la tenim creada, és la variable FOREIGN. Necessitem crear la ficticia multiplicativa; 

TITLE1 "Model QUADRÀTIC" height=2;
DATA d.mydata;
set d.mydata;
for_wei= foreign*weight;
weight2=weight**2; * Forma quadràtica;
for_wei2= foreign*weight2; * Fictícia multiplicativa per la forma quadràtica;
RUN; 

PROC REG data = d.mydata;
MODEL price = weight foreign for_wei; 
MODEL price = weight weight2; *Forma quadràtica sense distingir entre el foreig;
MODEL price = weight weight2 foreign for_wei2; * Inclou el weight i weight^2 però també;
	OUTPUT OUT=mydataestQ P=YfitQ;
RUN; 
* Interepretació resultats: 
  MODEL 1
  F gran, pvalor quasi 0, per tant estimació correcta. Veiem que l'ajust ha millorat molt: som capaços d'explicar un 50% del model. Si mirem "for_wei", 
  veiem que l'estimació del paràmetre ñes 2.36, amb un estadístic t de 2.11 i un p valor inferior a 0.05 (no seria significatiu al 1%). Per tant, diem que 
  estadísticament és diferent a 0 i que el pendent de la recta depèn del país de fabricació del cotxe. 

  MODEL 3
  Recull la no linealitat (forma quadràtica) o, a més a més, distingeix entre cotxes americans i no americans. Té un ajust molt més bó. L'únic paràmetre
  que no seria significatiu al 1% seria el "foreig", per tant, tots els altres paràmetres són, amb total seguretat, estadísticament diferents a 0. 


* GRÀFICAMENT;
* 1er. Ordenem les dades;

PROC SORT data=mydataestQ;
by weight;
RUN; 

* Ho feiem per unir els punts i veure la línia recte que formen;

* 2n. Creem variables PRICE1, PRICE2, YFITQ1 i YFITQ2, segons si foreign=1 o bé foreign=0;
DATA mydataestQ;
    set mydataestQ;
if foreign=1 then Yfitq1=Yfitq; else Yfitq1=.;
if foreign=1 then Price1=Price; else Price1=.; * Price1 contindrà els preus del cotxes que tenen foreign=1;
if foreign=0 then Yfitq2=Yfitq; else Yfitq2=.;
if foreign=0 then Price2=Price; else Price2=.;
RUN; 

* 3er. Gràfic de la variable PRICE vs PES amb ajust quadràtic;
GOPTIONS RESET=all;

symbol1 value=dot height=1.5 cv=blue width=5;
symbol2 value=dot height=1.5 cv=red width=5;
symbol3 value=dot height=0 interpol=join cv=blue ci=blue width=5; * No volem que ens dibuixi els punts, sinó que els uneixi -> interpol=join;
symbol4 value=dot height=0 interpol=join cv=red ci=red width=5;
axis1 label=('Pes del cotxe');
axis2 label=(angle=90 'Preu del cotxe');
title 'Ajust lineal i quadràtic';

PROC GPLOT DATA= mydataestQ;
plot (price1 price2 Yfitq1 Yfitq2)*weight / haxis=axis1 vaxis=axis2 OVERLAY; * Amb overlay no tendrem 4 gràfics separats sino que estaran superposats;
RUN;
QUIT;


* 5. Especificar model lineal-log, log-lineal i  log-log, considerant també la variable FOREIGN;
* ESTIMACIÓ DEL MODEL; 
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
* NO podem comparar el R^2 ajustat amb els altres models perquè aqui la variable dependent no es price, sinó el logaritme de price. 
  Observam estadisístics molt grans i p-valors molt propers a 0. El model representa bé l'estimació del preu així com l'efecta sobre el preu
  que te la variable FOREIGN;

* GRÀFICAMENT; 
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
symbol3 value=dot height=0 interpol=join cv=gray ci=blue width=5; * No volem que ens dibuixi els punts, sinó que els uneixi -> interpol=join;
symbol4 value=dot height=0 interpol=join cv=gray ci=red width=5;
axis1 label=('Pes del cotxe');
axis2 label=(angle=90 'Logaritme Preu del cotxe');
title 'Ajust LOG-lineal';

PROC GPLOT DATA= mydataloli;
plot (lprice1 lprice2 Yfit1 Yfit2)*weight / haxis=axis1 vaxis=axis2 OVERLAY; * Amb overlay no tendrem 4 gràfics separats sino que estaran superposats;
RUN;
QUIT;

* Interpretació del model Log_Lineal: multipliquem el valor de l'estimació del paràmetre per 100 i ho interpretem com un percenttge. 
  Interpretació del model Lineal_Log: dividirem el valor de l'estimació del parametre entre 1000 i diem: 1 lliure més de pes farà augmentar (X/100) dóllars
  el preu del cotxe. 
