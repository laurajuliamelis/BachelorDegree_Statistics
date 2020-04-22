/* PRÀCTICA 3
   Laura Julià Melis
   14.03.2018 */

LIBNAME d 'E:\ECONOMETRIA\Pràctiques\Pràctica_3';

* 1. LECTURA DE LA INFORMACIÓ;
OPTIONS MISSING='·';

Data DATA; 
infile 'E:\ECONOMETRIA\Pràctiques\Pràctica_3\93cars.txt' delimiter= '09'x FIRSTOBS=2;
input Manufacturer$	Model$	Type$ Minprice Midrangeprice Maxprice CityMPG	HighwayMPG	Airbags	Drivetraintype	Cylinders Enginesize Horsepower	RPM	EngineRmile	Manual	Fueltank	Passengers	Length	Wheelbase	Width	Uturn	Rearseat Luggage Weight Domestic;
keep midrangeprice horsepower rearseat airbags manual domestic type;
RUN;



* 2. ESTIMACIÓ PER MQO DE DIFERENTS ESPECIFICACIONS;
* a)	Midrangeprice=B1+ B2*Horsepower + B3*Rearseat + B4*Airbags + U;

PROC REG DATA = DATA;
MODEL Midrangeprice= Horsepower Rearseat Airbags;
RUN;

/* COMENTARIS:
Les variables Airbags i Housepower tenen associat un p-valor inferior a 0.05, són significatives. Per tant, aquestes
variables són les variables rellevants per explicar la variable dependent (preu mig del cotxe).

Si mirem els signes de les estimacóns del paràmetres, veiem que (llevat de l'intercept) tots són positius. Això
significa que un increment en cada una de les variables, causaria un increment en la variable 'Midrangeprice'. Així
doncs, un augment en la potència (CV), en l'espai dels seients posteriors i en el nombre d'airgabs, provocaria un
increment del preu mig del cotxe.*/



* b)	Domestic, Manual i Domestic*Manual;
Data DATA;
set DATA;
var1 = Domestic*Manual;
label var1 = 'Domestic*Manual';
RUN; 

* Model amb variable DOMESTIC;
PROC REG DATA = DATA;
MODEL Midrangeprice= Horsepower Rearseat Airbags Domestic;
RUN;
/* S'observa una millora en la bondat de l'ajust. A més, el fet d'afegir la variable Domestic ha fet que la variable Rearseat sigui significativa.*/


* Model amb variable MANUAL;
PROC REG DATA = DATA;
MODEL Midrangeprice= Horsepower Rearseat Airbags Manual;
RUN;
/*La bondat de l'ajust en aquest cas, tot i que hem afegit una nova variable, no és sificient. L'ajust no ha millorat.*/

* Model amb DOMESTIC, MANUAL i la interacció d'ambdues;
PROC REG DATA = DATA;
MODEL Midrangeprice= Horsepower Rearseat Airbags Domestic Manual var1;
RUN;
/*Tots els paràmetres són significatius (estadísticament diferents a 0). El model ha millorat molt afegint les dues variables ja que estem considerant
les interseccions. */


* c)	Horsepower*Domestic;
Data DATA;
set DATA;
Var2 = Horsepower*Domestic;
label var2 = 'Horsepower*Domestic';
RUN; 

* Afegim les fictícies de manera multiplicativa;
PROC REG DATA = DATA;
MODEL Midrangeprice= Horsepower Rearseat Airbags var2;
RUN;

* Especificació mixta;
PROC REG DATA = DATA;
MODEL Midrangeprice= Horsepower Rearseat Airbags Domestic var2;
RUN;
/* Estudiem si el pendent de la recta es modifica segons el fet de que es tracti d'un cotxe americà o no.
Veiem que totes les variables són rellevants. L'efecte de la potència sobre el preu esperat del vehicle no és el mateix si es tracta d'un cotxe americà o no*/


* d)	Type;

Data DATA;
set DATA;
if Type ='Small' then Small = 1;
else Small=0;
if Type ='Midsize' then Midsize = 1;
else Midsize=0;
if Type ='Large' then Large = 1;
else Large=0;
if Type ='Compact' then Compact = 1;
else Compact=0;
if Type ='Sporty' then Sporty = 1;
else Sporty=0;
if Type ='Van' then Van = 1;
else Van=0;
RUN; 

PROC REG DATA = DATA;
MODEL Midrangeprice= Small MidSize Large Compact Sporty Van;
RUN;
