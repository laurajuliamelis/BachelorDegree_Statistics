* LAURA JULIÀ MELIS - 7.03.2018
* Econometria - Pràctica 2;
libname d "E:\ECONOMETRIA\Pràctiques\Pràctica_2";

PROC IMPORT OUT = WORK.MYDATA
			datafile= "E:\ECONOMETRIA\Pràctiques\Pràctica_2\gpa1.xls"
			dbms = xls;
			GETNAMES = YES;
RUN;


* 1. ANAMLITZEM LA SURTIDA D'html;
PROC REG DATA = mydata;
MODEL colGPA= hsGPA ACT SKIPPED; *Var_dep (y) = var_explicatives (x) // no cal escriure el temre independent;
RUN;

* Contrast d'hipòtesis -> H0: beta1=beta2=beta3=0, H1: existeix almenys un betaj !=0
* ROOT MSE: arrel de l'error quadratic mig:  sqrt(SQE/N-k)=sigma_estimada
* f Value in Analysis of Variance -> estimació conjunta
* Pr > |t| in Parameter Estimates -> Per als valos menors a 0.05, rebutjem la Ho i diem que el paràmetre és estadisticament igual a 0, per al superior a 0.05
  diem que no es estadisticament igual a 0 i per tant, la variable no és rellevant per wexplicar la variable dependent (notes universitat)
* Mirem els signes-> positus: un increment en aqulles variables(x) provocaria un increment en la var dependent(y) i igual a l'inrevés. Exemple: el signe negatiu
  a l'SKIPPED significa que a més classes que et saltes pitjor nota obtens (increment de SKIPPED, disminució en colGPA-nota);



* 2. ESTANDARITZEM ELS COEFICIENTS BETA DEL MODEL;
PROC REG DATA = mydata;
MODEL colGPA= hsGPA ACT SKIPPED /stb;
RUN;
* Així, en 'Parameter Estimates' obtenim la columna 'Standardized Estimate', amb els coeficients estandaritzats i, d'aquesta manera, ja els podem comparar;



* 3. GUARDEM EN UNA BBDD LES VARIABLES DEL MODEL, ELS RESIDUS Y LA Y AJUSTADA (y_barret);
PROC REG DATA = mydata;
MODEL colGPA= hsGPA ACT SKIPPED;
OUTPUT OUT=mydataset P=Yfit R=resid;
RUN;

PROC PRINT data=mydataset;
RUN;
* En la pantalla output no hi ha res nou. La única cosa que ha canviat és que en la llibreria WORK m'ha creat la BBDD amb les vars, la Yfit i els residus;



* 4. GUARDEM ELS RESULTATS DE L'ESTIMACIÓ EN UNA ALTRA BBDD;
PROC REG DATA = mydata OUTEST=results;
MODEL colGPA= hsGPA ACT SKIPPED / STB OUTSEB; * Opció per indicar que volem q guardi els resultats en la BBDD indicada anteriorment (results);
RUN;

PROC PRINT data=results;
RUN;

* 5. GRÀFIC DE LS VALORS DE LA VARIABLE DEPENDENT colGPA;
GOPTIONS RESET=all;

SYMBOL1 value=dot c=red height=1.5;
AXIS1 label=('Num classes perdudes per setmana');
AXIS2 label=(angle=90 'Nota mitjana a la universitat');
AXIS3 label=('Nota mitjana en insitut');
TITLE1 H=2 'Rendiment acadèmic a la Universitat';
PROC GPLOT DATA=mydata;
PLOT colGPA*skipped / haxis=axis1 vaxis=axis2;
PLOT colGPA*hsGPA / haxis=axis3 vaxis=axis2;
RUN;

QUIT;

* 6. PREDICCIÓ PER UNA NOVA OBSERVACIÓ INDIVIDUAL:	(hsGPA=3, ACT=24, SKIPPED=2);

* 6.1 A mà:
* El nostre model és: Y= B1 + B2*hsGPA + B3*ACT + B4*SKIPPED
  Els valors estimats per a cada paràmetre son:
	- Intercept = 1.38955
	- hsGPA = 0.41182
	- ACT = 0.01472
	- SKIPPED = 0.01472

 Així doncs, em pregunten: Y_barret= 1.38955 + 0.41182*3 + 0.01472*24 + 0.01472*2 = 2.81207;

* 6.2 Amb SAS:
* 	a. Creem una nova BBDD per les observacions a predir; 
DATA mydata1;
input hsGPA ACT skipped;
cards;
3 24 2
;
RUN;

*	b. Afegim a l'arxiu original les noves observacions;
DATA mydata2;
set mydata mydata1;
RUN;
* Afageix files i deixa MISSINGS a totes aquelles variables de les que no te valors. 

* c. Estimació MQO amb les noves dades;
PROC REG DATA = mydata2;
MODEL colGPA=hsGPA ACT SKIPPED / p clm cli;
RUN;



* 7. FEIEM CONTRASTOS AMB RESTRICCIONS LINEALS SOBRE ELS PARÀMETRES;
* Contrast -> H0: Beta2 + Beta4 =0.3;
PROC REG DATA = mydata;
MODEL colGPA=hsGPA ACT SKIPPED;
TEST hsGPA+SKIPPED=0.3 / print; * Hem dona el resultat del test; 
RUN;
* Ens apareix al final l'estadistic de contrast F i el seu pvalor. Com és major que 0.05, no podem rebutjar Ho i diem que la restricció és certa;

PROC REG DATA = mydata;
MODEL colGPA=hsGPA ACT SKIPPED;
RESTRICT hsGPA+SKIPPED=0.3; * M'estima el model restringit, les matrius R etc etc; 
RUN;


* Si tenim més d'una restricció, cal separar-ho amb comes;
PROC REG DATA = mydata;
MODEL colGPA=hsGPA ACT SKIPPED;
RESTRICT hsGPA+SKIPPED=0.3, act=0.05; * Tant per TEST com per RESTRICT;
RUN;
