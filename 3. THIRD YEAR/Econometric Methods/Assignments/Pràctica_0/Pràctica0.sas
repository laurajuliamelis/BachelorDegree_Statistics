/* Laura Juli� Melis
   Classe 14.02.2018
   ECONOMETRIA: Pr�ctica 0 */

*--------------------------------------------------EXERCICI--------------------------------------------------*

* IMPORTACI� FITXER "Invest_Data.xls";
PROC IMPORT DATAFILE='E:\ECONOMETRIA\Pr�ctica_0\Invest_Data.xls'
			OUT=MEUDISC.nombas4
			DBMS=XLS REPLACE;
			GETNAMES=YES; 
RUN;

PROC PRINT DATA= MEUDISC.nombas4;
RUN;



*--------------------------------------------------CLASSE--------------------------------------------------*
* 1. BLOC DATA;

* Creem una base de dades;
DATA nombase;
INPUT VAR1 VAR2 VAR3$;
CARDS; 			
12 24 XX
33 42 YY
55 66 ZZ
;
RUN;

* 2. BLOC PROC;

* Si volem veure el contingut de la base de dades;
PROC PRINT data=nombase;
RUN;


* 3. GUARDAR DADES;
/* La base de dades "nombase.sas7bdat" creada des del BLOC DATA es guarda a la llibreria temporal WORK (Explorer-Libraries-Work).
Podem guardar la base de manera permanent creant nosaltres una llibreria amb la funci� LIBNAME*/

LIBNAME MEUDISC 'E:\ECONOMETRIA\Pr�ctica_0';

DATA MEUDISC.nombase;
INPUT VAR1 VAR2 VAR3$;
CARDS; 			
12 24 XX
33 42 YY
55 66 ZZ
;
RUN;

*Aix�, la base ja est� guardada a la meva llibreria.


* 4. LECTURA DELS VALORS;
*Si no separem els valors de la base de dades amb espais, caldr� indicar-ho a l'input;
DATA MEUDISC.nombase;
INPUT VAR1 1-2 VAR2 3-4 VAR3$ 5-6;
CARDS; 			
1224XX
3342YY
5566ZZ
;
RUN;


* 5. LECTURA DE DADES D'UN ARXIU TXT;
/*Importarem la base de dades "Invest_Data.txt". Les variables estan separades per espais, aix� que no cal indicar res. Per� la primera fila cont� el 
nom de les variables i si no ho indiquem, SAS no ho podr� llegir b� perqu� es trobara valors car�cter (afegirem FIRSTOBS) */

DATA MEUDISC.nombas1;
infile 'E:\ECONOMETRIA\Pr�ctica_0\INVEST_DATA.TXT' FIRSTOBS=2;
input Year GNP Invest CPI Interest;
RUN;

PROC PRINT data=meudisc.nombas1;
RUN;

*Si tenim un arxiu txt separat per comes, li indicarem mitjan�ant "DELIMITER";
DATA MEUDISC.nombas2;
infile 'E:\ECONOMETRIA\Pr�ctica_0\Separacio_comes.TXT' DELIMITER=';';
input VAR1 VAR2 VAR3;
RUN;

PROC PRINT data=meudisc.nombas2;
RUN;

* 6. IMPORTACI� DE DADES D'ARXIUS D'ALTRES FORMATS;

/* COMANDA DBMS:
TAB: si les variables estan separades per tabuladors. 
CSV: si estan en format csv. 
XLS/XLSX: si estan en format Excel*/

PROC IMPORT DATAFILE='E:\ECONOMETRIA\Pr�ctica_0\prova.xlsx'
			OUT=MEUDISC.nombas3
			DBMS=XLSX REPLACE;
			GETNAMES=YES; *Si la primera fila cont� el nom de les variables, els llegir� com a tsl;
RUN;

PROC PRINT DATA= MEUDISC.nombas3;
RUN;


