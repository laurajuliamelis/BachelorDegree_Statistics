libname t5 '.';


proc univariate data=t5.datos_cartera;
 var ibc nmc nbs;
run;
proc means data=t5.datos_cartera;
 var ibc nmc nbs;
run;

proc corr data=t5.datos_cartera cov;
 var ibc nmc nbs;
run;


proc nlp tech=CONGRA OUTEST=t5.pp;
 min z;
 parms p1=0.3, p2=0.3, p3=0.4;
 bounds 0<=p1<=1, 0<=p2<=1,0<=p3<=1;
 lincon 0.0764167*p1+0.1343333*p2+0.1493333*p3>=0.12, 
        p1+p2+p3=1;
 z=0.0028182652*p1**2+0.0030091515*p2**2+0.0401111515*p3**2+
2*(-0.0002750606*p1*p2+0.0048011212*p1*p3-0.0059145758*p2*p3);
run;

proc print data=t5.pp;
run;

proc nlp tech=CONGRA OUTEST=t5.pp2;
 max z;
 parms p1=0.3, p2=0.3, p3=0.4;
 bounds 0<=p1<=1, 0<=p2<=1,0<=p3<=1;
 lincon p1+p2+p3=1;
 z=(1-0.1)*(0.0764167*p1+0.1343333*p2+0.1493333*p3)-
    0.1*(0.0028182652*p1**2+0.0030091515*p2**2+0.0401111515*p3**2+
2*(-0.0002750606*p1*p2+0.0048011212*p1*p3-0.0059145758*p2*p3));
run;

proc print data=t5.pp2;
run;
