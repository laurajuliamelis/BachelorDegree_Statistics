libname t5 '.';

proc nlp tech=QUANEW;
 min z;
 parms x=0, y=0;
 bounds x>=0, y>=0;
 nlincon c1-c4<=0;
 c1=((5-x)**2+(45-y)**2)**(1/2)-40;
 c2=((12-x)**2+(21-y)**2)**(1/2)-40;
 c3=((17-x)**2+(5-y)**2)**(1/2)-40;
 c4=((52-x)**2+(21-y)**2)**(1/2)-40;
 z=((5-x)**2+(45-y)**2)**(1/2)+((12-x)**2+(21-y)**2)**(1/2)+((17-x)**2+(5-y)**2)**(1/2)+((52-x)**2+(21-y)**2)**(1/2);
run;


proc nlp tech=NMSIMP;
 min z;
 parms x=0, y=0;
 bounds x>=0, y>=0;
 nlincon c1-c4<=0;
 c1=((5-x)**2+(45-y)**2)**(1/2)-40;
 c2=((12-x)**2+(21-y)**2)**(1/2)-40;
 c3=((17-x)**2+(5-y)**2)**(1/2)-40;
 c4=((52-x)**2+(21-y)**2)**(1/2)-40;
 z=((5-x)**2+(45-y)**2)**(1/2)+((12-x)**2+(21-y)**2)**(1/2)+((17-x)**2+(5-y)**2)**(1/2)+((52-x)**2+(21-y)**2)**(1/2);
run;
 
