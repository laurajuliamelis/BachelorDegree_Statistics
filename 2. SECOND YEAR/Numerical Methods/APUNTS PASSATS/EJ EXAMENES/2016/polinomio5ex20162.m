%%SOLO SIRVE PARA LA GRAFICA EJ C%%NO COGER RESULTADOS PARA EL EJERCICIO B%%recta a0+a1xx=[0 0.15 0.31 0.5 0.6 0.75];y=[1.0 1.004 1.031 1.117 1.223 1.422];disp('recta')A=[ones(size(x)); x]'b=y';sol=A\bZ=0:0.25:2;%%solr(1)=a0 y solr(2)=a1recta=sol(1)+sol(2)*Z;plot(x,y,'*',Z,recta,'c','LineWidth',2),title('recta')e=y-(sol(1)+sol(2)*x);r=norm(e)x=[0 0.15 0.31 0.5 0.6 0.75];y=[1.0 1.004 1.031 1.117 1.223 1.422];disp('polinomio')A=vander(x,5); %%mirar help vanderb=y';[Q,R]=qr(A);b1=Q'*b;coef_pol=R\b1ZZ=0:0.1:2;pol=polyval(coef_pol,ZZ);e=y-polyval(coef_pol,x);rpol=norm(e)plot(x,y,'*',Z,recta,'c',ZZ,pol,'r','Linewidth',2)