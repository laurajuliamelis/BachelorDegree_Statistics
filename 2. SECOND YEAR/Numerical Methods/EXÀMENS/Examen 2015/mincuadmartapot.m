x=[0.25 0.50 0.75 1.00 1.25 1.50 1.75];y=[0.40 0.50 0.90 1.28 1.60 1.66 2.02];disp('potencia')A=[ones(size(x)); log(x)]'b=log(y)';[Q,R]=qr(A);b1=Q'*b;p=R\b1Z=0:0.15:2;corba = exp(p(1)).*Z.^p(2);e=y-(exp(p(1)).*x.^p(2));rcorba=norm(e)plot(x,y,'*',Z,corba,'r','LineWidth',2),title('corba')