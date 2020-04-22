format short g
arrels=[1:10]
p=poly(arrels)
q=p 
q(2)=q(2)+1/(2^13)
arrels1=roots(p),arrels2=roots(q)