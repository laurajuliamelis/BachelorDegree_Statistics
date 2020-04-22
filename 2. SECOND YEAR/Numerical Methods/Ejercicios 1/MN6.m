%% exemple de bucle for
n=5
x=1
for k=1:n
   x=x+1/factorial(k)
end
error=abs(x-exp(1))