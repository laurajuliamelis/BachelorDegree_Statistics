%% exemple bucle FOR
x=2;
for k=1:1:10
        x=1/2*(x+2/x);
end 
error=abs(x-sqrt(2))