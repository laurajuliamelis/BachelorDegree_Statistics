% Script per calcular el nombre m�xim pel qual Matlab pot calcular Stirling
i=0;

while (STIRLING(i+1)./factorial(i+1))<1
    i=i+1
end

i


