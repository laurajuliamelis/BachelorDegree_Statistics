for n=[50 100 143 170 200 1000 1500]
millor=1/sqrt(2*pi*n)*exp(1)/n;
for i=2:n
    millor=millor*i*exp(1)/n;
end
millor
end