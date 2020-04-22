%% matriu simetrica
n=6
for i=1:n  %files
    for j=1:i-1 %columnes
    M(i,j)=-12
    M(j,i)=-M(i,j)
    end
    M(i,i)=100
    
end