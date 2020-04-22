%% Resolucio per substitucio enrera
function X=enrera(A,B)
amp=[A';B]';
n=length(B);
disp ('************Solucio del sistema AX=b per substitucio entera')
X=zeros(n,1);
X(n)=amp(n,n+1)/amp(n,n);
for i=(n-1):-1:1
X(i)=(amp(i,n+1)-amp(i,i+1:n)*X(i+1:n))/amp(i,i);
end
return


