%% Resolucio per substitucio endavant
%% Matriu quadrada
%% B vector fila
function X=endavant(A,B)
amp=[A';B]';
n=length(B);
X=zeros(n,1);
X(1)=amp(1,n+1)/amp(1,1);
for i=2:n
X(i)=(amp(i,n+1)-amp(i,1:i-1)*X(1:i-1))/amp(i,i);
end
return

%%  A=[5 0 0 0; 1 3 0 0; 3 4 2 0; -1 3 -6 -1]
%%  b=[-10 4 2 5]
%%  X=endavant(A,b)