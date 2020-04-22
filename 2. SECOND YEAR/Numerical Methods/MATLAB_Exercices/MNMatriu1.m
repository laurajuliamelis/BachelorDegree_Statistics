%% Resolucio per substitucio endavant
%% Matriu quadrada
%% B vector fila
function X=endavant(A,B)
amp=[A';B]';
n=length(B);
X=zeros(n,1);
X(1)=amp(1,n+1)/amp(1,1);
for i=2:n
X(i)=(amp(i,n+1)-amp(i,i+1:n)*X(i+1:n))/amp(i,i);
end
return

%%  A=[1.62 1 0.75 0.62 0.52 0.46]
%%  b=[0.5 1 1.5 2 2.5 3]
%%  X=MNMatriu1(A,b)