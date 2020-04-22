%% Reduccio a triangular superior d'una matriu
function [U,L]=gausselim(A,B)
U=[A';B]';
m = size(U); n=m(1);
L=eye(n);
for k=1:n-1
if (abs(U(k,k))<eps)
j=find(abs(U(k:n,k))>0); j=j(1);;
aux=j+k-1;
aux=U(k,:); U(k,:)=U(j+k-1,:); U(j+k-1,:)=aux;
end
L(k:n,k)=U(k:n,k)/U(k,k);
U(k+1:n,:)=U(k+1:n,:)-L(k+1:n,k)*U(k,:);
end
return
 %% A=[10 -7 0 1; -3 2 6 0; 5 -1 5 0; -1 0 2 -1]
 %% B=[7 4 6 -3]
 %% [U,L]=gausselim(A,B)
 %% enrera(U(:,1:4),U(:,5)')