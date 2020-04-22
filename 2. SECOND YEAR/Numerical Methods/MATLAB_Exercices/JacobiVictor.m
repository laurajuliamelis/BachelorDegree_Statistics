%% JACOBI %%
A=[-4 1 1 1 1; 1 -4 1 1 1; 1 1 -4 1 1; 1 1 1 -4 1; 1 1 1 1 -4];
b=[1;1;1;1;1];
U=triu(A,1)
L=tril(A,-1)
D=diag(diag(A))
Bj=-(inv(D))*(L+U)
cj=(inv(D))*b
roBj=max(abs(eig(Bj)))        %% Radi espectral, si (roBj > 1) -> divergent %% 
norm(Bj)                 %% norm(Bj) > roBj %%
X=[0; 0; 0]
if roBj>=1
    disp('divergent')
break
else disp('convergent')
end
for n=0:9
   Y=Bj*X+cj
   Cota_error=(norm(Bj))/(1-(norm(Bj)))*norm(Y-X)
   X=Y;
   n=n+1
   error=abs(A\b-X)
end

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
