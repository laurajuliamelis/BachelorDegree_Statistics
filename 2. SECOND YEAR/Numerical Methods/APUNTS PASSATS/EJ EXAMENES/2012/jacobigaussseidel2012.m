%% Exercici 2 examen 2012A=[-4 1 1 1 1; 1 -4 1 1 1; 1 1 -4 1 1; 1 1 1 -4 1; 1 1 1 1 -4];b=[1; 1; 1; 1; 1];A\b;%% JACOBIA=[-4 1 1 1 1; 1 -4 1 1 1; 1 1 -4 1 1; 1 1 1 -4 1; 1 1 1 1 -4];b=[1; 1; 1; 1; 1];D=diag(diag(A));L=tril(A,-1);U=triu(A,1);BJ=-inv(D)*(L+U)CJ=inv(D)*bx=A\b   % Solucio dei sistema%Comencem el metode iteratiux=[0; 0; 0; 0;0];   % és e mateix que x=zeros(size(b));r=A*x-b;for k=1:4:50;  x=BJ*x+CJ;  r=norm(A*x-b);  if r < 0.5*10^(-8)      break  endend%Els residus cada vegada tenen més zerositeracionsJ=keig(BJ); abs(eig(BJ)); %radi espectralradiespectralJ=max(abs(eig(BJ)))%% Metode de Gauss-SeidelA=[-4 1 1 1 1; 1 -4 1 1 1; 1 1 -4 1 1; 1 1 1 -4 1; 1 1 1 1 -4];b=[1; 1; 1; 1; 1];D=diag(diag(A));L=tril(A,-1);U=triu(A,1);BGS=-inv(D+L)*(U)CGS=inv(D+L)*bx=A\b   % Solucio dei sistema%Comencem el metode iteratiux=[0; 0; 0; 0; 0];   % és e mateix que x=zeros(size(b));r=A*x-b;for k=1:4:50;  x=BGS*x+CGS;  r=norm(A*x-b); %residu  if r < 0.5*10^(-8)      break  endenditeracionsGS=k %iteracionseig(BGS); %radi espectralradiespectralGS=max(abs(eig(BGS)))