%% 1.4. SOLUCIONS D'EQUACIONS NO LINEALS.
%  Apartat 3a - Principal

clc
clear all
format long g


%% Metode iteratiu convergent ii).
f = @(x)x.^3 + 4.*x.^2 - 10;
g=@(x)0.5*sqrt(10 - x.^3);
x0=1;
tol=0.5*(10.^-6);
N=10;

x1 = Exercici4_Apartat3b_Funcio ( f, g, x0, tol, N)

%% Metode iteratiu convergent iii).
f = @(x)x.^3 + 4.*x.^2 - 10;
g=@(x) x-((x.^3 + 4.*x.^2 - 10)/(3.*x.^2 + 8.*x));
x0=1;
tol=0.5*(10.^-6);
N=10;

x2 = Exercici4_Apartat3b_Funcio ( f, g, x0, tol, N)