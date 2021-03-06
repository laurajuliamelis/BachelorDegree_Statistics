%% 1.3. Integraci? num?rica: f?rmules compostes. 
%  Apartat C

clc

T2(2)=T(2)+(T(2)-T(1))/(4.^1-1);
T2(3)=T(3)+(T(3)-T(2))/(4.^1-1);
T2(4)=T(4)+(T(4)-T(3))/(4.^1-1);
T2(5)=T(5)+(T(5)-T(4))/(4.^1-1);
T2(6)=T(6)+(T(6)-T(5))/(4.^1-1);
T2(7)=T(7)+(T(7)-T(6))/(4.^1-1);
T2(8)=T(8)+(T(8)-T(7))/(4.^1-1);

T3(3)=T2(3)+(T2(3)-T2(2))/(4.^2-1);
T3(4)=T2(4)+(T2(4)-T2(3))/(4.^2-1);
T3(5)=T2(5)+(T2(5)-T2(4))/(4.^2-1);
T3(6)=T2(6)+(T2(6)-T2(5))/(4.^2-1);
T3(7)=T2(7)+(T2(7)-T2(6))/(4.^2-1);
T3(8)=T2(8)+(T2(8)-T2(7))/(4.^2-1);

T4(4)=T3(4)+(T3(4)-T3(3))/(4.^3-1);
T4(5)=T3(5)+(T3(5)-T3(4))/(4.^3-1);
T4(6)=T3(6)+(T3(6)-T3(5))/(4.^3-1);
T4(7)=T3(7)+(T3(7)-T3(6))/(4.^3-1);
T4(8)=T3(8)+(T3(8)-T3(7))/(4.^3-1);

T5(5)=T4(5)+(T4(5)-T4(4))/(4.^4-1);
T5(6)=T4(6)+(T4(6)-T4(5))/(4.^4-1);
T5(7)=T4(7)+(T4(7)-T4(6))/(4.^4-1);
T5(8)=T4(8)+(T4(8)-T4(7))/(4.^4-1);

T6(6)=T5(6)+(T5(6)-T5(5))/(4.^5-1);
T6(7)=T5(7)+(T5(7)-T5(6))/(4.^5-1);
T6(8)=T5(8)+(T5(8)-T5(7))/(4.^5-1);

T7(7)=T6(7)+(T6(7)-T6(6))/(4.^6-1);
T7(8)=T6(8)+(T6(8)-T6(7))/(4.^6-1);

T8(8)=T7(8)+(T7(8)-T7(7))/(4.^7-1);

taula_resultats=[T;T2;T3;T4;T5;T6;T7;T8]'