# METAANALISIS - Ejercicio 5
# 10.10.2018


# 1. Lectura de datos
asp = read.table(stdin(), header=TRUE, sep=';', dec=',')
estudio;nt;obst;Propt;nc;obsc;Propc
Cardiff I;615;58;0,094;624;76;0,122
Cardiff II;847;129;0,152;878;185;0,211
Paris I;1620;244;0,151;406;77;0,190
Paris I;1563;154;0,099;1565;218;0,139
Amis;2267;395;0,174;2257;427;0,189
CDP-A;758;88;0,116;771;110;0,143
Gamis;317;39;0,123;309;49;0,159
Art;813;102;0,125;816;130;0,159
Aris;365;38;0,104;362;57;0,157
Micristin;672;65;0,097;668;106;0,159
Roma;40;9;0,225;40;19;0,475


# 2. MA efectos fijos
asp$Y = log(asp$Propt/asp$Propc)
asp$var = (1-asp$Propc)/(asp$nc*asp$Propc) + (1-asp$Propt)/(asp$nt*asp$Propt)
asp$sd = sqrt(asp$var)
asp$W = 1/asp$var
Y = sum(asp$Y * asp$W) / sum(asp$W)
SE = sqrt(1/sum(asp$W))
ic.FE = Y + c(-1,1)*1.96*SE
asp$icL = asp$Y - 1.96*asp$sd
asp$icR = asp$Y + 1.96*asp$sd

# forest plot estudios individuales
plot(c(min(asp$icL)-0.2, max(asp$icR)), c(1,13), t='n', xlab='', ylab='')
segments(asp$icL, 1:11, asp$icR, 1:11)
abline(v=0, lty=2)
text(-1.4, 1:11, lab=asp$estudio, pos=2)
points(asp$Y, 1:11, cex=sqrt(asp$W)/min(sqrt(asp$W)), pch=15)

# 3. MA efectos aleatorios
Q = sum(asp$W*(asp$Y-Y)^2)
U = 10*(mean(asp$W) - var(asp$W)/sum(asp$W))
D = (Q-10)/U # Varianza entre estudios
asp$W2 = 1/(asp$var+D)
Y2 = sum(asp$Y * asp$W2) / sum(asp$W2)
SE2 = sqrt(1/sum(asp$W2))
ic.RE = Y2 + c(-1,1)*1.96*SE2

# completamos forest plot
segments(ic.FE[1], 12, ic.FE[2], 12, lwd=2)
segments(ic.RE[1], 13, ic.RE[2], 13, lwd=2)
text(-1.4, 12:13, lab=c("Fixed Ef.", "Random Ef."), pos=2)

