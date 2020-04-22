# ******************************************************************************
# Problema 12
# ******************************************************************************
# Kendall - Spearman (cas sense empats)
# ******************************************************************************
# Dades agafades del problema 2, pesos que eren capaços d'aixecar 12 voluntaris, 
# abans (x) i després (y) de programa d'entrenament ("lleugerament" retocades per que
# no hi hagi empats)
x = c(14.4, 15.9, 14.5, 13.9, 16.6, 17.4, 18.6, 20.3, 20.4, 15.4, 15.5, 14.1)
y = c(20.4, 22.9, 19.4, 24.3, 25.1, 20.9, 24.6, 24.4, 24.9, 19.9, 21.5, 21.4)

# Mida mostral:
n = length(x)

# Taula amb totes les possibles diferències entre x[i] i x[j]:
difs.x = outer(x,x, "-")
# Descartem les diferències de la diagonal (i == j) i de la meitat triangular superior:
difs.x = difs.x[ltri <- lower.tri(difs.x)]
# Totes les possibles diferències entre y[i] i y[j]:
difs.y = outer(y,y, "-")[ltri]


# Versió alternativa del codi anterior fent servir 'for'
# (és unes 6 vegades més lenta que utilitzant 'outer')
#difs.x = difs.y = numeric(n * (n - 1) / 2)
#k = 0
#for (i in 1:n) {
#  if (i == n) break
#  for (j in (i+1):n) {
#    k = k + 1
#    difs.x[k] = x[i] - x[j]
#    difs.y[k] = y[i] - y[j]
#  }
#}

# Total de diferències concordants i discordants:
concor = sum(sign(difs.x)*sign(difs.y) > 0)
discor = sum(sign(difs.x)*sign(difs.y) < 0)

# Estadístic de Kendall:
(concor - discor) / length(difs.x)

# Versió alternativa de l'estadístic de Kendall
sum(sign(difs.x)*sign(difs.y)) / length(difs.x)

# Càlcul directe amb la funció 'cor' de R:
cor(x,y, method = "kendall")
# Comparació amb la correlació de Pearson:
cor(x,y)

# Forma directa R per fer la prova de significació de la correlació de Kendall
# H0: tau = 0   vs   H1: tau > 0
cor.test(x,y, method = "kendall", alternative = "greater", exact = TRUE)

# La mateixa prova de significació feta a partir de les taules:
# Caldria haver calculat la correlació mostral de Kendall: 0.3939394.
# Buscaríem a la taula de valors crítics per la tau de Kendall, "One tailed",
# per 0.05 i n = 12 trobaríem el valor 0.394, just coincident amb el
# valor mostral si l'arrodonim a tres decimals. De fet pel caràcter discret
# d'aquest estimador, la veritable probabilitat d'error de tipus I associada
# a aquest valor no és exactament 0.05 sinó lleugerament inferior, podem
# perfectament rebutjar H0.
# 

# L'índex de Kendall és un estadístic basat en rangs, solament utilitza la
# informació continguda als rangs, el resultat és el mateix amb les dades
# originals i amb els seus rangs:
rX = rank(x)
rY = rank(y)

cor(rX,rY, method = "kendall")

# Correlació de Spearman:
# La calcularem com la correlació mostral de Pearson sobre els rangs:
cor(rX, rY)
cor(x, y, method = "spearman")

# Càlcul de la correlació de Spearman a partir de la fórmula directa:
(12 / (n * (n^2 - 1))) * sum(rX * rY) - 3 * (n + 1) / (n - 1)

# Prova de significació:
cor.test(x, y, method = "spearman", alternative = "greater")
# Que no és el mateix que fer:
cor.test(rX, rY, alternative = "greater")
# Fixem-nos que pel primer cas ha utilitzat la prova de significació
# no paramétrica, basada en rangs, pel coeficient de Spearman.
# En canvi, pel segon cas ha fet (possiblement no del tot adequadament)
# la prova paramètrica basada en la suposició de normalitat bivariant,
# sobre les dades de rangs.


# ************************************************************************
# Problema 13
# Cas amb empats: dades "Law School"
# ************************************************************************
proves = read.table("Law_school.txt", header = TRUE)

n = nrow(proves)

# Càlcul de totes les possibles diferències:
difs.x = difs.y = numeric(n * (n - 1) / 2)
k = 0
for (i in 1:n) {
  if (i == n) break
  for (j in (i+1):n) {
    k = k + 1
    difs.x[k] = proves$LSAT[j] - proves$LSAT[i]
    difs.y[k] = proves$GPA[j] - proves$GPA[i]
  }
}

# Versió més ràpida, utilitzant la funció 'outer':
difs.x = outer(proves$LSAT, proves$LSAT, "-")
difs.x = difs.x[ltri <- lower.tri(difs.x)]
difs.y = outer(proves$GPA, proves$GPA, "-")[ltri]

# Determinació de totes les sèries d'empats i la seva llargada per 
# un vector qualsevol 'x':
ties = function(x) {
  ti = sapply(lapply(unique(x), function(xi, x) x %in% xi, x),sum)
  result = ti[ti > 1]
  if (length(result) > 0)
    names(result) = paste("t", 1:length(result), sep = "")
  return(result)
}

# Sèries d'empats i llargada de cada sèrie, per x i y:
ti = ties(proves$LSAT)
ti
ui = ties(proves$GPA)
ui

n = length(proves$LSAT)
N = length(difs.x) # = n(n - 1) / 2

# Total de diferències concordants i discordants:
concor = sum(sign(difs.x)*sign(difs.y) > 0)
discor = sum(sign(difs.x)*sign(difs.y) < 0)

(concor + discor) / N

# Tau-a:
(concor - discor) / length(difs.x)
(concor - discor) / (concor + discor)
# Tau-b:
(concor - discor) / 
  sqrt((N - 0.5 * sum(ti*(ti-1))) * (N - 0.5 * sum(ui*(ui-1))))
# Tau-c:
k = min(n - sum(ti), n - sum(ui))
2 * k * (concor - discor) / (n * n * (k - 1))

# Tau-a:
sum(sign(difs.x)*sign(difs.y)) / N
# Tau-b:
sum(sign(difs.x)*sign(difs.y)) / 
  sqrt((N - 0.5 * sum(ti*(ti-1))) * (N - 0.5 * sum(ui*(ui-1))))
# Tau-c:
k = min(n - sum(ti), n - sum(ui))
2 * k * sum(sign(difs.x)*sign(difs.y)) / (n * n * (k - 1))


cor(proves, method = "kendall")
cor(proves$LSAT, proves$GPA, method = "kendall")

cor.test(proves$LSAT, proves$GPA, method = "kendall")

rLSAT = rank(proves$LSAT)
rGPA  = rank(proves$GPA)

cor.test(proves$LSAT, proves$GPA, method = "kendall")
cor.test(rLSAT, rGPA, method = "kendall")

require(Kendall)
Kendall(proves$LSAT, proves$GPA)

require(pvrank)
tau.estim = comprank(proves$LSAT, proves$GPA, indr = "kendall", 
                     tiex = "midrank")
tau.estim
comprank(proves$LSAT, proves$GPA, indr = "kendall", tiex = "woodbury")
comprank(proves$LSAT, proves$GPA, indr = "kendall", tiex = "wgh")
comprank(proves$LSAT, proves$GPA, indr = "kendall", tiex = "gh")
comprank(proves$LSAT, proves$GPA, indr = "kendall", tiex = "dubois")

ranktes(tau.estim, n, index = "kendall", approx = "gaussian", CC = FALSE, 
        type = "two-sided")
ranktes(tau.estim, n, index = "kendall", approx = "gaussian", CC = TRUE, 
        type = "two-sided")

# Correlació de Spearman:
# La calcularem com la correlació mostral de Pearson sobre els rangs:
cor(rLSAT, rGPA, method = "pearson")
# o directament:
cor(proves$LSAT, proves$GPA, method = "spearman")

# Significació:
cor.test(proves$LSAT, proves$GPA, method = "spearman")
# Que no és el mateix que fer la prova t de Student basada en la suposició
# de normalitat bivariant sobre els rangs:
cor.test(rLSAT, rGPA, method = "pearson")

