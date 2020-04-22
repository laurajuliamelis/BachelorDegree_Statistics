ties = function(x) {
  ti = sapply(lapply(unique(x), function(xi, x) x %in% xi, x),sum)
  result = ti[ti > 1]
  if (length(result) > 0)
    names(result) = paste("t", 1:length(result), sep = "")
  return(result)
}


cremes = read.table("Cremes cosmetiques.txt", header = TRUE)


crema = cremes$Cream
elimTaques = cremes$RemBlemish

# Típica anàlisi paramètrica normal:
aovCremes = aov(elimTaques ~ crema)
anova(aovCremes)
# Per determinar quines cremes són realment diferents:
TukeyHSD(aovCremes)

# Prova de permutacions equivalent a ANOVA anterior:
# Estadístic F sobre les dades reals:
fObs = anova(aovCremes)["crema", "F value"]
fObs

nperms = 19999
set.seed(23771)
fPerms <- replicate(nperms, 
                    oneway.test(sample(elimTaques) ~ crema, var.equal = TRUE)$statistic)
# Pot trigar bastant...

# p-valor:
(sum(fPerms >= fObs) + 1) / (nperms + 1)

# Per determinar quines cremes són realment diferents entre si:
meanNew.obs = mean(elimTaques[crema == "New"])
meanOld.obs = mean(elimTaques[crema == "Old"])
meanControl.obs = mean(elimTaques[crema == "Control"])
difObs = c(abs(meanNew.obs - meanOld.obs),
           abs(meanNew.obs - meanControl.obs),
           abs(meanOld.obs - meanControl.obs))
difObs

difPerms = vapply(1:nperms,
                     function(iperm) {
                       perm.elimTaques = sample(elimTaques)
                       meanNew = mean(perm.elimTaques[crema == "New"])
                       meanOld = mean(perm.elimTaques[crema == "Old"])
                       meanControl = mean(perm.elimTaques[crema == "Control"])
                       return(c(abs(meanNew - meanOld),
                                abs(meanNew - meanControl),
                                abs(meanOld - meanControl)))
                     },
                     FUN.VALUE = c(0,0,0))

p.vals = (rowSums(apply(difPerms, 2, ">=", difObs)) + 1) / (nperms + 1)
p.vals
p.adjust(p.vals, method = "holm")

kruskal.test(elimTaques ~ crema)

# Prova de Kruskal-Wallis pas a pas:
# Rangs de les valoracions:
rElimTaques = rank(elimTaques)
rElimTaques
# Suma de rangs segons crema:
sumRangsCrema = tapply(rElimTaques, crema, sum)
# Mida mostral total:
n = length(rElimTaques)
n
# Mida mostral segons crema:
nCrema = tapply(rElimTaques, crema, length)
nCrema

# Estadístic H (ignorant empats):
h = (12 / (n * (n + 1))) * sum(sumRangsCrema^2 / nCrema) - 3 * (n + 1)
h
# p-valor segons khi-quadrat:
pchisq(h, df = length(nCrema) - 1, lower.tail = FALSE)

# Correcció per empats:
ti = ties(elimTaques)
ti
h.correc = h / (1 - sum(ti^3 - ti) / (n^3 - n))
h.correc
# p-valor segons khi-quadrat:
pchisq(h.correc, df = length(nCrema) - 1, lower.tail = FALSE)

# Quines cremes són realment diferents?
comparaNewOld = wilcox.test(elimTaques[crema == "New"], elimTaques[crema == "Old"], 
                        conf.int = TRUE, correct = FALSE)
comparaNewOld
comparaNewControl = wilcox.test(elimTaques[crema == "New"], elimTaques[crema == "Control"],  
                        conf.int = TRUE, correct = FALSE)
comparaNewControl
comparaOldControl = wilcox.test(elimTaques[crema == "Old"], elimTaques[crema == "Control"],  
                        conf.int = TRUE, correct = FALSE)
comparaOldControl

p.adjust(c(comparaNewOld$p.value, comparaNewControl$p.value, comparaOldControl$p.value), 
         method = "holm")
