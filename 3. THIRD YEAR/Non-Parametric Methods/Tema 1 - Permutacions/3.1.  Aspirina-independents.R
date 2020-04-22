abans = c(270, 150, 270, 420, 202, 255, 165, 220, 305, 210, 240, 300, 300, 70)
despres = c(525, 570, 190, 395, 370, 210, 490, 250, 360, 285, 630, 385, 195, 295)

dades <- c(abans, despres)
n.abans <- length(abans)

suma.real <- sum(abans)
suma.real

nperms <- 9999
sums.perm <- replicate(nperms, {
  sum(sample(dades)[1:n.abans])
}
)

(sum(sums.perm <= suma.real) + 1) / (nperms + 1)
