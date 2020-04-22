
y <- rvel

X <- matrix( c(rep(1,24), dens, dens2), nrow = 24 )

XtX <- t(X) %*% X
XtY <- t(X) %*% rvel

solve(XtX, XtY)
solve(XtX) %*% XtY #és el mateix

solve(XtX) #és la inversa de XtX