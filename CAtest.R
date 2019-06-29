table.T <- matrix(c(8, 5, -2, 2, 4, 2, 0, -3, 3, 6, 2, 3, 3, -3, -6, 
                    -6, -4, 1, -1, -2), nrow = 5)
table.SVD <- svd(table.T)
table.SVD$d

X <- matrix(c(1801, 774, 171, 111, 266, 84, 205, 117, 35), nrow = 3)
Xp <- X / sum(X)
# Dr <- matrix(c(0.51, 0, 0, 0, 0.07, 0, 0, 0, 0.01), nrow = 3)


r <- Xp %*% matrix(c(1, 1, 1), nrow = 3)
c <- t(Xp)  %*% matrix(c(1, 1, 1), nrow = 3)

Dr <- diag(as.numeric(r))
Dc <- diag(as.numeric(c))


R <- solve(Dr) %*% Xp
C <- t(solve(Dc) %*% t(Xp))
Deviation <- Xp - r %*% t(c)

z <- solve(expm::sqrtm((Dr))) %*% Deviation %*% solve(expm::sqrtm((Dc)))

V <- svd(z)$v

CentR <- R - matrix(c(1, 1, 1), nrow = 3) %*% t(c)

## sqare root of matrix Dc
a.eig <- eigen(Dc)
Dc.sqrt <- a.eig$vectors %*% diag(sqrt(a.eig$values)) %*% solve(a.eig$vectors)

## row profile
F <-  CentR %*% solve(Dc) %*% (Dc.sqrt %*% V)
