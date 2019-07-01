table.T <- matrix(c(8, 5, -2, 2, 4, 2, 0, -3, 3, 6, 2, 3, 3, -3, -6, 
                    -6, -4, 1, -1, -2), nrow = 5)
table.SVD <- svd(table.T)
table.SVD$d

library(ca)
profs <- matrix(c(0.333, 0.067, 0.138, 0.083, 0.056, 0.200, 0.862, 0.083, 
                  0.611, 0.733, 0.000, 0.833), nrow = 4)
colnames(profs) <- c("Holidays", "HalfDays", "FullDays")
rownames(profs) <- c("Norway", "Canada", "Greece", "France/Germany")

library(rgl)
open3d()
lines3d(c(0, 1.2), c(0, 0), c(0, 0))
lines3d(c(0, 0), c(0, 1.2), c(0, 0))
lines3d(c(0, 0), c(0, 0), c(0, 1.2))
lines3d(c(0, 1), c(1, 0), c(0, 0), lwd = 2)
lines3d(c(0, 0), c(0, 1), c(1, 0), lwd = 2)
lines3d(c(0, 1), c(0, 0), c(1, 0), lwd = 2)
points3d(profs, size = 6)
texts3d(profs, texts = rownames(profs), adj = c(0, -0.3), font = 2)
texts3d(rbind(c(1.25, 0, 0), c(0, 1.25, 0), c(0, 0, 1.25)), texts = colnames(profs))

tab <- read.table(pipe("pbpaste"))
tab.rowsum <- apply(tab, 1, sum)
tab.colsum <- apply(tab, 2, sum)
tab.sum <- sum(tab)
tab.exp <- tab.rowsum %*% t(tab.colsum) /tab.sum
tab.chi2 <- sum((tab - tab.exp)^2 / tab.exp)
tab.chi2/tab.sum #the total inertia


tab <- as.matrix(tab)
tab.pro <- tab / apply(tab, 1, sum)
tab.colmass <- apply(tab, 2, sum) / sum(tab)

chidst <- 0
for(j in 1:ncol(tab)) 
  chidst <- chidst + (tab.pro[1, j] - tab.pro[2, j])^2 / tab.colmass[j]

sqrt(chidst)

dist(tab.pro %*% diag(1/sqrt(tab.colmass)))

### correspondence matrix 

tab.P <- as.matrix(tab)  / sum(tab)

### row and column masses
tab.r <- apply(tab.P, 1, sum)
tab.c <- apply(tab.P, 2, sum)

### CA step 1: the matrix S

tab.S <- diag(1/sqrt(tab.r)) %*% (tab.P - tab.r %*% t(tab.c)) %*% diag(1/sqrt(tab.c))

### CA step 2: the SVD of S

tab.svd <- svd(tab.S)

### CA steps 3&4 standard row and colum coordinates
tab.rsc <- diag(1/sqrt(tab.r)) %*% tab.svd$u
tab.cpc <- diag(1/sqrt(tab.c)) %*% tab.svd$v

### CA steps 5&6 pricinpal row and column coordinates
tab.rpc <- tab.rsc %*% diag(tab.svd$d)
tab.cpc <- tab.cpc %*% diag(tab.svd$d)

### CA step 7: principal inertias (eigenvalues) and %s
tab.svd$d^2; round(100 * tab.svd$d^2 /sum(tab.svd$d^2), 2)

### plot the row and column principal coordinates
par(mar = c(4.2, 4, 1, 1), mgp = c(2, 0.5, 0), cex.axis = 0.8, font.lab = 2)
plot(rbind(tab.rpc, tab.cpc), type = "n", asp = 1, 
     xlab = "CA dimension 1", ylab = "CA dimension 2")
abline(h = 0, v = 0, col = "gray")
text(tab.rpc, labels = rownames(tab), font = 2)
text(tab.cpc, labels = colnames(tab), font = 2, col = "gray")

# 
library(ca)
plot(ca(tab))

X <- matrix(c(1801, 774, 171, 111, 266, 84, 205, 117, 35), nrow = 3)
Xp <- X / sum(X)
# Dr <- matrix(c(0.51, 0, 0, 0, 0.07, 0, 0, 0, 0.01), nrow = 3)


r <- Xp %*% matrix(c(1, 1, 1), nrow = 3)
c <- t(Xp)  %*% matrix(c(1, 1, 1), nrow = 3)

Dr <- diag(as.numeric(r))
Dc <- diag(as.numeric(c))


R <- solve(Dr) %*% Xp
C <- solve(Dc) %*% t(Xp)
Deviation <- Xp - r %*% t(c)

z <- solve(expm::sqrtm((Dr))) %*% Deviation %*% solve(expm::sqrtm((Dc)))

V <- svd(z)$v
U <- svd(z)$u

CentR <- R - matrix(c(1, 1, 1), nrow = 3) %*% t(c)
CentC <- C - matrix(c(1, 1, 1), nrow = 3) %*% t(r)

## sqare root of matrix Dc
a.eig <- eigen(Dc)
Dc.sqrt <- a.eig$vectors %*% diag(sqrt(a.eig$values)) %*% solve(a.eig$vectors)

## sqare root of matrix Dr
a.eig <- eigen(Dr)
Dr.sqrt <- a.eig$vectors %*% diag(sqrt(a.eig$values)) %*% solve(a.eig$vectors)

## row profile
F <-  CentR %*% solve(Dc) %*% (Dc.sqrt %*% V)

## column profile 
G <- CentC %*% solve(Dr) %*% (Dr.sqrt %*% U)
