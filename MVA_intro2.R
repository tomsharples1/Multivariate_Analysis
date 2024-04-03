# Computing exercises 2.5

a = c(3,1,1,6)
b = c(5,6,2,8)

A <- matrix(a, nrow=2, byrow=T)
A

B <- matrix(b, nrow=2, byrow=T)
B

A%*%B # matrix multiplication 

A + B # matrix addition

dim(A)

3*A

A*B

t(a) %*% b # t() transposes a matrix 

solve(A) # yields the inverse of the matrix 

det(A) # det

sum(diag(A)) # trace is the dum of the diagonal 


# 1 
c = c(3,2,1,2,1,3,1,3,2)
C <- matrix(c, nrow=3, byrow=T)
C

v = c(1,1,1)
X1 <- solve(C) %*% v
X1

# 2 

X <- as.matrix(iris[,1:4])
colMeans(X)
cov(X)
cor(X)

# 2)ii
n =150
H = diag(rep(1,n)) - rep(1,n) %*% t(rep(1,n)) / n

H_T <- H %*% t(H)
H_T

H_2 <- H %*% H 
H_2

trunc(colMeans(H%*%X)) # trunc used to round 

cov(H)
cor(H)

# 2)iii
X_centered <- sweep(X, 2, colMeans(X))
cov_matrix <- t(X_centered) %*% X_centered / (n-1)
cov_matrix


# 3.6 computer tasks 

A <- matrix(c(3,1,1,6), nrow=2, byrow=T)
Eig = eigen(A)
lambda = Eig$values
lambda

Q = Eig$vectors

# Spectral decomposition of A is A = Q∆Q^T 

Q%*%diag(lambda)%*%t(Q) # therefore is true 

Asqrt = Q %*% diag(lambda**0.5) %*% t(Q)
Asqrt%*%Asqrt

A_trunc <- lambda[1] * Q[,1] %*% t(Q[,1])
A_trunc

D <- A - A_trunc 

# 2 norm 
svd(D)$d[1]

# Frobenius norm 
sqrt(sum(diag(t(D)%*%D))) # trace(D^T D)^0.5
sqrt(sum(svd(D)$d^2))    # (sum sigma_i^2)^0.5

# Frobenios norm = 2-norm in this case 
# as D is a rank 1 matrix 

##################################
# 2 
#################################

# singlar value decomposition can be computed in R using 
# using svd()

data(iris)

n = 150
H <- diag(rep(1,n)) - rep(1,n)%*% t(rep(1,n)) / n
X <- H%*% as.matrix(iris[,1:4])

X.svd <- svd(X) # compact 
X.svd
sigma <- X.svd$d
sigma # singular values

U <- X.svd$u
V <- X.svd$v

# check that Xv= sigma U 

t(X %*% V[,1]) - sigma[1] %*% U[,1]

## best rank 1,2,3 approximations 

norms <- data.frame()

# rank1

Xr <- sigma[1] * U[,1] %*% t(V[,1])
D <- X-Xr
norms[1,1] <- sqrt(sum(diag(t(D)%*%D)))
norms[1,2] <- svd(D)$d[1]

for (rank in 2:4){
  Xr <- U[,1:rank] %*% diag(sigma[1:rank]) %*% t(V[,1:rank])
  D = X-Xr
  # Frobenius norm 
  norms[rank, 1] <- sqrt(sum(diag(t(D)%*%D)))
  norms[rank, 2] <- svd(D)$d[1] # 2-rnorm
}

colnames(norms) <- c("Frobenius-nrom", "2-norm")
norms

# Eigenvalues

XtX <- eigen(t(X) %*% X)
XtX$values
svd(X)$d^2 # yields the same thing

cov(X) *149/150
t(X)%*%X/150

svd(X)$d/sqrt(149)
sqrt(eigen(cov(X)*149/150)$values)

S <- cov(X)
eigen(S)$vectors[,1]
V[,1]

xSx <- function(x){
  - t(x) %*% S %*% x / sum(x^2)
  # maximising x^T Sx subject to x^T x=1
  # is equiv to maxing 
  # x^T S x / x^T x 
  # minus sign because optim minimises 
}

xSx(c(1:4))
out <- optim(c(1,1,1,1), xSx, )
out$par/sqrt(sum(out$par^2)) # normalise

eigen(S)$vectors[,1]


######################################
##.4
######################################

X <- as.matrix(iris[,1:4])
v <- rnorm(dim(X)[2])

for (iter in 1:500){
  u <- X %*% v 
  u <- u/sqrt(sum(u^2))
  v <- t(X) %*% u
  v <- v/sqrt(sum(v^2))
}

X.svd <- svd(X)
X.svd$v[,1]/v
X.svd$u[,1]/u

