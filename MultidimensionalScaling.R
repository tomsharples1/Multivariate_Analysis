##########################
# Multivariate analysis 
# Multidimensional scaling 
##########################

data("eurodist")
eurodist

eurodist.mds <- cmdscale(eurodist)
eurodist.mds


plot(eurodist.mds, main="Distances of European cities")
text(eurodist.mds, labels=row.names(eurodist.mds))


eurodist.mds.eig <- cmdscale(eurodist, eig=TRUE)
eurodist.mds.eig$eig

# we see lots of negative values in the eignvalue matrix which means that the distances
# could be distance by road but it cannot be euclidian ¡

# Create the Euclidean distance matrix from your set of 2-dimensional points. 
# What is the Frobenius norm between this matrix and the original distance matrix?

dist1 <- dist(eurodist.mds)
dist2 <- dist(cmdscale(eurodist, k=3), diag=TRUE, upper=TRUE)
sqrt(sum((eurodist-dist1)^2)*2)

sqrt(sum((eurodist - dist2)^2)*2) # uses 3 dim


# Task 2 

df=structure(list(a = c(0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0), b = c(0, 
                                                                0, 0, 0, 0, 0, 1, 0, 1, 0, 1), c = c(0, 0, 0, 0, 1, 0, 0, 1, 
                                                                                                     0, 1, 0), d = c(1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0), e = c(0, 0, 
                                                                                                                                                             1, 0, 0, 0, 0, 0, 0, 0, 1), f = c(0, 0, 1, 0, 0, 0, 0, 0, 0, 
                                                                                                                                                                                               0, 1), g = c(0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0), h = c(1, 0, 0, 
                                                                                                                                                                                                                                                    0, 0, 0, 0, 1, 1, 0, 0), i = c(0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 
                                                                                                                                                                                                                                                                                   0)), class = "data.frame", row.names = c(NA, -11L), .Names = c("a", 
                                                                                                                                                                                                                                                                                                                                                  "b", "c", "d", "e", "f", "g", "h", "i"))
df

# Jaccard 

jaccard <- 1 - as.matrix(dist(df, method="binary", diag=T, upper=T))
jaccard

# SMC 

SMC <- (dim(df)[2] - as.matrix(dist(df, method="manhattan", diag=T, upper=T)))/dim(df)[2]
SMC 

FtoD <- function(FF){
  n = dim(FF)[1]
  D <- matrix(nr=n, nc=n)
  for (ii in 1:n){
    for (jj in 1:n){
      D[ii,jj] <- sqrt(FF[ii, ii] + FF[jj,jj] - 2*FF[ii, jj] )
    }
  }
  return (D)
}

jacc.mds <- cmdscale(FtoD(jaccard))
smc.mds <- cmdscale(FtoD(SMC))

par(mfrow=c(1,2))
plot(jacc.mds, cex=0, main="MDS for JACC")
text(jacc.mds, labels=1:11)
plot(smc.mds,cex=0, main="MDS for SMC ")
text(smc.mds, labels=1:11)


# Task 3

data(swiss)

library(dplyr)
library(ggpubr)
library(ggrepel)


mds <- swiss |> dist() |> cmdscale() |> data.frame() 
# take swiss data, compute dist(), take those dist() and make cmdscale matrix 
# then convert from matrix to data fra,ma
colnames(mds) <- c("x", "y")

ggplot(mds, aes(x=x, y=y)) + geom_point() + geom_text_repel(aes(label=row.names(mds)), size=3)

# geom_text_repel makes each label easier to see avoiding overlaps 

mds3D <- swiss |> dist() |> cmdscale(k=3) |> data.frame()
colnames(mds3D) <- c("x", "y", "z")

library(rgl)
plot(mds3D)

library(ggplot2)
D=1-cor(swiss)
mdsR <- D |> cmdscale() |> data.frame()
colnames(mdsR) <- c("Dim.1", "Dim.2")
# Plot MDS
ggscatter(mdsR, x = "Dim.1", y = "Dim.2", 
          label=colnames(swiss),
          repel = TRUE)


# NEXT TASK

X <- matrix(c(0,0,
                 1,0,
                 0,1,
                 -1,0,
                 0, -1), nc=2, byrow=TRUE)

dist(X) # tells you the distances
D <- as.matrix(dist(X), upper=T, diag=T)

# first we calculate A which is the Hadamard product D * D

A = - D^2/2

# Forming the centering matrix H 

H <- diag(5) - 1/5 * matrix(rep(1,5), nc=1) %*% matrix(rep(1,5), nr=1)

B <- H %*% A %*% H # (H %*% X) %*% t(H %*% X)
round(B,2)
(H %*% X) %*% t(H %*% X) # these are equal 

# or you can use sweep command 

Xc <- sweep(X, 2, colMeans(X))
Xc %*% t(Xc) # gives the same answer 

# To be able to apply theorem 2 B must be positive semi definite

B.eig <- eigen(B)
B.eig$values # 2 values positive and the other 3 are 0 so B is positive semi definite 
# because 2 eigen values are given this means the rank(B) = 2
# so working in 2D space 

# Theorem 2 says X = UA^1/2 
Y <- sqrt(B.eig$values[1:2])* B.eig$vectors[,1:2]
Y

# Now these values are not the ones we started with however 
# checking the distances tells us that 

dist(Y)
dist(X) # the distance matrix is the same !!!

# We were trying to find any 5 points in 2D space that had these distances 

# now using Xc 

Xc <- sweep(X, 2, colMeans(X))
B2 <- Xc %*% t(Xc)

B2.eig <- eigen(B2)
B2.eig


Y2 <- sqrt(B2.eig$values[1:2])* B2.eig$vectors[,1:2]
Y2
dist(Y2)
# you get another set of points, shows the numerical instabilities 

# nice and easy R command for this 
Y.mds <- cmdscale(D) # Classical multi-dimesional scaling 
round(Y.mds, 2)

################################################################################

D2 <- D
D2[2,1] <- 0.5
D2[1,2] <- D2[2,1]
D2

A <- -0.5 * D2^2
A
B2 <- H %*% A %*% H
B2.eig <- eigen(B2)
B2.eig$values

# this matrix is not positive definite because we have a negative eigenvalue
# Then this is not a euclidean distance matrix 

# Finding the positive part of B 

lambda_sqrt <- diag(sqrt(B2.eig$values[1:2])) # taking the positive part 
lambda_sqrt

U <- B2.eig$vectors[,1:3]
U

Y <- U %*% lambda_sqrt
Y

dist(Y)
dist(X) # the distance matricies are not the same but they are close 

lambda_sqrt1 <- diag(sqrt(B2.eig$values[1:3]))

Y2 <- U %*% lambda_sqrt1
Y2

dist(Y2)
dist(X) # slightly closer than before 

# R command 

cmdscale(D2, k=2)
dist(cmdscale(D2, k=2)) # these are the same distances as the Ys 


################################################################################
# Binary attribute data 

animal <- matrix(c(1,1,0,0,1,1,1,1,1,0,0,1,1,0,0,
                   1,0,1,0,0,0,1,0,1,0,0,0,0,1,0), 
                 nr=5, byrow=TRUE)
rownames(animal) <- c("Lion", "Giraffe", "Cow", "Sheep", "Human")
colnames(animal)<-paste("A", 1:6, sep="")

SMC <- function(x,y){
  sum(x==y)/length(x)
}

n= dim(animal)[1]

F_SMC = outer(1:n, 1:n, 
              Vectorize(function(i,j){
                SMC(animal[i,], animal[j,])
              }
              )) # for each combinatio of 1:n to 1:n apply this function 

# for SMC we use Manhattan/L1 and we have to subtract it from the largest
# possible distance to get the SMC 

(6 - as.matrix(dist(animal, method="manhattan", diag=T, upper=T)))/6

# Jaccard similarity coefficient 

jaccard <- function(x,y){
  bt = table(y,x)
  return ((bt[2,2])/bt[1,2]+ bt[2,1] + bt[2,2])
}

F_jaccard = outer(1:n,1:n, Vectorize(function(i,j){
  jaccard(animal[i,], animal[j,])
}
))

F_jaccard

jaccard(animal[1,], animal[2,])

# Jaccard uses dist with binary distance metric 

1 - as.matrix(dist(animal, method="binary", diag=T, upper=T))

# To perform MDS on this data we first need to covert from a similarity matrix F 
# to distance matrices 

FtoD <- function(FF){
  n=dim(FF)[1]
  D <- matrix(nr=n, nc=n)
  for (ii in 1:n){
    for (jj in 1:n){
      D[ii,jj] <- sqrt(FF[ii,ii] + FF[jj,jj] - 2*FF[ii,jj])
    }
  }
  return(D)
}

mds1 <- cmdscale(FtoD(F_SMC))
mds2 <- cmdscale(FtoD(F_jaccard))

plot(mds1, xlab = "MDS1 Dimension 1", ylab = "MDS1 Dimension 2",
     ylim=c(-0.45, 0.8), xlim=c(-0.7, 0.6)) ; text(mds1[, 1], mds1[, 2], rownames(animal), pos=3)

plot(mds2)

################################################################################
# Classical MDS with MNIST dataset 

load('/Users/tomsharples/Desktop/R/mnist.rda')
X<- mnist$train$x[1:1000,]

Y<- (X>0.3)*1.  # multiply by 1 to convert from T/F to a 1/0

n=dim(Y)[1]
p=dim(Y)[2]

F_SMC=(p-as.matrix(dist(Y, method="manhattan", diag = TRUE, 
                        upper = TRUE)))/p
F_Jaccard = 1-as.matrix(dist(Y, method="binary", diag=TRUE, 
                             upper=TRUE))

mds1=data.frame(cmdscale(FtoD(F_SMC))) # dont forget cmdscale 
mds2=data.frame(cmdscale(FtoD(F_Jaccard)))

Digit = as.factor(mnist$train$y[1:1000])
library(ggplot2)
# plotting using SMC 
ggplot(mds1, aes(x=X1, y=X2, colour=Digit, label=Digit))+
  geom_text(aes(label=Digit))+ ggtitle("MDS for MNIST using the SMC") 

# plotting using jaccard 

ggplot(mds2, aes(x=X1, y=X2, color=Digit, label=Digit))+
  geom_text(label=Digit) + ggtitle("MDS for MNIST using Jaccard ")


16200/(473**2)


