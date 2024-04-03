##################
# MVA: Exercises 5.4 
##################

# Task 1 

library(MASS)

X1 <- crabs |> dplyr::select(CL, CW) |> as.matrix()
Y1 <- crabs |> dplyr::select(FL, RW, BD) |> as.matrix()

head(Y1)

X_mean <- colMeans(X1)
Y_mean <- colMeans(Y1) 

Xcent <- sweep(X1, 2, X_mean)
Ycent <- sweep(Y1, 2, Y_mean)

Sxx <- cov(X1)
Syy <- cov(Y1)
Sxy <- cov(X1, Y1)

eigen_xx <- eigen(Sxx)
eigen_yy <- eigen(Syy)

Vx <- eigen_xx$vectors
Vy <- eigen_yy$vectors 

Sxx_inv_sqrt <- Vx %*% diag(1/sqrt(eigen_xx$values)) %*% t(Vx)
Syy_inv_sqrt <- Vy %*% diag(1/sqrt(eigen_yy$values)) %*% t(Vy)

Sxx_inv_sqrt
Syy_inv_sqrt

# Q = Sxx_half %*% Sxy %*% Syy_half

Q <- Sxx_inv_sqrt %*% Sxy %*% Syy_inv_sqrt
Q

svd_Q <- svd(Q)

# first pair of CC vectors 
a1 <- Sxx_half %*% svd_Q$u[,1] # first pair => first value u[,1] !!
a1

b1 <- Syy_half %*% svd_Q$v[,1]
b1


eta <- as.matrix(Xcent) %*% a1
eta

phi <- as.matrix(Ycent)%*% b1
phi

cor(eta, phi) # highly correlated 

cca.out <- data.frame(eta=eta, phi=phi) # what do eta and phi represent? 
cca.out
library(ggplot2)
ggplot(cca.out, aes(x=eta, y=phi)) + geom_point() 

library(CCA)

cca <- cc(X1, Y1)
plt.cc(cca, var.label = TRUE)


#################
# Task 2
################

# prem season 2022 CCA

library(dplyr)

prem <- read.csv("/Users/tomsharples/Desktop/R/soccer-standings.csv", header=T)
table <- prem |> dplyr::select(Team, W, D, L, G, GA, GD)
knitr::kable(head(table, 5), booktabs=TRUE, escape=FALSE) # escape makes sure that special characters will not trigger syntax errors

X <- table[, c('W', 'D')]
Y <- table[, c('G', 'GA')]

# compute Sxx and Syy 
# SVD Sxx and Syy to find the inv sqrt 
# compute Q and svd(Q) 
# first first pair of CC vectors 
# find eta and phi 

Sxx <- cov(X)
Syy <- cov(Y)
Sxy <- cov(X,Y)

eigen_x <- eigen(Sxx)
eigen_y <- eigen(Syy)

Ux <- eigen_x$vectors
Uy <- eigen_y$vectors

Ax <- diag(1/sqrt(eigen_x$values))
Ay <- diag(1/sqrt(eigen_y$values))

Sxx_inv_sqrt <- Ux %*% Ax %*% t(Ux)
Syy_inv_sqrt <- Uy %*% Ay %*% t(Uy)

# Finding Q and svd of Q 
Q <- Sxx_inv_sqrt %*% Sxy %*% Syy_inv_sqrt
svd_Q <- svd(Q)

a1 <- Sxx_inv_sqrt %*% svd_Q$u[,1]
b1 <- Syy_inv_sqrt %*% svd_Q$v[,1]

a1
b1

# Finding eta and phi 

Xcent <- sweep(X, 2, colMeans(X))
Ycent <- sweep(Y, 2, colMeans(Y))
eta <- as.matrix(Xcent) %*% a1
phi <- as.matrix(Ycent) %*% b1 
eta 
phi

# plotting for interpreation 

cca.out <- data.frame(Team= table$Team, eta=eta, phi=phi)
library(ggplot2)
ggplot(cca.out, aes(x=eta, y=phi, label=Team)) + geom_point() + geom_text(aes(label=Team), hjust=0, vjust=0, size=4)

# plotting eta vs phi is a very linear relationship => theyre highly correlated 
cor(eta, phi) # 0.967 = first singular value 
svd_Q$d[1] # 0.967 these are the same init


# Using CCA library 

library(CCA)

prem.cca <- cc(X,Y)
prem.cca$cor # same as d values in Q 
prem.cca$xcoef # canonical cor vectors for X 
prem.cca$ycoef # ^^ for y 

head(prem.cca$scores$xscores) # canonical cor variables eta 
head(prem.cca$scores$yscores) 

prem.cca 
plt.cc(prem.cca, var.label=T, ind.names=table$Team)

# This is much easier but I dont undestand it as much, I understand the theory for Sxx etc

# do CCA with (W, D) and (G, GA, L)

prem <- read.csv("/Users/tomsharples/Desktop/R/soccer-standings.csv", header=T)
table <- prem |> dplyr::select(Team, W, D, L, G, GA, GD)
knitr::kable(head(table, 5), booktabs=TRUE, escape=FALSE)

X <- table[, c('W', 'D')]
Y <- table[, c('G', 'GA', 'L')]

Sxx <- cov(X)
Syy <- cov(Y)
Sxy <- cov(X,Y)

eigen_x <- eigen(Sxx)
eigen_y <- eigen(Syy)

Ux <- eigen_x$vectors
Uy <- eigen_y$vectors

Ax <- diag(1/sqrt(eigen_x$values))
Ay <- diag(1/sqrt(eigen_y$values))

Sxx_inv_sqrt <- Ux %*% Ax %*% t(Ux)
Syy_inv_sqrt <- Uy %*% Ay %*% t(Uy)

Q <- Sxx_inv_sqrt %*% Sxy %*% Syy_inv_sqrt
svd_Q <- svd(Q)

# solutions
a1 <- Sxx_inv_sqrt %*% svd_Q$u # first canonical vectors 
b1 <- Syy_inv_sqrt %*% svd_Q$v 

Xcent <- sweep(X, 2, colMeans(X))
Ycent <- sweep(Y, 2, colMeans(Y))

eta <- as.matrix(Xcent) %*% a1
phi <- as.matrix(Ycent) %*% b1 

Sxx_inv_sqrt
Syy_inv_sqrt
Q
svd_Q
a1 
b1 
eta 
phi 

# Check using CCA library and function 

cc(X,Y) # big tick !! 

# they match therefore my method is correct alhamdulliah ¡¡

##############
# Exercise 3 
#############

mm <- read.csv("https://stats.idre.ucla.edu/stat/data/mmreg.csv")
colnames(mm) <- c("Control", "Concept", "Motivation",
                  "Read", "Write", "Math",
                  "Science", "Sex")
summary(mm)
psych <- mm[, 1:3]
acad <- mm[, 4:7]

Spp <- cov(psych)
Saa <- cov(acad)
Spa <- cov(psych, acad)

eigen_p <- eigen(Spp)
eigen_a <- eigen(Saa)

Up <- eigen_p$vectors 
Ua <- eigen_a$vectors

Ap <- diag(1/sqrt(eigen_p$values))
Aa <- diag(1/sqrt(eigen_a$values))

# U A U^T 

Spp_inv_sqrt <- Up %*% Ap %*% t(Up)
Saa_inv_sqrt <- Ua %*% Aa %*% t(Ua)

# finding Q 

Q <- Spp_inv_sqrt %*% Spa %*% Saa_inv_sqrt
svd_Q <- svd(Q)

a1 <- Spp_inv_sqrt %*% svd_Q$u
b1 <- Saa_inv_sqrt %*% svd_Q$v

psych_cent <- sweep(psych, 2, colMeans(psych))
acad_cent <- sweep(acad, 2, colMeans(acad))

eta <- as.matrix(psych_cent) %*% a1 
phi <- as.matrix(acad_cent) %*% b1 

Spp_inv_sqrt
Saa_inv_sqrt
Q
svd_Q
a1
b1
eta 
phi 

cca <- cc(psych, acad) 

plt.cc(cca, var.label = T, ind.names = colnames(mm))

# Need to add interpretation later 

