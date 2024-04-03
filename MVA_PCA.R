# Computing class problems 4 

############################
head(iris)

X <- as.matrix(iris[,1:4])
X.pca <- prcomp(X)
print(X.pca)

summary(X.pca)

X.pca.scaled <- prcomp(X, scale=T) # uses correlation matrix 

X.pca$sdev
X.pca$rotation
X.pca$center

X.pca$scale # TRUE if corr matrix used, or FALSE if cov matrix used 

head(X.pca$x)

###############################
# exercise 1 
X <- as.matrix(iris[,1:4])

mean_X <- colMeans(X)
mean_X
var_X <- cov(X)
var_X

pca_result <- prcomp(X, scale=TRUE) # scale is true using R and scale false is using S
sample_mean_pca <- pca_result$center # this is correct 

X_cent <- sweep(X, 2, mean_X) # centered matrix 
S <- t(X_cent)%*%X_cent /149

var_X # these are the same 
S

eigen_vectors <- eigen(var_X)$vectors
eigen_vectors

eigen_values <- eigen(var_X)$values
eigen_values

pca_result$sdev # singluar values 

# pca_result$sdev ^ 2 == eign_values

principal_comp <- X %*% eigen_vectors
head(principal_comp)

principal_comp_pca <- X %*% pca_result$rotation # this gives the eigenvectors using prcomp
head(principal_comp_pca)

principal_comp_score_pca <- pca_result$x
principal_comp_score_pca # gives the scores 


# 1)i 

X <- as.matrix(iris[,1:4])
mean_X <- colMeans(X)
X_cent <- sweep(X, 2, mean_X)
factor <- sqrt(nrow(X) - 1)
H_X <- X_cent / factor 
head(H_X)

svd(H_X)$d
svd(X_cent)$d # they are related by the factor 

svd(H_X)$v
svd(X_cent)$v # yes the single vectors are the same here because every vector should be a unit so the factor doesnt make a difference 

svd_res <- svd(H_X)
U <- svd_res$u
sigma <- diag(svd_res$d)
V <- svd_res$v

svd_score_HXV <- H_X %*% V
head(svd_score_HXV)

svd_score_UE <- U %*% sigma
head(svd_score_UE)

# These are equaL !!

############
# Exercise 2
############

library(MASS)

C <- as.matrix(crabs[,4:8])

pca_crabs <- prcomp(C, scale=T)
plot(pca_crabs)# Scale = T using the correlation 

var_explained <- pca_crabs$sdev^2 / sum(pca_crabs$sdev^2)

plot(1: length(var_explained), var_explained, type="b",
     main="Scree Plot", xlab = "Principal Component Number",
     ylab= "Proportion of variance explained")

C1 <- C + 15

pca_crabs_1 <- prcomp(C1, scale=T)
pca_crabs_1 # INVARIANT TO TRANSFORM  

D <- diag(5,5,5)

C2 <- C %*% D
pca_crabs_2 <- prcomp(C2, scale=T) # Invariant when using R but does change when using S 
pca_crabs
pca_crabs_2

library(pracma)
U <- randortho(5)

C3 <- C %*% U 
pca_crabs_3 <- prcomp(C3, scale=F)

##############################
# E.2 solutions 

C <- crabs[,4:8]
crabs.pca <- prcomp(C, scale=T)
plot(crabs.pca)

# plotting the PC scores using $x
plot(crabs.pca$x[,1], crabs.pca$x[,2])

# adding color will help see patterns 
library(ggfortify)

autoplot(crabs.pca, data=crabs, colour="sex", scale=F, shape="sex")
# we can clearly see a difference in male and female crabs 


crabs.pca$rotation[,1]
crabs.pca$rotation[,2]

fviz(crabs.pca, element='var')
# note that there is a clear sex dependence here; female crabs have a smaller PC2 score

autoplot(crabs.pca, data=crabs, colour='sex')

# adding a constant to each column of C 

C2 <- sweep(C, 2, c(1,2,3,4,5), FUN="+")
colMeans(C)
colMeans(C2)
head(prcomp(C2)$x/crabs.pca$x)

C3 <- as.matrix(C) %*% diag(1:5)
head(prcomp(C3)$x/crabs.pca$x)


#############
# Exercise 3 
#############

data <- read.csv("/Users/tomsharples/Desktop/R/soccer-standings.csv")
data <- data[-1, ]
X <- data[, 0:8] # we want {Team, W, D, L, G, GA, GD}
X
sample_mean = colMeans(X)


#######################################
library(factoextra)
install.packages("factoextra")


################
# exercise 3 in hand exercises
###############

lambda <- c(4.22, 2.38, 1.88, 1.11, 0.91, 0.82, 0.58, 0.44, 0.35, 0.19, 0.05, 0.04, 0.04)
lambda / sum(lambda) * 100

plot(1:length(lambda), lambda / sum(lambda) * 100, ylab="Proportion of variance explained", xlab="component number")

which(cumsum(lambda)/sum(lambda) > 0.9)
which(lambda>mean(lambda))


#############
# Exercise 4 
############

R <- matrix(c(1, 0.5792, 0.2414, 0.5792, 1, 0.5816, 0.2414, 0.5816, 1), nrow=3, byrow=T)
R
eigen(R)$vectors[,1]
#####
#ex 5
#####

library(dplyr)
football <- read.csv("/Users/tomsharples/Desktop/R/soccer-standings.csv", header = TRUE)
football
table <- football |> dplyr::select(Team, W, D, L, G, GA, GD)
knitr::kable(head(table,10), booktabs = TRUE, escape=FALSE) # creates the table in R 

tmp <- table[,2:7]
rownames(tmp)=table$Team # keeps the names in 
tmp

# scale = T uses R (correlation)
prem.pca <- prcomp(tmp, retx=T, scale=T) # retx = whether rotated values should be returned
prem.pca

prem.pca$rotation[,1]

fviz(prem.pca, element = 'var') # circle plot 

autoplot(prem.pca, data = table, label=TRUE, shape=FALSE) # PC1 vs PC2 plot 
#######
