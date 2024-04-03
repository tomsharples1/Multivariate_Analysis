##############################
# Linear discriminant analysis
##############################

library(MASS)
data(crabs)
head(crabs)

# 1) use LDA for predict sex against 5 numerical measurements
crabs.lda <- lda(sex ~ FL+RW+CL+CW+BD, data=crabs)
crabs.lda

# 2) Test predictive accuracy
# split into test and train sets

set.seed(123)
test.ind <- sample(200, size=50)

crabs.test <- crabs[test.ind,]
crabs.train <- crabs[-test.ind,]

crabs.lda1 <- lda(sex ~ FL+RW+CL+CW+BD, crabs.train)
crabs.pred <- predict(crabs.lda1, crabs.test)

lda.acc <- sum(crabs.pred$class == crabs.test$sex)/dim(crabs.test)[1]*100
lda.acc

table(crabs.test$sex, crabs.pred$class)
# one female crabs has been msisclassified 

# 3) Plot the histogram of 1d projections of the data 

par(mfrow=c(2,1))
plot(lda(sex ~ FL+RW+CL+CW+BD, crabs))
# 1D projections just mean LDA 

# 4) Use your classifier to predict the sex of a crab that has 
# BD=14, FL=15.5, RW=13.3, CL=31, and CW=36.
# What probability does the classifier give for this crab being male?

newdata <- data.frame(BD=14, FL=15.5, RW=13.3, CL=31, CW=36)
newdata_pred <- predict(crabs.lda, newdata)

newdata_pred$posterior[2] * 100 # probability of crab being male i think

# 5) Create a new variable that indicates the species and the sex of the crab.
# With levels BM, BF, OM, OF. This can be done as follows:
crabs$spsex <- factor(paste(crabs$sp, crabs$sex, sep=""))

# Build a classifier to predict the species and sex of crabs
# Test its predicitve accuracy and provide plots to show effectiveness

# Classifier

crabs1.lda <- lda(spsex ~ FL+RW+CL+CW+BD, data=crabs)
plot(crabs1.lda, col=as.integer(crabs$spsex))

set.seed(123) 
test.index <- sample(1:200, size=50)
crabs.test <- crabs[test.index,]
crabs.train <- crabs[-test.index,]

crabs.lda<-lda(spsex ~ FL+RW+CL+CW+BD, crabs.train)
crabs.pred <- predict(crabs.lda, crabs.test)
sum(crabs.pred$class == crabs.test$spsex)/dim(crabs.test)[1]*100

table(crabs.test$spsex, crabs.pred$class)


#######
# TASK2
#######

# generating multivariate norm
set.seed(123)
mu1 <- c(1,1)
mu2 <- c(-1,1)
mu3 <- c(1,-1)
mu4 <- c(-1,-1)
Sigma <- matrix(c(0.5,0.4,0.4,0.5), nr=2)
library(mvtnorm)
S1 <- rmvnorm(100, mu1, Sigma)
S2 <- rmvnorm(100, mu2, Sigma)
S3 <- rmvnorm(100, mu3, Sigma)
S4 <- rmvnorm(100, mu4, Sigma)
X=rbind(S1,S2,S3,S4)
dat <- data.frame(popn=as.factor(c(rep("A",100),rep("B",100),rep("C",100),rep("D",100))), X1=X[,1], X2=X[,2])

# 1) plot all 4 popn on the same graph and color them by popn 
library(ggplot2)
ggplot(aes(x=X1, y=X2, color=popn), data=dat) + geom_point()

# 2) Use LDA to train a classifier. 
# Plot the 2d projection found, and use the partimat command from the klaR package 
# to visualise the discriminant regions.

par(mfrow=c(1,1))
dat.lda <- lda(popn~., data=dat)
plot(dat.lda, col=as.integer(dat$popn))

library(klaR)
partimat(popn~., data=dat, method="lda")

# 3) Experiment with different population means, 
# different number of populations, and different covariance functions. 
# What makes populations easy/hard to separate?

set.seed(123)
mu1 <- c(1,1)
mu2 <- c(-1,1)
mu3 <- c(1,-1)
mu4 <- c(-1,-1)
Sigma <- matrix(c(0.5,0.4,0.4,0.5), nr=2)
library(mvtnorm)
n =5000
S1 <- rmvnorm(n, mu1, Sigma)
S2 <- rmvnorm(n, mu2, Sigma)
S3 <- rmvnorm(n, mu3, Sigma)
S4 <- rmvnorm(n, mu4, Sigma)
X=rbind(S1,S2,S3,S4)
dat1 <- data.frame(popn=as.factor(c(rep("A",100),rep("B",100),rep("C",100),rep("D",100))), X1=X[,1], X2=X[,2])

partimat(popn~., data=dat1, method="lda")

# as population size increases it becomes harder to separate them

#########
# TASK 3
########

# 1) Create a training set of 1000 images, 
# and try using the lda command to fit a linear classifier. Did it work?
load(file='/Users/tomsharples/Desktop/R/data_sets/MNIST.rda')

X <- as.matrix(mnist$train$x[1:1000,])
y <- mnist$train$y[1:1000]
library(MASS)
lda(X, y)
# this does not work !


# 2)  Do PCA on your training data using just the X's to make a 100x100 dataset

X.pca <-prcomp(X)
p=100 # note prediction accuracy gets worse as p increases too large
Xrot = X.pca$x[,1:p]
plot(Xrot[,1], Xrot[,2], col=y)

# 3) Do linear discriminant on the 100 PC variables you derived in the previous part. 
# Plot the LDA projections of the data

mnist.lda <- lda(Xrot, y, prior=rep(0.1, 10))
x.lda <- predict(mnist.lda)$x[,1:3]

plot(x.lda[,1], x.lda[,2], col=y)

# 4) Find the predictive accuracy of your classifier using the MNIST test data. 

Xtest <- as.matrix(mnist$test$x)
ytest <- as.matrix(mnist$test$y)

Xtest <- as.matrix(mnist$test$x[1:10000,])
Ytest <- as.matrix(mnist$test$y[1:10000])
Xtest.rot <- predict(X.pca, Xtest)[,1:p]
Ytest.pred <- predict(mnist.lda, Xtest.rot)

sum(Ytest.pred$class == Ytest)/length(Ytest)

table(Ytest.pred$class, Ytest)

# 5) Does the predictive accuracy change if instead of using the first p=100
# You can also try using larger training sets 

n = 10000
p = 10

X <- as.matrix(mnist$train$x[1:n,])
y <- mnist$train$y[1:n]
X.pca <- prcomp(X)
Xrot <- X.pca$x[,1:p]
mnist.lda <- lda(Xrot, y, prior=rep(0.1, 10))

Xtest <- as.matrix(mnist$test$x)
ytest <- as.matrix(mnist$test$y)

Xtest <- as.matrix(mnist$test$x[1:n,])
Ytest <- as.matrix(mnist$test$y[1:n])
Xtest.rot <- predict(X.pca, Xtest)[,1:p]
Ytest.pred <- predict(mnist.lda, Xtest.rot)

sum(Ytest.pred$class == Ytest)/length(Ytest)

# 72% n=1000, p=400
# 65.7% n=1000, p=500
# 79% n=1000, p=50
# 86.8% n=10000, p=50


















