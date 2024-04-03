###################
# MVA: Clusetering
##################

# Question 1

data(iris)
iris <- iris[,1:4]

dist <- dist(iris, method="euclidian")
# Perform hierarchical clustering and plot dendrogram

par(mfrow=c(1,1))

clust.sl <- hclust(dist, method="single")
clust.cplt <- hclust(dist, method="complete")
clust.avg <- hclust(dist, method="average")
clust.ward <- hclust(dist, method="ward.D2")

plot(clust.sl)
plot(clust.cplt)
plot(clust.avg)
plot(clust.ward)

# computing confusion matrix 

cutree(clust.sl, k=3)
table(cutree(clust.sl, k=3), iris$Species)

cutree(clust.cplt, k=3)
table(cutree(clust.cplt, k=3), iris$Species)

cutree(clist.avg, k=3)
table(cutree(clust.avg, k=3), iris$Species)

cutree(clust.ward, k=3)
table(cutree(clust.ward, k=3), iris$Species)


# Compare hierarchical clustering to k-means 
set.seed(123)
iris2 <- iris[,1:4]
(iris.k <- kmeans(iris2, centers = 3, nstart=25))

table(iris.k$cluster, iris$Species)

library(factoextra)
fviz_cluster(iris.k, data=iris2, ellipse.type = "norm", geom="point")

# comparing with model based clustering 

library(mclust)

iris.m <- Mclust(iris2, G=3)
plot(iris.m, what = c("classification"))

#########
# TASK 2
#########
library(dplyr)

IPL <- read.csv("~/Desktop/R/MVA /IPL.csv", header=FALSE)
head(IPL)

IPL10<-IPL %>% dplyr::filter(V2>=10) %>%
  dplyr::select(V1, V5, V6, V7, V8, V9, V12, V13)

plot(IPL10)

# Return to this later it isnt working 


#########
# TASK 3
########

data(USArrests)
USA <- USArrests

set.seed(123)
(USA.k <- kmeans(USA, centers=3, nstart=25))
fviz_nbclust(USArrests, kmeans, method="wss")

# kmeans
fviz_cluster(USA.k, data=USA)

# m clust 

USA.m <- Mclust(USA, G=3)
plot(USA.m, what=c("classification"))

USA_dist <- dist(USA)
# single link 

USA.sl <- hclust(USA_dist, method="single")
plot(USA.sl)

USA.comp <- hclust(USA_dist, method="complete")
plot(USA.comp)

USA.ward <- hclust(USA_dist, method="ward.D2")
plot(USA.ward)

USA.avg <- hclust(USA_dist, method="average")
plot(USA.avg)


