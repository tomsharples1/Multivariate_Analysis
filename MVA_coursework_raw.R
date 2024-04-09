un <- read.csv("~/Desktop/R/MVA /coursework/un.csv", header=TRUE)
gdp <- un[,3:14] # The GDP per capita.
years <- seq(1952, 2007,5) 
colnames(gdp) <- years
rownames(gdp) <- un[,2]

lifeExp <- un[,15:26] # the life expectancy 
colnames(lifeExp) <- years 
rownames(lifeExp) <- un[,2]

popn <- un[,27:38] # the population size 
colnames(popn) <- years
rownames(popn) <- un[,2]

library(dplyr)
library(tidyr)
library(ggplot2)


# Exploratory analysis 

# How have the variables GDP, lifeExp and popn changed over the years?
# Are there correlations between the variables? 
# boxplots etc 

# Begin by creating some basic exploratory data analysis plots, 
# showing how the three variables (GDP, life expectancy, population) 
# have changed over the past 70 years. 
# For example, you could show should how the average life expectancy and GDP per capita 
#for each continent has changed through time. 
# Note that there are many different things you could try -
# please pick a small number of plots which you think are most informative.

# Bar chart of number of countries per continent 
ggplot(un, aes(x=continent, fill=continent)) + geom_bar() + 
  labs(y="Number of Countries", 
       title="Number of Countries per Continent") 

# EDA GDP
gdp$country <- rownames(gdp)

gdp_long <- tidyr::pivot_longer(gdp, cols=-country, names_to="year", values_to="gdp")
gdp_long

ggplot(gdp_long, aes(x=gdp, fill=year, color=year)) + geom_density(alpha=0.5) +
  labs(x="GDP", 
       title="Distribution of GDP per year") 

# EDA LifeExp

lifeExp$country <- rownames(lifeExp)

life <- tidyr::pivot_longer(lifeExp, cols=-country, names_to="year", values_to="lifeExp")

ggplot(life, aes(x=lifeExp, fill=year, color=year)) + geom_boxplot(alpha=0.8) +
  labs(x="Life expectancy", 
       title="Distribution of Life expectancy") # I like this one a lot 

# EDA population 

popn$country <- rownames(popn)

pop<- tidyr::pivot_longer(popn, cols=-country, names_to = "year", values_to = "population")
pop_log <- tidyr::pivot_longer(popn, cols=-country, names_to = "year", values_to = "population") %>% mutate_at(vars(3), log)

ggplot(pop, aes(x=population, fill=year, color=year)) + geom_density(alpha=0.5) 
ggplot(pop_log, aes(x=population, fill=year, color=year)) + geom_density(alpha=0.5)

# Plotting GDP over 70 years

gdp_per_conti <- un %>% group_by(continent) %>% select(starts_with("gdpPercap")) %>% summarise_all(mean) %>% 
  t() %>% as.data.frame() %>% slice(-1)
colnames(gdp_per_conti) <- c("Africa", "Americas", "Asia", "Europe", "Oceania")
gdp_per_conti$year <- years

mean_gdp<- tidyr::pivot_longer(gdp_per_conti, cols = -year, names_to = "continent", values_to = "mean_gdp")

ggplot(mean_gdp, aes(x=year, y=as.numeric(mean_gdp), color=continent)) + geom_line() + geom_point() +
  labs(y="Mean GDP", x="Year", 
       title="Mean GDP over 70 yeards by continent")

# Plotting life exp over 70 years 

lifeExp_per_conti <- un %>% group_by(continent) %>% select(starts_with("lifeExp")) %>% 
  summarise_all(mean) %>% t() %>% as.data.frame() %>% slice(-1)
colnames(lifeExp_per_conti) <- c("Africa", "Americas", "Asia", "Europe", "Oceania")
lifeExp_per_conti$year <- years
mean_life <- tidyr::pivot_longer(lifeExp_per_conti, cols=-year, names_to="continent", values_to="mean_life")
ggplot(mean_life, aes(x=year, y=as.numeric(mean_life), color=continent)) + geom_line() +
  geom_point() + 
  labs(y="Mean life expectancy", 
       title = "Mean life excpectancy over 70 years by continent")


# Plotting population

popn_per_conti <- un %>% group_by(continent) %>% select(starts_with("pop")) %>% summarise_all(mean) %>%
  t() %>% as.data.frame() %>% slice(-1)
colnames(popn_per_conti) <- c("Africa", "Americas", "Asia", "Europe", "Oceania")
popn_per_conti$year <- years
mean_pop <- tidyr::pivot_longer(popn_per_conti, cols=-year, names_to = "continent", values_to = "mean_pop")
ggplot(mean_pop, aes(x=year, y=as.numeric(mean_pop), color=continent)) + geom_line() + geom_point() +
  labs(y="Mean Population",
       title = "Mean Population over 70 years by continent")

# GDP vs Life

continents <- rep(un$continent, each = 12)
overall <- cbind(continents, gdp_long, life$lifeExp, pop$population) %>% rename("life" = "life$lifeExp", "population" = "pop$population")
head(overall) # hell yeah !!!

ggplot(overall, aes(x=gdp, y=life)) + geom_point(aes(color=continents, size = population), alpha=0.6) + 
  geom_smooth(method="loess") + 
  labs(x="GDP per capita", y= "Life expectancy",
       title = "Relationship between GDP per capita and Life expectancy")

# PCA 
# Carry out principal component analysis on the three different variables. 
# It makes sense here to look at each variable type on its own 
# (i.e. do PCA on gdp, then life-expectancy, rather than doing PCA on the entire dataset
# Do I use the sample covariance or correlation matrix? why is one preferred? 
# how many principal components you would choose to retain?
# interpretation of the leading principal components

#############
# PCA for GDP
#############

library(ggfortify)
library(factoextra)

# indicates that the first 2 components are the best 

gdp <- un %>% group_by(continent) %>% select(starts_with("gdpPercap")) %>% 
  as.data.frame()

colnames(gdp)[2:13] <- years
rownames(gdp) <- un[,2]
head(gdp)

gdp.pca <- prcomp(gdp[,2:13], scale=TRUE) # TRUE = corr
summary(gdp.pca)
gdp.pca$rotation

gdp.pca$x

# Scree plot 
fviz_eig(gdp.pca, addlabels = TRUE, 
         main="Scree plot for GDP per capita")

# 94% of explained variance so just keep first PC score

# circle plot 
fviz(gdp.pca, element='var') # must use correlation 

# biplot
fviz_pca_biplot(gdp.pca, label='var', habillage = gdp$continent,
                addEllipses=TRUE, ellipse.level=0.95) # can i interpret this? 

# Scatter plot 
autoplot(gdp.pca, data=gdp, label=TRUE, shape=FALSE, 
         color="continent", label.size = 3, repel=TRUE,
         main="PCA for GDP per capita")


library(ggfortify)
library(factoextra)
gdp_log <- un %>% group_by(continent) %>% select(starts_with('gdpPercap')) %>% 
  mutate_at(vars(-1), log) %>% as.data.frame() 
colnames(gdp_log)[2:13] <- years
rownames(gdp_log) <- un[,2]

gdp_log.pca <- prcomp(gdp_log[,2:13], scale=TRUE) # TRUE = corr
summary(gdp_log.pca)

gdp_log.pca$x[,1:2]

gdp_log.pca$rotation[,1:2]

# Understanding PC2 
congo <- gdp["Congo Dem. Rep.",][,2:13] %>% t()
guin <- gdp["Equatorial Guinea",][,2:13] %>% t()
iraq <- gdp["Iraq",][,2:13] %>% t()
taiwan <- gdp["Taiwan",][,2:13] %>% t()

par(mfrow=c(2,2))
ggplot(congo, aes(x=years, y=as.numeric(congo[,1]))) + geom_line() + 
  labs(title="CONGO")

ggplot(guin, aes(x=years, y=as.numeric(guin[,1]))) + geom_line() +
  labs(title="GUINEA")

ggplot(iraq, aes(x=years, y=as.numeric(iraq[,1]))) + geom_line() +
  labs(title="IRAQ")

ggplot(taiwan, aes(x=years, y=as.numeric(taiwan[,1]))) + geom_line() + 
  labs(title = "taiwan")

# comparison of PC2 
congo <- gdp["Congo Dem. Rep.",][,2:13] %>% t()
guin <- gdp["Equatorial Guinea",][,2:13] %>% t()
iraq <- gdp["Iraq",][,2:13] %>% t()
taiwan <- gdp["Taiwan",][,2:13] %>% t()
combined <- cbind(congo, guin, iraq, taiwan)
colnames(combined) <- c("congo", "guin", "iraq", "taiwan")
ggplot(combined, aes(x = years)) +
  geom_line(aes(y = congo, color = "Congo Dem. Rep.")) +
  geom_line(aes(y = guin, color = "Equatorial Guinea")) +
  geom_line(aes(y = iraq, color = "Iraq")) +
  geom_line(aes(y = taiwan, color = "Taiwan")) + 
  geom_vline(xintercept = 1982, linetype = "dashed", color="red") +
  labs(title = "GDP growth in Congo, Guinea, Iraq and Taiwan", 
       y = "GDP per capita")

#################
# PCA for lifeExp 
#################

lifeExp <- un %>% group_by(continent) %>% select(starts_with("life")) %>%
  as.data.frame()
colnames(lifeExp)[2:13] <- years
rownames(lifeExp) <- un[,2]

head(lifeExp)

lifeExp.pca <- prcomp(lifeExp[,2:13], scale=TRUE) # TRUE = corr
summary(lifeExp.pca)


lifeExp.pca$rotation

# Scree plot
fviz_eig(lifeExp.pca, addlabels = TRUE)

# Circle plot 
fviz(lifeExp.pca, element ='var')

# biplot 
fviz_pca_biplot(lifeExp.pca, label='var', habillage = lifeExp$continent,
                                 addEllipses=TRUE, ellipse.level=0.95) # interpretation? 

# Scatter plot 
autoplot(lifeExp.pca, lifeExp, label=TRUE, shape=FALSE,
         color='continent', label.size = 3, 
         main= "PCA for Life expectancy")

lifeExp.pca$rotation[,1:2]

oman <- lifeExp["Oman",][,2:13] %>% t()
zim <- lifeExp["Zimbabwe",][,2:13] %>% t()
double <- cbind(oman, zim)
ggplot(double, aes(x=years)) + 
  geom_line(aes(y=oman, color="Oman")) +
  geom_line(aes(y=zim, color="Zimbabwe")) +
  labs(title="Life Expectancy change in Oman and Zimbabwe", 
       y="Life Expectancy")

####################################
# PCA scores for gdp against lifeExp 
####################################

plot(gdp.pca$x[,1], lifeExp.pca$x[,1], col="blue")

gdp$PC1_gdp <- gdp.pca$x[,1]
gdp$PC1_life <- lifeExp.pca$x[,1]


# scatter plot 
ggplot(gdp, aes(x=PC1_gdp, y=PC1_life, col=continent, 
                     label=rownames(gdp))) + geom_text(size=3) # skewed 

# log transformation for GDP 
gdp_log <- un %>% group_by(continent) %>% select(starts_with('gdpPercap')) %>% 
  mutate_at(vars(-1), log) %>% as.data.frame() 
colnames(gdp_log)[2:13] <- years
rownames(gdp_log) <- un[,2]

gdp_log.pca <- prcomp(gdp_log[,2:13], scale=TRUE)
gdp$PC1_gdp_log <- gdp_log.pca$x[,1]

autoplot(gdp_log.pca, gdp_log, label=TRUE, shape=FALSE,
         color='continent')

fviz_eig(gdp_log.pca, addlabels = TRUE, 
         main="Scree plot for log GDP")

# first PC scores for log gdp and lifeExp scatter 
ggplot(gdp_log, aes(x=PC1_gdp_log, y=PC1_life, col=continent, 
                     label=rownames(gdp))) + geom_text(size=3) +
  labs(title="First principal components for GDP per capita and life expectancy")


# Use your analysis to produce scatter plots of the PC scores for gdp and life expectancy,
# labeling the names of the countries and colouring the data points by continent
# can also plot the first PC score for life exp against first PC score for GDP 

# CCA 

# Perform CCA using log(gdp) and life expectancy as the two sets of variables
# Provide a scatter plot of the first pair of CC variables, label and colour the points
# what are my conclusions here? 
# What has been the effect of using log(gdp) rather than gdp as used in the PCA

library(CCA)

# log gdp vs life
cca <- cc(gdp_log[,2:13], lifeExp[,2:13])

cca$xcoef[,1] 
cca$ycoef[,1]

eta <- cca$scores$xscores[,1]
psi <- cca$scores$yscores[,1]

cca.out <- data.frame(continent = gdp_log$continent, eta=eta, psi=psi)
cca.out
ggplot(cca.out, aes(x=eta, y=psi, 
                    label=rownames(cca.out), 
                    col=continent)) + geom_text(size=3) +
  labs(y='Psi', x='Eta', 
       title = "First canonical component analysis on log GDP and Life expectancy ")

# gdp vs life 

cca1 <- cc(gdp[,2:13], lifeExp[,2:13])
eta1 <- cca1$scores$xscores[,1]
psi1 <- cca1$scores$yscores[,1]
cca1.out <- data.frame(continent=continent, eta1=eta1, psi1=psi1)
ggplot(cca1.out, aes(x=eta1, y=psi1, label=rownames(cca1.out),
                     col=continent)) + geom_text(size=3) +
  labs(y="Psi", x="Eta",
       title = "First Canonical component analysis on GDP and Life expectancy") 




# Mutlidimensional scaling 

# Using
# log GDP, life exp and log popn
# start with log GDP with continents on, then bind life then bind log popn
library(dplyr)

popn_log <- un %>% select(starts_with('pop')) %>% mutate_at(vars(-1), log)
rownames(popn_log) <- un[,2]
colnames(popn_log) <- years




overall <- cbind(continents, gdp_long, life$lifeExp, pop$population) %>% rename("life" = "life$lifeExp", "population" = "pop$population")
head(overall) # hell yeah !!!

continents <- rep(un$continent, each = 12)
gdp_long_log <- gdp %>% mutate_at(vars(-13), log) %>% tidyr::pivot_longer(cols=-country, names_to="year", values_to = "log_gdp")
lifeExp$country <- rownames(lifeExp)
lifeExp_long <- tidyr::pivot_longer(lifeExp, cols=-country, names_to='year', values_to = "life")
popn$country <- rownames(popn)
popn_log <- popn %>% mutate_at(vars(-13), log) %>% tidyr::pivot_longer(cols=-country, names_to='year', values_to = 'log_pop')

# transformed data frame !!!! WHICH DOESNT WORK LOOOOL
un.transformed <- cbind(continents, gdp_long_log, lifeExp_long$life, popn_log$log_pop) %>% rename("life" = "lifeExp_long$life", 
                                                                                                  "log_pop" = "popn_log$log_pop")
head(un.transformed)

UN.transformed <- cbind(log(un[,3:14]), un[,15:26], log(un[,27:38]))
rownames(UN.transformed) <- un[,2]

mds <- cmdscale(dist(UN.transformed)) %>% as.data.frame()
mds <- cbind(un[,1], mds)
colnames(mds) <- c("continent", "coord1", "coord2")
rownames(mds) <- un[,2]
head(mds)

ggplot(mds, aes(x=coord1, y=coord2, label=rownames(mds), col=continent)) + geom_text(size=3) +
  labs(y="Second coordinate", x="First coordinate", 
       title = "Multi Dimensional Scaling")


cmdscale(dist(UN.transformed, upper=F, diag=TRUE))

# Find and plot a 2-dimensional representation of the data. 
# colour each data point by the continent it is on
# discuss what this shows in comparison to the PCA and CCA 


# Linear discriminant analysis

# Train a classifier to predict the continent of each country
# using gdp, lifeExp, and popn from 1952-2007
# test the accuracy by splitting the data into test and train then predict 
# calculate predictive accuracy 
# is linear better than quad here?

library(MASS)

un_lda <- subset(un, select = -c(country))

un.lda <- lda(continent ~ ., data=un_lda)
un.lda

set.seed(123)

test.ind <- sample(141, size=50)

un.test <- un_lda[test.ind,]
un.train <- un_lda[-test.ind,]

un.lda1 <- lda(continent ~. , data=un.train)
un.pred <- predict(un.lda1, un.test)

lda.acc <- sum(un.pred$class == un.test$continent)/dim(un.test)[1] * 100
lda.acc # 50% 
table(un.test$continent, un.pred$class)

lda.acc <- sum(un.pred$class == un.test$continent)/dim(un.test)[1] * 100
paste(lda.acc, "%", sep="")

library(klaR)
library(plyr)


un.lda <- lda(continent ~ ., data=un_lda)

continent_numeric <- as.numeric(factor(un_lda$continent))

par(pty="s")
plot(un.lda, col=continent_numeric, abbrev=TRUE, 
     main="LDA plot")

install.packages("tinytex")

# Clustering 

# apply clustering methods to the GDP and life expectancy data
# How many clutsers is right? fviz_nbcluster() using elbow test 
# which method is best here? 
# do different methods find the same clusters ? is there a natural interpretation to the clusters 
# Scaling the data is advised 
library(dplyr)
gdp1 <- un %>% dplyr::select(starts_with("gdpPercap"))
rownames(gdp1) <- un[,2]
colnames(gdp1) <- years

gdp1_log <- un %>% dplyr::select(starts_with("gdpPercap")) %>% mutate_all(log)
rownames(gdp1_log) <- un[,2]


library(factoextra)


# GDP
set.seed(123)
(gdp.k <- kmeans(gdp1, centers=3, nstart=25))
fviz_cluster(gdp.k, data=gdp1, geom="text")
fviz_nbclust(gdp1, kmeans, method = "wss", linecolor = "red")

# log GDP
gdp1_log <- un %>% dplyr::select(starts_with("gdpPercap")) %>% mutate_all(log)
rownames(gdp1_log) <- un[,2]
set.seed(123)
gdp_log.k <- kmeans(gdp1_log, centers=3, nstart=25)
fviz_cluster(gdp_log.k, data=gdp1_log, geom='text', 
             labelsize = 10)
fviz_nbclust(gdp1_log, kmeans, method="wss")

lifeExp.scaled <- scale(lifeExp)

# k means clustering 
library(factoextra)
set.seed(123)

# GDP
(gdp.k <- kmeans(gdp1, centers=3, nstart=25))
fviz_cluster(gdp.k, data=gdp1, geom="text")

fviz_nbclust(gdp1, kmeans, method = "wss", linecolor = "red")
# this is definitely 3 clusters by the elbow test

# life exp 
lifexp.k <- kmeans(lifeExp.scaled, centers=3, nstart=25)
fviz_cluster(lifexp.k, data=lifeExp.scaled, geom="text",
             labelsize = 10)

fviz_nbclust(lifeExp.scaled, kmeans, method = "wss", linecolor = "red")
# this one could be anywhere from 3-7 im not sure 

# hierarchical clustering 

# gdp 
dist_gdp <- dist(gdp1, method="euclidian")

clust.sl <- hclust(dist_gdp, method="single")
clust.cplt <- hclust(dist_gdp, method="complete")
clust.avg <- hclust(dist_gdp, method="average")
clust.ward <- hclust(dist_gdp, method="ward.D2")


par(mfrow=c(1,1))
plot(clust.sl) # useless 
plot(clust.cplt)
plot(clust.avg) # finds 3   
plot(clust.ward) # finds 3 


table(cutree(clust.avg, k=3), continent)
table(cutree(clust.ward, k=3), continent)


# log gdp 

dist_gdp_log <- dist(gdp1_log, method="euclidian")

clust.sl2 <- hclust(dist_gdp_log, method="single")
clust.cplt2 <- hclust(dist_gdp_log, method="complete")
clust.avg2 <- hclust(dist_gdp_log, method="average")
clust.ward2<- hclust(dist_gdp_log, method="ward.D2")


par(mfrow=c(1,1))
plot(clust.sl2) # useless 
plot(clust.cplt2)
plot(clust.avg2) # finds 3   
plot(clust.ward2) # finds 3 

# life exp 

dist_life <- dist(lifeExp.scaled, method="euclidian")

clust.sl1 <- hclust(dist_life, method="single")
clust.cplt1 <- hclust(dist_life, method="complete")
clust.avg1 <- hclust(dist_life, method="average")
clust.ward1 <- hclust(dist_life, method="ward.D2")


par(mfrow=c(1,1))
plot(clust.sl1) 
plot(clust.cplt1) # finds 3 
plot(clust.avg1) # finds 3   
plot(clust.ward1) # finds 3 

# the last 3 clustering methods find the exact same clusters 

table(cutree(clust.cplt1, k=3), continent)
table(cutree(clust.avg1, k=3), continent)
table(cutree(clust.ward1, k=3), continent)

# Linear regression 

# want to predict the life exp in 2007 based off previous data 
# build model to predict life exp from gdp or log(gdp) 
# which regression method is best here: Ridge, OLS, PCR
# assess the accuracy of each method, using R^2 and methods from predicting brozek
# assess whether raw gdp is better than log(gdp)

# first create a data frame which has 2007 life and log gdp data 



linear <- un %>% dplyr::select(lifeExp_2007, starts_with("gdpPercap")) %>% mutate_at(vars(-1), log)
rownames(linear) <- un[,2]
colnames(linear)[2:13] <- c("log_gdp_1952", "log_gdp_1957", "log_gdp_1962",
                        "log_gdp_1967", "log_gdp_1972", "log_gdp_1977", 
                        "log_gdp_1982", "log_gdp_1987", "log_gdp_1992",
                        "log_gdp_1997", "log_gdp_2002", "log_gdp_2007")
head(linear)

linear_no_log <- un %>% dplyr::select(lifeExp_2007, starts_with("gdpPercap"))
rownames(linear_no_log) <- un[,2]
colnames(linear_no_log)[2:13] <- c("gdp_1952", "gdp_1957", "gdp_1962", "gdp_1967", 
                            "gdp_1972", "gdp_1977", "gdp_1982", "gdp_1987", 
                            "gdp_1992", "gdp_1997", "gdp_2002", "gdp_2007")
head(linear_no_log)


# OLS regression
# logged 
lm.ols <- lm(lifeExp_2007 ~ ., data=linear)
summary(lm.ols)
summary(lm.ols)$r.squared
mean(lm.ols$residuals^2)

# raw 

lm.ols1 <- lm(lifeExp_2007~., data=linear_no_log)
summary(lm.ols1)
summary(lm.ols1)$r.squared # raw performs worse in R squared significantly 
mean(lm.ols1$residuals^2)


# PCR 

library(pls)
lm.pcr <- pcr(lifeExp_2007 ~ ., ncomp=12, scale=TRUE, data=linear, 
              validation='CV')

lm.pcr1 <- pcr(lifeExp_2007 ~., ncomp=12, scale=T, data=linear_no_log, 
               validation="CV")


plot(RMSEP(lm.pcr), legendpos = "topright", main = "Response: LifeExp 2007 \n predictors: log GDP")
plot(RMSEP(lm.pcr1), legendpos = "topright", main = "Response: LifeExp 2007 \n predictors: GDP")

# log gdp 
pca <- prcomp(linear[,2:13], scale=TRUE)
lm.pcr2 <- lm(lifeExp_2007 ~ pca$x[,1:3], data=linear)
summary(lm.pcr2)$r.squared
mean(lm.pcr2$residuals^2) # MSE 

# raw gdp 
pca1 <- prcomp(linear_no_log[,2:13], scale=TRUE)
lm.pcr3 <- lm(lifeExp_2007 ~ pca1$x[,1:2], data=linear_no_log)
summary(lm.pcr3)$r.squared # lower again
mean(lm.pcr3$residuals^2) # higher error 


# ridge regression 

library(glmnet)

# log gdp 
X <- as.matrix(linear[,2:13])
y <- linear$lifeExp_2007
lambdas <- 10^seq(3, -4, by = -.1)

lm.ridge <- glmnet(X, y, alpha=0, lambda=lambdas)
plot(lm.ridge, xvar='lambda')

coef(lm(y~X))


set.seed(123)
cv_fit <- cv.glmnet(X, y, alpha=0, lambda=lambdas)
plot(cv_fit)

cv_fit$lambda.1se

ridge_cv_error <- min(cv_fit$cvm)
ridge_cv_error

best_lam <- cv_fit$lambda.min
best.ridge <- glmnet(X,y, alpha=0, lambda = best_lam)
coef(best.ridge)



y_pred <- predict(best.ridge, newx = X)
sst <- sum((y-mean(y))^2)
sse <- sum((y_pred-y)^2)

rsq.ridge_log <- 1- sse/sst
rsq.ridge_log

#########
# raw gdp 
#########
X1 <- as.matrix(linear_no_log[,2:13])
y1 <- linear_no_log$lifeExp_2007
lambdas <- 10^seq(3, -4, by = -.1)

lm.ridge1 <- glmnet(X1, y1, alpha=0, lambda=lambdas)
plot(lm.ridge1, xvar='lambda')

coef(lm(y1~X1))

set.seed(123)
cv_fit1 <- cv.glmnet(X1, y1, alpha=0, lambda=lambdas)
plot(cv_fit1)


cv_fit1$lambda.1se

ridge_cv_error1 <- min(cv_fit1$cvm) # error 
ridge_cv_error1

best_lam1 <- cv_fit1$lambda.min
best.ridge1 <- glmnet(X1,y1, alpha=0, lambda=best_lam1)
coef(best.ridge1)

y_pred1 <- predict(best.ridge1, s=best_lam1, newx = X1)
sst1 <- sum((y1-mean(y1))^2)
sse1 <- sum((y_pred1-y1)^2)

rsq.ridge_raw <- 1- sse1/sst1
rsq.ridge_raw


###################
# Error comparisons 
##################

# ols
summary(lm.ols)$r.squared # log 
mean(lm.ols$residuals^2)

summary(lm.ols1)$r.squared # raw 
mean(lm.ols1$residuals^2)

# pcr 
summary(lm.pcr2)$r.squared # log
mean(lm.pcr2$residuals^2) 

summary(lm.pcr3)$r.squared # raw
mean(lm.pcr3$residuals^2) 

# ridge 
ridge_cv_error <- min(cv_fit$cvm) # log
ridge_cv_error

y_pred <- predict(lm.ridge, s=best_lam, newx = X)
sst <- sum((y-mean(y))^2)
sse <- sum((y_pred-y)^2)

rsq.ridge_log <- 1- sse/sst
rsq.ridge_log

ridge_cv_error1 <- min(cv_fit1$cvm) # raw  
ridge_cv_error1

y_pred1 <- predict(lm.ridge1, s=best_lam1, newx = X1)
sst1 <- sum((y1-mean(y1))^2)
sse1 <- sum((y_pred1-y1)^2)

rsq.ridge_raw <- 1- sse1/sst1
rsq.ridge_raw

# For Raw Data
raw_data <- matrix(
  c(
    mean(lm.ols1$residuals^2), # MSE OLS (raw)
    summary(lm.ols1)$r.squared, # R^2 OLS (raw)
    mean(lm.pcr3$residuals^2), # MSE PCR (raw)
    summary(lm.pcr3)$r.squared, # R^2 PCR (raw)
    min(cv_fit1$cvm),           # MSE Ridge (raw)
    rsq.ridge_raw               # R^2 Ridge (raw)
  ),
  nrow = 2,
  byrow = FALSE,
  dimnames = list(c("MSE", "R squared"), c("OLS", "PCR", "Ridge"))
)

# Print table for raw data
as.table(raw_data)

# For Log Data
log_data <- matrix(
  c(
    mean(lm.ols$residuals^2),   # MSE OLS (log)
    summary(lm.ols)$r.squared,  # R^2 OLS (log)
    mean(lm.pcr2$residuals^2),  # MSE PCR (log)
    summary(lm.pcr2)$r.squared, # R^2 PCR (log)
    min(cv_fit$cvm),            # MSE Ridge (log)
    rsq.ridge_log               # R^2 Ridge (log)
  ),
  nrow = 2,
  byrow = FALSE,
  dimnames = list(c("MSE", "R squared"), c("OLS", "PCR", "Ridge"))
)

# Print table for log data
as.table(log_data)


linear$predicted_lifeExp <- predict(lm.ols)

linear$continent <- un[,1]

head(linear)
# Plot using ggplot2
ggplot(data = linear, aes(x = lifeExp_2007, y = predicted_lifeExp)) +
  geom_text(aes(label=rownames(linear), color=continent), size=3) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth=0.5) +
  labs(x = "Actual Life Expectancy in 2007", y = "Predicted Life Expectancy", title = "Scatter Plot for OLS Regression Model")










