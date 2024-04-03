iris[,2]
iris$Sepal.Width # this looks nicer and shorter 


library(dplyr)
select(iris, "Sepal.Width") 

iris$Sepal.Length 

iris$Combined <- iris$Sepal.Length + iris$Sepal.Width

head(iris$Combined) # unsure if this is correct 

iris[iris[,3] < 5,] # seclecting all rows taht have petal length less than 5

filter(iris, Petal.Length < 5) # same thing

setosa <- iris[iris["Species"] == "setosa",]

mean(setosa$Sepal.Length) # 5.006 

filter(iris, Sepal.Length > 5)

flowers_g_5 <- subset(iris, Sepal.Length > 5)

proportion <- table(flowers_g_5$Species) / nrow(flowers_g_5)

print(proportion)

iris |> filter(Species =='versicolor') |> filter(Petal.Width <1.5)

flowers_sub <- subset(iris |> filter(Sepal.Length > 5) |> filter(Petal.Length <5))

proportion_1 <- table(flowers_sub$Species) / nrow(flowers_sub)

print(proportion_1)

is.data.frame(iris)

is.matrix(iris)

X <- as.matrix(iris[,1:4])
is.matrix(X)

multi <- diag(1:4)

multiplied_X <- X %*% multi

multiplied_X


#2

Ex1 <- data.frame(
  Student=LETTERS[1:5],
  P = c(41, 72, 46, 77, 59),
  S = c(63, 82, 38, 57, 85)
)

#column means 
Ex1 |> select_if(is.numeric) |> colMeans()

#coveriances
Ex1 |> select_if(is.numeric) |> cov() # pipeline 

#shorter code using familiar commands 
colMeans(Ex1[,2:3])
cov(Ex1[,2:3])

mtcars2 <- within(mtcars, {
  vs <- factor(vs, labels = c("V", "S"))
  am <- factor(am, labels = c("automatic", "manual"))
  cyl  <- ordered(cyl)
  gear <- ordered(gear)
  carb <- ordered(carb)
})

head(mtcars2)

library(ggplot2)

ggplot(mtcars2, aes(x = wt, y = mpg, color=gear)) +
  geom_point() + 
  labs(title = "Miles per Gallon vs. Weight",
       x = "Weight",
       y = "Miles per Gallon",
       color = "Gear")

ggplot(mtcars, aes(x = cyl)) + 
  geom_point() + 
  labs(title = "Count of Cars by Number of Cylinders",
       x = "Number of Cylinders",
       y = "Count")

# Box plot of mpg grouped by gear
ggplot(mtcars2, aes(x = gear, y = mpg)) +
  geom_boxplot() +
  labs(title = "Miles per Gallon Distribution by Gear",
       x = "Gear",
       y = "Miles per Gallon")

# Scatter plot of mpg vs. hp with regression line
ggplot(mtcars2, aes(x = hp, y = mpg)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Miles per Gallon vs. Horsepower",
       x = "Horsepower",
       y = "Miles per Gallon")

data(mtcars2)
pairs(mtcars2)

library(GGally)
ggpairs(mtcars2, mapping = aes(color = am), 
        columns = c("wt", "mpg", "disp", "hp"),)

library(mvtnorm)
mu <- c(1,0)
Sigma <- matrix(c(2,1,1,2), nr = 2)
X <- rmvnorm(n =100, mean=mu, sigma=Sigma)

mean(X)
cov(X)

mu1 <- c(5, 2)
Sigma1 <- matrix(c(5,2,2,5), nr=2)
X1 <- rmvnorm(n=100000, mean=mu1, sigma=Sigma1)

mean(X1)
cov(X1)


