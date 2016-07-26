#Data Visualization in R
#https://www.youtube.com/watch?v=WOhsomgBNhM

data(iris)
summary(iris)
sl <- iris$Sepal.Length
hist(sl)
density(sl)
sl.d <- density(sl)
plot(sl.d)

hist(sl, freq=FALSE)
lines(sl.d)

boxplot(sl)
sl.b <- boxplot(sl)
summary(sl.b)
names(sl.b)
sl.b$stats
sl.b$stats[3]

barplot(sl)

#scatterplot matrix
pairs(iris[,1:4])
pairs(iris[,1:5])

qqnorm(sl)
qqline(sl)

par(mfrow=c(2,2))
plot(sl.d)
hist(sl, freq=FALSE)
lines(sl.d)
barplot(sl)
qqnorm(sl)

#DEFUALT
par(mfrow=c(1,1))

library(lattice)
bwplot(Sepal.Length~Sepal.Width,data=iris)
dotplot(Sepal.Length~Sepal.Width,data=iris)
#Conditional Plot
xyplot(Sepal.Length~Sepal.Width | Petal.Width,data=iris)
xyplot(Sepal.Width~Sepal.Length | Species,data=iris)

library(psych)
pairs.panels(iris)

source("filename containing plotting examples")