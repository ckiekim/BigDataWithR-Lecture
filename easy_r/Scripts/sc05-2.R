# Iris data
head(iris)
tail(iris)
View(iris)
dim(iris)
str(iris)
summary(iris)

iris3

data(mtcars)
head(mtcars)
?mtcars
dim(mtcars)
View(mtcars)
str(mtcars)
summary(mtcars)

data(AirPassengers)
ap <- AirPassengers
head(ap)
View(ap)
head(AirPassengers)
View(AirPassengers)
?AirPassengers
summary(ap)

data(airquality)
head(airquality)
tail(airquality)
View(airquality)
str(airquality)
summary(airquality)
?airquality

data(Titanic)
head(Titanic)
View(Titanic)
dim(Titanic)
str(Titanic)
summary(Titanic)
?Titanic
require(graphics)
mosaicplot(Titanic, main = "Survival on the Titanic")
## Higher survival rates in children?
apply(Titanic, c(3, 4), sum)
## Higher survival rates in females?
apply(Titanic, c(2, 4), sum)
## Use loglm() in package 'MASS' for further analysis ...

data(InsectSprays)
head(InsectSprays)
View(InsectSprays)
?InsectSprays
dim(InsectSprays)
str(InsectSprays)
summary(InsectSprays)

data(Orange)
head(Orange)
?Orange
View(Orange)
str(Orange)
summary(Orange)

data(swiss)
dim(swiss)
View(swiss)
str(swiss)
summary(swiss)

rbind(c(1, 2, 3), c(4, 5, 6))
(x <- data.frame(id=c(1, 2), name=c("a", "b"), stringsAsFactors=F))
str(x)
(y <- rbind(x, c(3, "c")))
(y <- cbind(x, greek=c("alpha", "beta"), stringsAsFactors=F))
str(y)
(x$greek = c("alpha", "beta"))
str(x)

sum(1:10)
d <- matrix(1:9, ncol=3)
d
apply(d, 1, sum)
apply(d, 2, sum)
apply(d, c(1, 2), sum)
apply(d, sum)
apply(d, 3, sum)
?apply
apply(d, c(1, 2), sum)

head(iris)
apply(iris[, 1:4], 2, sum)
apply(iris[, 1:4], 2, mean)
colSums(iris[, 1:4])
colMeans(iris[, 1:4])

install.packages("doBy")
library(doBy)
summary(iris)
summaryBy(Sepal.Width + Sepal.Length ~ Species, iris)
order(iris$Sepal.Width)
iris[order(iris$Sepal.Width),]
iris[order(iris$Sepal.Length, iris$Sepal.Width), ]
orderBy(~ Sepal.Width, iris)
orderBy(~ Species + Sepal.Width, iris)

sample(1:10, 5)
sample(1:10, 5, replace = T)
sample(1:3, 2, prob = c(3, 1, 6))
sample(1:10, 10)
iris[sample(NROW(iris), NROW(iris)), ]
sampleBy(~ Species, frac=0.1, data=iris)

split(iris, iris$Species)
lapply(split(iris$Sepal.Length, iris$Species), mean)
subset(iris, Species == "setosa")
subset(iris, Species == "setosa" & Sepal.Length > 5.0)
subset(iris, select = c(Species, Sepal.Width))
subset(iris, select = -c(Species, Sepal.Width))
iris[, !names(iris) %in% c("Sepal.Width", "Species")]

x <- data.frame(name=c("a", "b", "c"), math=c(1, 2, 3))
y <- data.frame(name=c("c", "b", "a"), english=c(4, 5, 6))
x
y
merge(x, y)
cbind(x, y)
y <- data.frame(name=c("d", "b", "a"), english=c(4, 5, 6))
merge(x, y)
merge(x, y, all = T)
