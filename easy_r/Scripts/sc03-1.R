a<-2
a
var1 <- c(1, 3, 5, 7, 9)
var1
var2 <- c(1:5)
var2
var3 <- seq(1, 5)
var3
var4 <- seq(1, 10, by=2)
var4
var5 <- seq(1, 10, by=3)
var5
var6 <- c(5, 10)
var5 + var6
var7 + var6
typeof(var7)
typeof(var6)
var9 <- c(1L, 3L)
typeof(var9)
var10 <- c(1L:10L)
typeof(var10)
var10
str1 <- "a"
str1
str2 <- "Hello"
str2
typeof(str2)
str3 <- c("a", "b", "c")
str3
str4 <- c("Hello", "World", "is", "good")
str4
str1 + 2    # 불가
str4 + var5 # 불가
mean(var3)
max(var3)
min(var3)
paste(str4, collapse = ", ")
paste(str4, collapse = " ")
str5 <- paste(str4, collapse = " ")
str5
str6 <- c("a", "b", "c", "a")
qplot(str6)
qplot(data=mpg, x=hwy)
qplot(data=mpg, x=cty)
qplot(data=mpg, x=drv, y=hwy)
qplot(data=mpg, x=drv, y=hwy, geom="line")
qplot(data=mpg, x=drv, y=hwy, geom="boxplot")
?qplot
qplot(data=mpg, x=drv, y=hwy, geom="boxplot", color=drv)

score <- c(80, 60, 70,  50, 90)
score
mean(score)
avg = mean(score)
avg

