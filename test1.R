library(ggplot2)
library(dplyr)

# No.1
n<-6
p<-1/3
x<-0:n
(px<-dbinom(x,size = n, prob = p))

m<-170
sd<-6
qnorm(0.8,mean = 170,sd=6)

qchisq(0.95, 3)

qt(0.975, 2)

iris.s<-iris %>% filter(Species=="setosa")
m<-mean(iris.s$Sepal.Length)
sd<-sd(iris.s$Sepal.Length)
(ll <- m - 1.96 * sd / sqrt(50))
(ul <- m + 1.96 * sd / sqrt(50))

# No.2
iris.s<-iris %>% filter(Species=="setosa")
mean(iris.s$Petal.Length)
sd(iris.s$Petal.Length)

# No.3
lm(hp~disp, data = mtcars)

# No.4
fit2 <- lm(weight~height + I(height^2), data=women)
summary(fit2)

# No.5
m<-8.1
alcohol <-
  c(15.40, 11.21, 12.67, 8.87, 12.15, 9.88, 2.06, 14.50, 0.12, 4.97)
t.test(alcohol, mu=8.1)
# 결론: p-value(0.5265)가 0.05보다 크므로 영가설 채택

# No.6
mpg_drv<- mpg %>% 
  filter(class == "subcompact") %>%  filter(drv=="f" | drv=="r")
var.test(mpg_drv$cty ~ mpg_drv$drv)
t.test(mpg_drv$cty ~ mpg_drv$drv, mu=0, var.equal=FALSE)

# No.7
iris.s<-iris %>% filter(Species=="setosa")
boxplot(iris.s$Petal.Length, main="boxplot of Petal.Length")
boxplot(iris.s$Petal.Length)$stats
iris.s$Petal.Length <- 
  ifelse(iris.s$Petal.Length < 1.1 | iris.s$Petal.Length > 1.9, 
         NA, iris.s$Petal.Length)
table(is.na(iris.s$Petal.Length))
mean(iris.s$Petal.Length, na.rm=T)
sd(iris.s$Petal.Length, na.rm=T)

# No.8
data <- c("T", "T", "M", "B", "M", "M", "B")
f.data <- factor(data, levels=c("T","M","B"), 
                 labels=c("Top","Middle","Bottom"))
f.data

# No.9
x <- c(315, 101, 108, 32)
chisq.test(x, p=c(9,3,3,1)/16)

# No.10
compact <- mpg %>% 
  filter(class=="compact") %>% 
  group_by(manufacturer)  %>% 
  summarise(total_mean = mean((hwy+cty)/2)) %>% 
  arrange(-total_mean) %>% 
  head(5)
compact
