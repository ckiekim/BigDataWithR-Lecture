# 문항 1
dbinom(3, 6, 1/3)
pnorm(1)
qnorm(0.9, 172, 6)
qchisq(0.95, 5)
pt(2, 3)

# 문항 2
library(dplyr)
iris.s <- iris %>% filter(Species == "setosa")
mu <- mean(iris.s$Sepal.Length)
sd <- sd(iris.s$Sepal.Length)
(ul <- mu + 1.96 * sd / sqrt(50))
(ll <- mu - 1.96 * sd / sqrt(50))
# ll(4.908) < 신뢰구간 < ul(5.104)

# 문항 3
lm(iris.s$Sepal.Width ~ iris.s$Sepal.Length)
# Sepal.Width = -0.5694 + 0.7985 * Sepal.Length

# 문항 4
subcompact <- mpg %>% 
  filter(class=="subcompact") %>% 
  group_by(manufacturer)  %>% 
  summarise(total_mean = mean((hwy+cty)/2)) %>% 
  arrange(-total_mean) %>% 
  head(5)
subcompact

# 문항 5
alcohol <- c(16.90, 13.21, 15.67, 9.87, 13.15, 9.98, 3.56, 14.50, 8.12, 6.97)
shapiro.test(alcohol)  # 정규성 검정, p-value가 0.818 이므로 정규분포임
t.test(alcohol, mu = 8.1)
# 1-sample-t 검정 결과 p-value가 0.04562 이므로 영가설 기각.
# 즉 알코올 섭취량이 달라졌다.

# 문항 6
library(ggplot2)
mpg.sc <- mpg %>% 
  filter(class == "subcompact") %>% filter(drv == "r" | drv == "f")
var.test(mpg.sc$hwy ~ mpg.sc$drv)
# 분산검정 결과 p-value가 0.01441 이므로 분산이 다름
t.test(mpg.sc$hwy ~ mpg.sc$drv, mu=0, var.equal=FALSE)
# 2-sample-t 검정 결과 p-value가 6.558e-06 이므로 영가설 기각.
# 즉 고속도로 연비가 다르다.

# 문항 7
mean(iris.s$Sepal.Width)  # 3.428
sd(iris.s$Sepal.Width)    # 0.3790644

boxplot(iris.s$Sepal.Width)
boxplot(iris.s$Sepal.Width)$stats
iris.s$Sepal.Width <- ifelse(iris.s$Sepal.Width < 2.9, 
                             NA, iris.s$Sepal.Width)
mean(iris.s$Sepal.Width, na.rm = T) # 3.45102
sd(iris.s$Sepal.Width, na.rm = T)   # 0.345882

# 문항 8
data <- c("H", "L", "M", "L", "M", "M", "H")
f.data <- factor(data, levels = c("H", "M", "L"),
                 labels = c("High", "Middle", "Low"))
str(f.data)
f.data

# 문항 9
beans <- c(322, 109, 99, 29)
chisq.test(beans, p = c(9, 3, 3, 1)/16)
# 카이제곱검정 결과 p-value가 0.6413 이므로 영가설 채택.
# 실험 결과 멘델의 유전 법칙을 따른다.

# 문항 10
fit2 <- lm(weight~height + I(height^2), data=women)
summary(fit2)
# weight = 261.87818 - 7.34832 * height + 0.08306 * height^2