exam <- read.csv("csv_exam.csv")
head(exam)
tail(exam)
View(exam)
dim(exam)
str(exam)
summary(exam)
quantile(exam$math)

# Mile per Gallon Data
mpg <- as.data.frame(ggplot2::mpg)
write.csv(mpg, "mpg.csv")
head(mpg)
tail(mpg)
View(mpg)
dim(mpg)
str(mpg)
?mpg
summary(mpg)

install.packages("dplyr")
library(dplyr)
df_raw <- data.frame(var1 = c(1, 2, 1), var2 = c(2, 3, 2))
df_raw
df_new <- df_raw
df_new <- rename(df_new, v2 = var2)
df_new

# 혼자서 해보기 (p112)
mpg_new <- mpg
mpg_new <- rename(mpg_new, city = cty)
mpg_new <- rename(mpg_new, highway = hwy)
head(mpg_new)

df <- data.frame(var1=c(4,3,8), var2=c(2,6,1))
df
df$var_sum <- df$var1 + df$var2
df
df$var_mean <- df$var_sum / 2
df

mpg$total <- (mpg$cty + mpg$hwy) / 2
head(mpg)
mean(mpg$total)
summary(mpg$total)
hist(mpg$total)
mpg$test <- ifelse(mpg$total >= 20, "pass", "fail")
tail(mpg, 10)
table(mpg$test)

library(ggplot2)
qplot(mpg$test)

mpg$grade <- ifelse(mpg$total >= 30, "A",
                    ifelse(mpg$total >= 20, "B", "C"))
tail(mpg, 10)
head(mpg, 100) %>% tail(10)
table(mpg$grade)
qplot(mpg$grade)

# 분석 도전 (p123)
midwest <- as.data.frame(ggplot2::midwest)
head(midwest)
write.csv(midwest, "midwest.csv")
dim(midwest)
str(midwest)
summary(midwest)

midwest <- rename(midwest, asian=popasian)
midwest <- rename(midwest, total=poptotal)
head(midwest)

midwest$perasian <- midwest$asian / midwest$total * 100
hist(midwest$perasian)

midwest$grade <- ifelse(midwest$perasian >= mean(midwest$perasian),
                        "large", "small")

table(midwest$grade)
qplot(midwest$grade)

quantile(midwest$perasian)
midwest$group <- ifelse(midwest$perasian >= 0.5211608, "A",
                        ifelse(midwest$perasian >= 0.2971697, "B",
                               ifelse(midwest$perasian >= 0.1737387, "C", "D")))
table(midwest$group)
qplot(midwest$group)