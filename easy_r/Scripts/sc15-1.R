exam <- read.csv("f:/Workspace/R/easy_r/Examples/csv_exam.csv")
exam[]
exam[1:5, ]
exam[,3:5]

exam[exam$class == 1,]
exam[exam$math >= 80,]
exam[exam$math >= 80 & exam$science >= 80,]

exam[, 3]
exam[, "math"]
exam[, c("id", "class", "math")]

exam[4, 5]
exam[exam$class == 4, c("math", "english")]

exam$tot <- (exam$math + exam$english + exam$science) / 3
aggregate(data = exam[exam$math >= 50 & exam$english >= 80,],
          tot ~ class, mean)

library(dplyr)
exam %>%
  filter(math >= 50 & english >= 80) %>% 
  mutate(total = (math + english + science) / 3) %>% 
  group_by(class) %>% 
  summarise(mean = mean(total))

mpg$total <- (mpg$cty + mpg$hwy)/ 2
aggregate(data = mpg[mpg$class == "suv" | mpg$class == "compact",],
          total ~ class, mean)

var1 <- c(1, 2, 3, 1, 2)
var2 <- factor(c(1, 2, 3, 1, 2))
var1
var2
var1 + 2
var2 + 2
class(var1)
class(var2)
levels(var1)
levels(var2)

var3 <- c("a", "b", "b", "c")
var4 <- factor(c("a", "b", "b", "c"))
var3
var4
class(var3)
class(var4)

mean(var1)
mean(var2)
var2
var2 <- as.numeric(var2)
mean(var2)

class(mpg$drv)
mpg$drv <- as.factor(mpg$drv)
class(mpg$drv)
levels(mpg$drv)
