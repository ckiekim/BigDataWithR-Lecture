mpg <- as.data.frame(ggplot2::mpg)
library(dplyr)

mpg_diff <- mpg %>%
  select(class, cty) %>% 
  filter(class %in% c("compact", "suv"))
head(mpg_diff)
tail(mpg_diff)
table(mpg_diff$class)
View(mpg_diff)

t.test(data = mpg_diff, cty ~ class, var.equal = T)

mpg_diff2 <- mpg %>%
  select(fl, cty) %>% 
  filter(fl %in% c("r", "p"))
View(mpg_diff2)

t.test(data = mpg_diff2, cty ~ fl, var.equal = T)

economics <- as.data.frame(ggplot2::economics)
cor.test(economics$unemploy, economics$pce)

head(mtcars)
dim(mtcars)

car_cor <- cor(mtcars)
round(car_cor, 2)

install.packages("corrplot")
library(corrplot)
corrplot(car_cor)
windows()
corrplot(car_cor, method = "number")

col <- colorRampPalette(c("#bb4444", "#ee9988", "#FFFFFF",
                        "#77AADD", "#4477AA"))
corrplot(car_cor,
         method = "color",
         col = col(200),
         type = "lower",
         order = "hclust",
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         diag = F)
