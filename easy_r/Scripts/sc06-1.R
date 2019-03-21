install.packages("dplyr")
library(dplyr)
exam <- read.csv("Examples/csv_exam.csv")
exam
exam %>% filter(class == 1)
exam %>% filter(class == 2)
exam %>% filter(class != 1)
exam %>% filter(class != 3)
exam %>% filter(math >= 50)
exam %>% filter(math < 50)
exam %>% filter(english >= 80)
exam %>% filter(english < 80)
exam %>% filter(class == 1 & math >= 50)
exam %>% filter(class == 2 & english >= 80)
exam %>% filter(math >= 80 | english >= 90)
exam %>% filter(math >= 80 & english >= 90)
exam %>% filter(math >= 80 | english >= 90) %>% filter(class == 3)

exam %>% filter(class == 1 | class == 3 | class == 5)
exam %>% filter(!(class == 2 | class == 4))
exam %>% filter(class %in% c(1, 3, 5))

class1 <- exam %>% filter(class == 1)
mean(class1$math)

# 혼자서 해보기(p133)
displ4 <- mpg %>% filter(mpg$displ <= 4)
displ5 <- mpg %>% filter(mpg$displ > 4)
mean(displ4$hwy)
mean(displ5$hwy)
mpg %>% filter(mpg$displ <= 4) %>% mean(mpg$hwy)  # Fail

exam %>% select(math)
exam %>% select(english)
exam %>% select(class, math, english)
exam %>% select(class, math, english) %>% head(10)
exam %>% select(class, math, english) %>% str
exam %>% select(class, math, english) %>% summary
exam %>% select(-math) %>% head
exam %>% select(-math, -english) %>% head
exam %>% filter(class == 1) %>% select(english)
exam %>% 
  filter(class == 3) %>% 
  select(-english)

exam %>% arrange(math)
library(doBy)
orderBy(~ math, exam)

exam %>% arrange(desc(math))
orderBy(~ -math, exam)
exam %>% arrange(class, desc(math))
orderBy(~ class + math, exam)
orderBy(~ class - math, exam)

mpg %>% filter(manufacturer == "audi") %>% arrange(desc(hwy)) %>% head(5)
mpg %>% filter(manufacturer == "audi") %>% arrange(-hwy) %>% head(5)
mpg_audi <- mpg %>% filter(manufacturer == "audi")
orderBy(~ hwy, mpg_audi) %>% head(5)

exam %>% mutate(total = math + english + science) %>% head
str(exam)
exam %>% 
  mutate(total = math + english + science,
         average = (math + english + science) / 3) %>% head
exam %>% 
  mutate(test = ifelse(science >= 60, "pass", "fail")) %>% head
exam %>% 
  mutate(total = math + english + science,
         average = total / 3) %>% head
exam %>% 
  mutate(total = math + english + science) %>% 
  arrange(-total) %>% head

exam %>% 
  mutate(average = (math + english + science) / 3,
         grade = ifelse(average >= 90, "A", 
                    ifelse(average >= 80, "B",
                        ifelse(average >= 70, "C",
                            ifelse(average >= 60, "D", "F"))))) %>% 
  arrange(-average)

mpg_new <- mpg %>% mutate(total = cty + hwy) 
head(mpg_new)

mpg %>% 
  mutate(avg = (cty + hwy) / 2) %>% 
  arrange(-avg) %>% head(3)

exam %>% 
  group_by(class) %>% 
  summarise(mean_math = mean(math),
            sum_math = sum(math),
            median_math = median(math),
            n = n())

mpg %>% 
  group_by(manufacturer, drv) %>% 
  summarise(mean_cty = mean(cty)) %>% 
  head(10)

mpg %>% 
  group_by(manufacturer) %>% 
  filter(class == "suv") %>% 
  mutate(avg = (hwy + cty) / 2) %>% 
  summarise(mean_avg = mean(avg)) %>% 
  arrange(-mean_avg) %>% 
  head(5)

mpg %>% 
  group_by(class) %>% 
  summarize(mean_cty = mean(cty)) %>% 
  arrange(-mean_cty) %>% 
  head

test1 <- data.frame(id = c(1:5),
                    midterm = c(60, 80, 70, 90, 85))
test1
test2 <- data.frame(id = c(1:5),
                    final = c(70, 83, 65, 95, 80))
test2
total <- left_join(test1, test2, by = "id")
total
merge(test1, test2)
name <- data.frame(class = c(1:5),
                   teacher= c("kim", "lee", "park", "choi", "han"))
exam
exam_new <- left_join(exam, name, by = "class")
exam_new
merge(exam, name)

group_a <- data.frame(id = c(1:5),
                    test = c(60, 80, 70, 90, 85))
group_a
group_b <- data.frame(id = c(1:5),
                    test = c(70, 83, 65, 95, 80))
group_b

group_all <- bind_rows(group_a, group_b)
group_all

fuel <- data.frame(fl = c("c", "d", "e", "p", "r"),
                   price_fl = c(2.35, 2.38, 2.11, 2.76, 2.22),
                   stringsAsFactors = F)
mpg_fuel <- left_join(mpg, fuel, by = "fl")
mpg_fuel %>% select(model, fl, price_fl) %>% head(5)

# 분석 도전(p160)
midwest <- as.data.frame(ggplot2::midwest)
midwest %>% 
  mutate(ratio = 100 - popadults / poptotal * 100) %>% head

midwest %>% 
  mutate(ratio = 100 - popadults / poptotal * 100) %>%  
  arrange(-ratio) %>% 
  head(5)

midwest %>%
  mutate(ratio = 100 - popadults / poptotal * 100,
         grade = ifelse(ratio >= 40, "large",
                      ifelse(ratio >= 30, "middle", "small"))) %>% 
  group_by(grade) %>% 
  summarise(count = n())

midwest %>% 
  mutate(ratio_asian = popasian / poptotal * 100) %>% 
  select(state, county, ratio_asian) %>% 
  arrange(ratio_asian) %>% 
  head(10)

head(iris)
head(Sepal.Length)
attach(iris)
head(Sepal.Length)
detach(iris)
head(Sepal.Length)

search()
attach(iris)
search()
detach(iris)
search()

install.packages("RMySQL")
library(RMySQL)
conn <- dbConnect(MySQL(), user="java", password="java",
                  dbname="ezen", host="127.0.0.1")
dbGetQuery(conn, "select * from orders")
orders <- dbGetQuery(conn, "select * from orders")
head(orders)
str(orders)
dbGetQuery(conn, "set names utf8")
dbGetQuery(conn, "select * from products limit 5")
dbDisconnect(conn)
