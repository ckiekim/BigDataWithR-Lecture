data <- read.csv("f:/Workspace/R/r_stat/Data/ch02.csv",
                 header = F, na.strings = c("."))
str(data)
data$V1 <- factor(data$V1, levels = c(1, 2),
                  labels = c("남자", "여자"))
data$V3 <- factor(data$V3, levels = 1:14,
                  labels = c("가구주", "가구주의 배우자", "자녀",
                    "자녀의 배우자", "가구주의 부모", "배우자의 부모",
                    "손자녀, 그 배우자", "증손자녀, 그 배우자", "조부모",
                    "형제자매, 그 배우자", "형제자매의 자녀, 그 배우자",
                    "부모의 형제자매, 그 배우자", "기타 친인척",
                    "그외같이사는사람"))
data$V4 <- factor(data$V4, levels = 1:8,
                  labels = c("안 받았음", "초등학교", "중학교",
                    "고등학교", "대학-4년제 미만", "대학-4년제 이상",
                    "석사과정", "박사과정"))
str(data)
save.image("f:/Workspace/R/r_stat/Data/data.rda")

str(cars)
plot(cars$speed, cars$dist,
     main="속도와 제동거리", xlab = "속도(mph)",
     ylab = "제동거리(ft)", pch = 20, col = "blue")
library(ggplot2)
ggplot(data = cars, aes(x = speed, y = dist)) +
  geom_point() +
  ggtitle("속도와 제동거리")

str(Nile)
plot(Nile)
plot(Nile, type = "p")
head(Nile)
Nile_new <- data.frame(year = c(1871:1970), water = decompose(Nile))
head(Nile_new)
ggplot(data = Nile_new, aes(x = year, y = water)) +
  geom_line()

load("f:/Workspace/R/r_stat/Data/data.rda")
tableV5 <- table(data$V5)
barplot(tableV5)
tableV5

hist(data$V2,
     main = "연령별 분포", xlab = "연령", ylab = "빈도")
hist(data$V2, breaks = c(seq(0, 90, 10)), right = F,
     main = "연령별 분포", xlab = "연령", ylab = "빈도")
hist(data$V2, probability = T,
     main = "연령별 분포", xlab = "연령", ylab = "빈도")

table.V4 <- table(data$V4)
table.V4
pie(table.V4, main = "학력수준별 비율", cex = 0.8)
v4 <- data.frame(table(data$V4))
colnames(v4) <- c("education", "cnt")
str(v4)

options(scipen = 10)
windowsFonts(malgun = "맑은 고딕")
theme_update(text = element_text(family = "malgun"))
ggplot(data = v4, aes(x="", y=cnt, fill=factor(education))) +
  geom_bar(stat="identity", width = 1) +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill="education", 
       x=NULL, 
       y=NULL, 
       title="학력수준별 비율", 
       caption="출처: 통계청 2010년 인구총조사") +
  scale_fill_brewer(palette="Dark2") +
  coord_polar(theta = "y", start=0)

library(scales)
ggplot(data = v4, aes(x="", y=cnt, fill=factor(education))) +
  geom_bar(stat="identity", width = 1) +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill="education", 
       x=NULL, 
       y=NULL, 
       title="학력수준별 비율", 
       caption="출처: 통계청 2010년 인구총조사") +
  scale_fill_brewer(palette="Dark2") +
  geom_text(aes( 
                label = percent(cnt/468284)), size=3) +
  coord_polar(theta = "y", start=0)





ranicafe <- read.csv("f:/Workspace/R/r_stat/data/cafedata.csv", 
                     header=T, 
                     na.strings="na", stringsAsFactors=FALSE )
ranicafe <- na.omit(ranicafe)
str(ranicafe)

ggplot(ranicafe, aes(Coffees)) + 
  geom_bar(fill="gray") + 
  ggtitle("라니의 카페 커피 판매량") + 
  theme(plot.title = element_text(size = 15, face="bold")) +
  xlim(0, 50) + xlab("판매량") +
  ylab("횟수") + 
  scale_y_continuous(breaks=0:10)

# 최소값과 최대값
ranicafe$Coffees
sort(ranicafe$Coffees)
sort(ranicafe$Coffees)[1]  
sort(ranicafe$Coffees, decreasing=TRUE)
sort(ranicafe$Coffees, decreasing=TRUE)[1]  
min( ranicafe$Coffees )
max( ranicafe$Coffees )
