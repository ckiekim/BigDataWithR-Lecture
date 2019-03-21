library(ggplot2)
windowsFonts(malgun = "맑은 고딕")
theme_update(text = element_text(family = "malgun"))

# No.1
irse <- iris[iris$Species == "virginica",]
plot(jitter(irse$Sepal.Width)~jitter(irse$Sepal.Length))

# No.2
line<-lm(irse$Sepal.Width~irse$Sepal.Length)
abline(line, col="red", lwd=2)
summary(line)

# No.3
library(dplyr)
data <- mpg %>% 
  filter(class=="suv") %>% 
  group_by(manufacturer) %>% 
  summarise(cty_mean=mean(cty)) %>% 
  arrange(-cty_mean) %>% 
  head(7)
ggplot(data=data, 
       aes(x=reorder(manufacturer, -cty_mean), y=cty_mean, 
           fill=manufacturer )) + 
  geom_bar(stat = "identity")

# No.4
library(tibble)
states_map <- map_data("state")
crime <- rownames_to_column(USArrests, var="state")
crime$state <-tolower(crime$state)

library(ggiraphExtra)
p1 <- ggChoropleth(data=crime, 
                   aes(fill=Murder,map_id=state),
                   map = states_map)+ 
  ggtitle("미국 주별 살인범죄 비율","(단위: 인구 10만명망 건수)") +  
  xlab("경도") +
  ylab("위도") + 
  theme(plot.title = element_text(face="bold", hjust=0.5, size=15, 
                                  color="darkblue"),
        plot.subtitle = element_text(hjust=1.0))
p2 <- ggChoropleth(data=crime, 
                   aes(fill=Rape,map_id=state),
                   map = states_map)+ 
  ggtitle("미국 주별 강간범죄 비율","(단위: 인구 10만명망 건수)") +
  xlab("경도")+
  ylab("위도") + 
  theme(plot.title = element_text(face="bold", hjust=0.5, size=15,
                                  color="darkblue"),
        plot.subtitle = element_text(hjust=1.0))
p3 <- ggChoropleth(data=crime, 
                   aes(fill=Assault,map_id=state),
                   map = states_map)+ 
  ggtitle("미국 주별 강도범죄 비율","(단위: 인구 10만명망 건수)") +
  xlab("경도")+
  ylab("위도") + 
  theme(plot.title = element_text(face="bold", hjust=0.5, size=15,
                                  color="darkblue"),
        plot.subtitle = element_text(hjust=1.0))
p4 <- ggChoropleth(data=crime, 
                   aes(fill=UrbanPop,map_id=state),
                   map = states_map)+ 
  ggtitle("미국 주별 도시 비율","(단위: 인구 10만명망 건수)") +
  xlab("경도")+
  ylab("위도") + 
  theme(plot.title = element_text(face="bold", hjust=0.5, size=15,
                                  color="darkblue"),
        plot.subtitle = element_text(hjust=1.0))

library(gridExtra)
grid.arrange(p1,p2,p3,p4, ncol=2,nrow=2)

# No.5
data <- read.csv("f:/Workspace/R/r_stat/data/GDP.csv")
str(data)
tsdata <- data %>% 
  ungroup %>% 
  arrange(년도) %>% 
  ts(start = c(1988,1))
tsdata <- tsdata[,-c(1)]  # 년도 열을 제거
str(tsdata)
library(dygraphs)
dygraph(tsdata)%>% dyRangeSelector

co <- read.csv("f:/Workspace/R/r_stat/data/GDP.csv",
              header=T)
str(co)
ggplot() +
  geom_line(data=co, aes(x=년도, y=한국, colour="green")) +
  geom_line(data=co, aes(x=년도, y=미국, colour="blue")) +
  geom_line(data=co, aes(x=년도, y=중국, colour="red")) +
  ylab("GDP 성장률")

library(reshape2)
gdp_cagr <- melt(co, id="년도")  # convert to long format
str(gdp_cagr)
names(gdp_cagr) <- c("년도", "국가", "rate")

ggplot(data=gdp_cagr,
       aes(x=년도, y=rate, colour=국가)) +
  geom_line(lwd=1) +
  ylab("GDP 성장률")

# No.6
df <-3

m4 <- rep(NA, 1000)
m8 <- rep(NA, 1000)
m32 <- rep(NA, 1000)
m64 <- rep(NA, 1000)
set.seed(9)
for(i in 1:1000){
  m4[i] <-mean(rchisq(4, df))
  m8[i] <-mean(rchisq(8, df))
  m32[i] <-mean(rchisq(32, df))
  m64[i] <-mean(rchisq(64, df))
}

par(mfrow = c(2,2))
hist(m4, xlim=c(0, 8), prob=T , main="표본의 개수 : 4개", 
     xlab="x", ylab="", col="cyan", border = "blue")
x1 <- seq(min(m4), max(m4),length=1000)
y1 <- dnorm(x= x1, mean=df, sd=sqrt(df*2/4))
lines(x1, y1, lty=2, lwd=2, col="red" )

hist(m8, xlim=c(1, 6), ylim=c(0,0.5), prob=T, main="표본의 개수 : 8개",
     xlab="x", ylab="", col="cyan", border = "blue")
x2 <- seq(min(m8), max(m8),length=1000)
y2 <- dnorm(x= x2, mean=df, sd=sqrt(df*2/8))
lines(x2, y2, lty=2, lwd=2, col="red" )

hist(m32, xlim=c(1.5, 4.5), prob=T, main="표본의 개수 : 32개", 
     xlab="x", ylab="", col="cyan", border = "blue")
x3 <- seq(min(m32), max(m32),length=1000)
y3 <- dnorm(x= x3, mean=df, sd=sqrt(df*2/32))
lines(x3, y3, lty=2, lwd=2, col="red" )

hist(m64, xlim=c(2, 4), ylim=c(0,1.35), prob=T, main="표본의 개수 : 64개", 
     xlab="x", ylab="", col="cyan", border = "blue")
x4 <- seq(min(m64), max(m64),length=1000)
y4 <- dnorm(x= x4, mean=df, sd=sqrt(df*2/64))
lines(x4, y4, lty=2, lwd=2, col="red" )

par(mfrow = c(1, 1))
