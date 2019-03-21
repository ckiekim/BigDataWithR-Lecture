library(ggplot2)
ggplot(data = mpg, aes(x = displ, y = hwy))
ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point()
ggplot(data = mpg, aes(x = displ, y = hwy)) + 
  geom_point() +
  xlim(3, 6)
ggplot(data = mpg, aes(x = displ, y = hwy)) + 
  geom_point() +
  xlim(3, 6) +
  ylim(10, 30)
?ggplot

ggplot(data = mpg, aes(x = cty, y = hwy)) + geom_point()
ggplot(data = midwest, aes(x = poptotal, y = popasian)) +
  geom_point() +
  xlim(0, 500000) +
  ylim(0, 10000) +
  labs(title = "총 인구와 아시아인과의 관계") +
  xlab("총 인구수") +
  ylab("아시아 인구수")
options(scipen = 0)

library(dplyr)
df_drv <- mpg %>% 
  group_by(drv) %>% 
  summarise(mean_hwy = mean(hwy))
df_drv
ggplot(data = df_drv, aes(x = drv, y = mean_hwy)) + geom_col()
ggplot(data = df_drv, aes(x = reorder(drv, -mean_hwy), y = mean_hwy)) + geom_col()
ggplot(data = mpg, aes(x = drv)) + geom_bar()
ggplot(data = mpg, aes(x = hwy)) + geom_bar()
ggplot(data = mpg, aes(x = cty)) + geom_bar()

mpg_suv = mpg %>% 
  filter(class == "suv") %>% 
  group_by(manufacturer) %>% 
  summarise(mean_cty = mean(cty)) %>% 
  arrange(-mean_cty) %>% 
  head(7)
mpg_suv

# 맑은 고딕 폰트 사용
library(extrafont)
windowsFonts()
windowsFonts(malgun = "맑은 고딕")
windowsFonts()
theme_update(text = element_text(family = "malgun"))

ggplot(data = mpg_suv, aes(x = reorder(manufacturer, -mean_cty),
                           y = mean_cty, fill=manufacturer)) +
  geom_col() +
  ggtitle("SUV 자동차의 시내 연비가 높은 7개 회사") +
  xlab("자동차 회사") +
  ylab("시내 연비") +
  theme(plot.title = element_text(family = "malgun", 
                                  face = "bold", hjust = 0.5, 
                                  size = 15, color = "darkblue"))

ggplot(data = mpg, aes(x = class)) + geom_bar()

ggplot(data = economics, aes(x = date, y = unemploy)) +
  geom_line()
head(economics)
ggplot(data = economics, aes(x = date, y = psavert)) +
  geom_line()

ggplot(data = mpg, aes(x = drv, y = hwy)) + geom_boxplot()

ggplot(data = mpg, aes(x = class, y = cty)) + geom_boxplot()
mpg3 <- mpg %>% filter(class %in% c("compact", "subcompact", "suv"))
ggplot(data = mpg3, aes(x = class, y = cty)) + geom_boxplot()
