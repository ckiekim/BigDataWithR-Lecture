rm(list=ls())
install.packages("ggiraphExtra")
library(ggiraphExtra)
head(USArrests)
tail(USArrests)
View(USArrests)
str(USArrests)

library(tibble)
crime <- rownames_to_column(USArrests, var="state")
crime$state <- tolower(crime$state)
str(crime)

library(ggplot2)
install.packages("maps")
states_map <- map_data("state")
str(states_map)

install.packages("mapproj")
windowsFonts()
windowsFonts(malgun = "맑은 고딕")
windowsFonts()
theme_update(text = element_text(family = "malgun"))
library(gridExtra)
m <-ggChoropleth(data = crime,
             aes(fill = Murder, map_id = state),
             map = states_map) +
  ggtitle("미국 주별 살인범죄 분포","(단위: 인구 10만명당 건수)") +
  xlab("경도") +
  ylab("위도") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, 
                                  size = 15, color = "darkblue"),
        plot.subtitle = element_text(hjust = 1.0))
r <- ggChoropleth(data = crime,
             aes(fill = Rape, map_id = state),
             map = states_map) +
  ggtitle("미국 주별 강간범죄 분포","(단위: 인구 10만명당 건수)") +
  xlab("경도") +
  ylab("위도") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, 
                                  size = 15, color = "darkblue"),
        plot.subtitle = element_text(hjust = 1.0))
a <- ggChoropleth(data = crime,
             aes(fill = Assault, map_id = state),
             map = states_map) +
  ggtitle("미국 주별 강도범죄 분포","(단위: 인구 10만명당 건수)") +
  xlab("경도") +
  ylab("위도") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, 
                                  size = 15, color = "darkblue"),
        plot.subtitle = element_text(hjust = 1.0))

u <- ggChoropleth(data = crime,
             aes(fill = UrbanPop, map_id = state),
             map = states_map) +
  ggtitle("미국 주별 도시비율 분포") +
  xlab("경도") +
  ylab("위도") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, 
                                  size = 15, color = "darkblue"))
windows()
grid.arrange(m, r, a, u, ncol=2)

ggChoropleth(data = crime,
             aes(fill = Murder, map_id = state),
             map = states_map,
             interactive = T)

install.packages("stringi")
install.packages("devtools")
devtools::install_github("cardiomoon/kormaps2014")
library(kormaps2014)

str(changeCode(korpop1))
library(dplyr)
korpop1 <- rename(korpop1,
                  pop = 총인구_명,
                  name = 행정구역별_읍면동)

str(changeCode(kormap1))

ggChoropleth(data = korpop1,
             aes(fill = pop, map_id = code, tooltip = name),
             map = kormap1) +
  ggtitle("우리나라 광역시,도별 인구 분포") +
  xlab("경도") +
  ylab("위도") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, 
                                  size = 15, color = "darkblue"))

ggChoropleth(data = korpop1,
             aes(fill = pop, map_id = code, tooltip = name),
             map = kormap1,
             interactive = T)

str(changeCode(tbc))
ggChoropleth(data = tbc,
             aes(fill = NewPts, map_id = code, tooltip = name),
             map = kormap1,
             interactive = T)

devtools::install_github("cardiomoon/Kormaps")
require(Kormaps)
require(tmap)
qtm(kormap1)
remove.packages("rdgal")
install.packages("rdgal")
