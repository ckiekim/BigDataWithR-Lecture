library(dplyr)
library(ggplot2)
library(ggiraphExtra)
library(tibble)
library(mapproj)
library(scales)
library(RColorBrewer)
library(readxl)
library(plotly)
library(raster)

# 2009년 미국 카운티별 실업률
unemp <- read.csv("http://datasets.flowingdata.com/unemployment09.csv",
                  header = FALSE, stringsAsFactors = FALSE)
names(unemp) <- c("id", "state_fips", "county_fips", "name", "year",
                  "?", "?", "?", "rate")
unemp$county <- tolower(gsub(" County, [A-Z]{2}", "", unemp$name))
unemp$county <- gsub("^(.*) parish, ..$","\\1", unemp$county)
unemp$state <- gsub("^.*([A-Z]{2}).*$", "\\1", unemp$name)
str(unemp)
county_df <- map_data("county", projection = "albers", parameters = c(39, 45))
str(county_df)
names(county_df) <- c("long", "lat", "group", "order", "state_name", "county")
county_df$state <- state.abb[match(county_df$state_name, tolower(state.name))]
county_df$state_name <- NULL

state_df <- map_data("state", projection = "albers", parameters = c(39, 45))
str(county_df)

choropleth <- merge(county_df, unemp, by = c("state", "county"))
choropleth <- choropleth[order(choropleth$order), ]
str(choropleth)

windowsFonts(malgun = "맑은 고딕")
windowsFonts()
theme_update(text = element_text(family = "malgun"))
windows()

ggplot(choropleth, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = rate), colour = alpha("white", 1 / 2), 
               size = 0.2) +
  geom_polygon(data = state_df, colour = "white", fill = NA) +
  coord_fixed() +
  theme_minimal() +
  ggtitle("2009 미국 카운티별 실업률") +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(),
        plot.title = element_text(family = "malgun", 
                                  face = "bold", hjust = 0.5, 
                                  size = 15, color = "darkblue")) +
  scale_fill_gradient(low="lightcyan1", high="red4")

# 한국 Wifi 분포도
koreawifi <- read.csv("f:/Workspace/R/easy_r/Examples/wifi.csv", stringsAsFactors = FALSE)
str(koreawifi)

korea <- getData('GADM', country='kor', level=2)
korea <- shapefile('f:/Workspace/R/easy_r/Examples/SIG_201804/TL_SCCO_SIG.shp')
str(korea)
dim(korea)
head(korea)

ggplot() + 
  geom_polygon(data=korea, aes(x=long, y=lat, group=group), 
               fill='white', color='black') +
  geom_point(data = koreawifi, 
             aes(x=long, y=lat, color = factor(company),
                 shape = factor(company))) + 
  scale_fill_brewer(palette = "Pastel1") + 
  ggtitle("한국 와이파이 분포도") +
  theme(plot.title = element_text(family = "malgun", 
                                  face = "bold", hjust = 0.5, 
                                  size = 15, color = "darkblue")) +
  xlim(125, 130) +
  ylim(33, 38.6)

wifi <- ggplot() + 
  geom_polygon(data=korea, aes(x=long, y=lat, group=group), 
               fill='white', color='black') +
  geom_point(data = koreawifi, 
             aes(x=long, y=lat, color = factor(company),
                 shape = factor(company))) + 
  scale_fill_brewer(palette = "Pastel1") + 
  ggtitle("한국 와이파이 분포도") +
  theme(plot.title = element_text(family = "malgun", 
                                  face = "bold", hjust = 0.5, 
                                  size = 15, color = "darkblue")) +
  xlim(125, 130) +
  ylim(33, 38.6)
ggplotly(wifi)
