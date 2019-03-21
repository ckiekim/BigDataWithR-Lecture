library(KoNLP)
library(dplyr)
useNIADic()
txt <- readLines("Examples/hiphop.txt")
head(txt)
tail(txt)
library(stringr)
txt <- str_replace_all(txt, "\\W", " ")
extractNoun("대한민국의 영토는 한반도와 그 부속도서로 한다.")
extractNoun("동해물과 백두산이 마르고 닳도록")
nouns <- extractNoun(txt)
str(nouns)
wordcount <- table(nouns)   # 동작 안함
wordcount <- table(unlist(nouns))  # 동작함
str(wordcount)
head(wordcount)
tail(wordcount)
df_word <- as.data.frame(wordcount, stringsAsFactors = F)
head(df_word)
df_word <- rename(df_word, word = Var1, freq = Freq)
df_word <- filter(df_word, nchar(word) >= 2)
top20 <- df_word %>% 
  arrange(-freq) %>% head(20)
top20

library(wordcloud)
library(RColorBrewer)
pal <- brewer.pal(8, "Dark2")
set.seed(1234)

wordcloud(words = df_word$word,
          freq = df_word$freq,
          min.freq = 3,
          max.words = 200,
          random.order = F,
          rot.per = .1,
          scale = c(5, 0.3),
          colors = pal)

windows()
display.brewer.all()

library(extrafont)
windowsFonts(malgun = "맑은 고딕")
windowsFonts()

pal <- brewer.pal(8, "Dark2")
wordcloud(words = df_word$word,
          freq = df_word$freq,
          min.freq = 2,
          max.words = 200,
          random.order = F,
          rot.per = 0.1,
          scale = c(6, 0.3),
          colors = pal,
          family = "malgun")

twitter <- read.csv("Examples/twitter.csv",
                    header = T,
                    stringsAsFactors = F,
                    fileEncoding = "UTF-8")
head(twitter)
str(twitter)
twitter <- rename(twitter,
                  no = 번호,
                  id = 계정이름,
                  date = 작성일,
                  tw = 내용)
twitter$tw <- str_replace_all(twitter$tw, "\\W", " ")
head(twitter$tw)
nouns <- extractNoun(twitter$tw)
wordcount <- table(unlist(nouns))
df_word <- as.data.frame(wordcount, stringsAsFactors = F)
df_word <- rename(df_word,
                  word = Var1,
                  freq = Freq)
df_word <- filter(df_word, nchar(word) >= 2)
top20 <- df_word %>% 
  arrange(-freq) %>% 
  head(20)
top20

library(ggplot2)
theme_update(text = element_text(family = "malgun"))

order <- arrange(top20, freq)$word
ggplot(data = top20, aes(x = word, y = freq)) +
  ylim(0, 2500) +
  geom_col(width = 0.8, fill = "darkblue") +
  coord_flip() +
  scale_x_discrete(limit = order) +
  geom_text(aes(label = freq), hjust = -0.3) +
  ggtitle("상위 20개 단어") +
  ylab("빈도") +
  xlab("단어") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, 
                                  size = 15, color = "darkblue"))

pal <- brewer.pal(8, "Dark2")
wordcloud(words = df_word$word,
          freq = df_word$freq,
          min.freq = 10,
          max.words = 200,
          random.order = F,
          rot.per = 0.1,
          scale = c(6, 0.2),
          colors = pal,
          family = "malgun")
