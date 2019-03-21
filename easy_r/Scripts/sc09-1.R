library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)
raw_welfare<-read.spss(file="Examples/Koweps_hpc10_2015_beta1.sav",
                       to.data.frame = T)
welfare <- raw_welfare
head(welfare)
tail(welfare)
View(welfare)
dim(welfare)
str(welfare)
summary(welfare)

welfare <- rename(welfare,
                  sex = h10_g3,
                  birth = h10_g4,
                  marriage = h10_g10,
                  religion = h10_g11,
                  income = p1002_8aq1,
                  code_job = h10_eco9,
                  code_region = h10_reg7)

class(welfare$sex)
table(welfare$sex)
table(is.na(welfare$sex))
welfare$sex <- ifelse(welfare$sex == 1, "남자", "여자")
table(welfare$sex)

class(welfare$income)
summary(welfare$income)
qplot(welfare$income)
qplot(welfare$income) + xlim(0, 1000)
welfare$income <- ifelse(welfare$income %in% c(0, 9999), NA,
                         welfare$income)
table(is.na(welfare$income))
sex_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(sex) %>% 
  summarise(mean_income = mean(income))
sex_income

ggplot(data = sex_income, aes(x = sex, y = mean_income)) +
  geom_col()
library(extrafont)
windowsFonts()
windowsFonts(malgun = "맑은 고딕")
theme_update(text = element_text(family = "malgun"))
ggplot(data = sex_income, aes(x = sex, y = mean_income)) +
  geom_col(aes(fill = factor(sex))) +
  ggtitle("성별에 따른 월급여 평균") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, 
                                  size = 15, color = "darkblue"))

ggplot(data = sex_income, aes(x = sex, y = mean_income, 
                              fill = sex)) +
  geom_col(width = 0.7) +
  ggtitle("성별에 따른 월급여 평균") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, 
                                  size = 15, color = "darkblue"))

class(welfare$birth)
summary(welfare$birth)
qplot(welfare$birth)
table(is.na(welfare$birth))
welfare$birth <- ifelse(welfare$birt == 9999, NA, welfare$birth)
table(is.na(welfare$birth))

welfare$age <- 2015 - welfare$birth + 1
summary(welfare$age)
qplot(welfare$age)

age_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age) %>% 
  summarise(mean_income = mean(income))
head(age_income)

ggplot(data = age_income, aes(x = age, y = mean_income)) +
  geom_line() +
  ggtitle("나이에 따른 월급여 평균") +
  xlab("나이") +
  ylab("평균 급여") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, 
                                  size = 15, color = "darkblue"))

welfare <- welfare %>% 
  mutate(age_gr = ifelse(age < 30, "young",
                         ifelse(age < 60, "middle", "old")))
table(welfare$age_gr)
qplot(welfare$age_gr)

age_gr_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age_gr) %>% 
  summarise(mean_income = mean(income))
age_gr_income

ggplot(data = age_gr_income,
       aes(x = age_gr, y = mean_income, fill = age_gr)) +
  geom_col(width = 0.7) +
  scale_x_discrete(limits = c("young", "middle", "old")) +
  ggtitle("연령대별 월급여 평균") +
  xlab("연령대") +
  ylab("평균 급여") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, 
                                  size = 15, color = "darkblue"))

sex_age_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age_gr, sex) %>% 
  summarise(mean_income = mean(income))
sex_age_income

ggplot(data = sex_age_income,
       aes(x = age_gr, y = mean_income, fill = sex)) +
  geom_col(width = 0.7, position = "dodge") +
  scale_x_discrete(limits = c("young", "middle", "old")) +
  ggtitle("성별, 연령대별 월급여 평균") +
  xlab("연령대") +
  ylab("평균 급여") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, 
                                  size = 15, color = "darkblue"))

sex_age2 <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age, sex) %>% 
  summarise(mean_income = mean(income))
head(sex_age2)
ggplot(data = sex_age2,
       aes(x = age, y = mean_income, col = sex)) +
  geom_line(size = 0.9) +
  ggtitle("성별, 연령대별 월급여 변화 추이") +
  xlab("나이") +
  ylab("평균 급여") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, 
                                  size = 15, color = "darkblue"))

class(welfare$code_job)
table(welfare$code_job)
library(readxl)
list_job <- read_excel("Examples/Koweps_Codebook.xlsx",
                       col_names = T, sheet = 2)
head(list_job)
dim(list_job)
welfare <- left_join(welfare, list_job, id = "code_job")
welfare %>% 
  filter(!is.na(code_job)) %>% 
  select(code_job, job) %>% 
  head(10)

job_income <- welfare %>% 
  filter(!is.na(job) & !is.na(income)) %>% 
  group_by(job) %>% 
  summarise(mean_income = mean(income))
head(job_income)
dim(job_income)

top10 <- job_income %>% 
  arrange(-mean_income) %>% 
  head(10)
top10

ggplot(data = top10,
       aes(x = reorder(job, mean_income), y = mean_income)) +
  geom_col() +
  coord_flip() +
  ggtitle("직업별 급여수준 상위 10") +
  xlab("직업") +
  ylab("평균 급여") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, 
                                  size = 15, color = "darkblue"))

bottom10 <- job_income %>% 
  arrange(mean_income) %>% 
  head(10)
bottom10
ggplot(data = bottom10,
       aes(x = reorder(job, -mean_income), y = mean_income)) +
  geom_col() +
  coord_flip() +
  ylim(0, 850) +
  ggtitle("직업별 급여수준 하위 10") +
  xlab("직업") +
  ylab("평균 급여") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, 
                                  size = 15, color = "darkblue"))

job_male <- welfare %>% 
  filter(!is.na(job) & sex == "남자") %>% 
  group_by(job) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  head(10)
job_male
job_female <- welfare %>% 
  filter(!is.na(job) & sex == "여자") %>% 
  group_by(job) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  head(10)
job_female

ggplot(data = job_male,
       aes(x = reorder(job, n), y = n)) +
  geom_col(width = 0.8, fill = "lightblue") +
  coord_flip() +
  ggtitle("남성 직업 빈도 상위 10") +
  xlab("직업") +
  ylab("돗수") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, 
                                  size = 15, color = "darkblue"))

ggplot(data = job_female,
       aes(x = reorder(job, n), y = n)) +
  geom_col(width = 0.8, fill = "lightpink") +
  coord_flip() +
  ggtitle("여성 직업 빈도 상위 10") +
  xlab("직업") +
  ylab("돗수") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, 
                                  size = 15, color = "darkblue"))

class(welfare$religion)
table(welfare$religion)
welfare$religion <- ifelse(welfare$religion == 1, "예", "아니오")
qplot(welfare$religion)

class(welfare$marriage)
table(welfare$marriage)
welfare$group_marriage <- ifelse(welfare$marriage == 1, "결혼",
                                 ifelse(welfare$marriage == 3, "이혼", NA))
table(welfare$group_marriage)
qplot(welfare$group_marriage)

religion_marriage <- welfare %>% 
  filter(!is.na(group_marriage)) %>%
  group_by(religion, group_marriage) %>% 
  summarise(n = n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group*100, 1))
religion_marriage

religion_marriage <- welfare %>% 
  filter(!is.na(group_marriage)) %>%
  count(religion, group_marriage) %>% 
  group_by(religion) %>% 
  mutate(pct = round(n/sum(n)*100, 1))
religion_marriage

divorce <- religion_marriage %>% 
  filter(group_marriage == "이혼") %>% 
  select(religion, pct)
divorce
ggplot(data = divorce,
       aes(x = religion, y = pct)) +
  geom_col()

class(welfare$age_gr)
table(welfare$age_gr)
table(welfare$religion)
age_gr_rel_marriage <- welfare %>% 
  filter(!is.na(group_marriage) & age_gr != "young") %>% 
  group_by(age_gr, religion, group_marriage) %>% 
  summarise(n=n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group * 100, 1))
age_gr_rel_marriage

df_divorce <- age_gr_rel_marriage %>% 
  filter(group_marriage == "이혼") %>% 
  select(age_gr, religion, pct)
df_divorce
ggplot(data = df_divorce,
       aes(x = age_gr, y = pct, fill = religion)) +
  geom_col(position = "dodge", width = 0.7) +
  ggtitle("연령대 및 종교 유무에 따른 이혼율") +
  xlab("연령대") +
  ylab("이혼율") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, 
                                  size = 15, color = "darkblue"))

list_region <- data.frame(code_region = c(1:7),
                          region = c("서울",
                                     "수도권(인천/경기)",
                                     "부산/경남/울산",
                                     "대구/경북",
                                     "대전/충남",
                                     "강원/충북",
                                     "광주/전남/전북/제주"))
list_region
welfare <- left_join(welfare, list_region, id = "code_region")
welfare %>% select(code_region, region)  %>%  head

region_age_gr <- welfare %>% 
  count(region, age_gr) %>% 
  group_by(region) %>% 
  mutate(pct = round(n / sum(n) * 100, 2))
head(region_age_gr)
ggplot(data = region_age_gr,
       aes(x = region, y = pct, fill = age_gr)) +
  geom_col(width = 0.8) +
  coord_flip()

list_order_old <- region_age_gr %>% 
  filter(age_gr == "old") %>% 
  arrange(pct)
list_order_old
order <- list_order_old$region

ggplot(data = region_age_gr,
       aes(x = region, y = pct, fill = age_gr)) +
  geom_col(width = 0.8) +
  coord_flip() +
  scale_x_discrete(limits = order) +
  ggtitle("지역별 연령대 비율") +
  ylab("백분율") +
  xlab("지역") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, 
                                  size = 15, color = "darkblue"))

region_age_gr$age_gr <- factor(region_age_gr$age_gr,
                               levels = c("old", "middle", "young"))
class(region_age_gr$age_gr)
levels(region_age_gr$age_gr)
ggplot(data = region_age_gr,
       aes(x = region, y = pct, fill = age_gr)) +
  geom_col(width = 0.8) +
  coord_flip() +
  scale_x_discrete(limits = order) +
  ggtitle("지역별 연령대 비율") +
  ylab("백분율") +
  xlab("지역") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, 
                                  size = 15, color = "darkblue"))
