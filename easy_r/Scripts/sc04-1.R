english <- c(90, 80, 60, 70)
math <- c(50, 60, 100, 20)
df_midterm <- data.frame(english, math)
df_midterm
class <- c(1, 1, 2, 2)
df_midterm <- data.frame(english, math, class)
df_midterm
mean(df_midterm$english)
mean(df_midterm$math)

install.packages("readxl")
library(readxl)
df_exam <- read_excel("excel_exam.xlsx")
View(df_exam)
str(df_exam)
mean(df_exam$english)
mean(df_exam$science)

df_novar <- read_excel("excel_exam_novar.xlsx")
df_novar
df_novar <- read_excel("excel_exam_novar.xlsx", col_names = F)
df_novar

df_sheet <- read_excel("excel_exam_sheet.xlsx", sheet = 3)
df_sheet

df_csv <- read.csv("df_midterm.csv")
df_csv

df_midterm
write.csv(df_midterm, "df_mid.csv")

save(df_midterm, file = "df_midterm.rda")
rm(df_midterm)
df_midterm
load("df_midterm.rda")
df_midterm