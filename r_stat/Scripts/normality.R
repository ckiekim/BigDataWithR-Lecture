# 정규성 검정
install.packages("UsingR", dependencies = TRUE)
library(UsingR)
str(cfb)

# Shapiro-Wilk normality test
shapiro.test(cfb$INCOME)

# Histogram and Kernel Density Plot 을 이용한 정규성 확인
hist(cfb$INCOME, breaks=100)

hist(cfb$INCOME, freq=FALSE, breaks=100, 
     main="Kernel Density Plot of cfb$INCOME")
lines(density(cfb$INCOME), col="blue", lwd=3)

# 분위수-분위수 그림 Q-Q plot을 이용한 정규성 확인
qqnorm(cfb$INCOME)
qqline(cfb$INCOME)

shapiro.test(cfb$AGE)
shapiro.test(cfb$DEBT)
