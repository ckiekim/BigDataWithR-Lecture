# 그림 9-1
set.seed(9)
rvnorm <- function(r) {
  x <- rnorm(50, 0, 1)
  y <- rnorm(50, r*x, sqrt(1-r^2))
  return(cbind(x,y))
}

par(mfrow=c(1, 3), mar=c(2, 2, 2, 1), oma=c(0,0,0,0))

r1 <- rvnorm(0.8)
plot(r1, main="r=0.8")
abline(lm(r1[,2] ~ r1[,1]), col="red")

plot(rvnorm(0), main="r=0")
abline(h=0, col="red")

r3 <- rvnorm(-0.8)
plot(r3, main="r=-0.8")
abline(lm(r3[,2] ~ r3[,1]), col="red")

# 상관계수
hf <- read.table("http://www.randomservices.org/random/data/Galton.txt",  
                 header=T, stringsAsFactors = FALSE)
str(hf)
hf$Gender <- factor(hf$Gender, levels=c("M", "F"))
hf.son <- subset(hf, Gender=="M")
hf.son <- hf.son[c("Father", "Height")]
str(hf.son)

f.mean <- mean(hf.son$Father)
s.mean <- mean(hf.son$Height)
cov.num <- sum( (hf.son$Father-f.mean) * (hf.son$Height - s.mean) )
(cov.xy <- cov.num / (nrow(hf.son) - 1))
cov(hf.son$Father, hf.son$Height) 
(r.xy <- cov.xy / (sd(hf.son$Father) * sd(hf.son$Height)))

# R 함수를 이용한 상관계수
cor(hf.son$Father, hf.son$Height)

# 그림 9-2
par(mfrow=c(1, 1), mar=c(4, 4, 1, 1))
plot(Height~Father, pch=16, data=hf.son, 
     xlab="아버지의 키(인치)", ylab="아들의 키(인치)")
plot(Height~Father, data=hf.son, 
     xlab="아버지의 키(인치)", ylab="아들의 키(인치)")
plot(jitter(Height)~jitter(Father), data=hf.son, 
     xlab="아버지의 키(인치)", ylab="아들의 키(인치)")
linear.model <- lm(Height~Father, data=hf.son)
abline(linear.model, col="red", lwd=2)

# 그림 9-3
# 자료수집 방법은 블로그(http://randstat.tistory.com)를 통해 설명드리겠습니다.
pt <- read.csv("f:/Workspace/R/r_stat/data/pm10.traffic.accident.csv",
               header=T)
plot(pt$ta ~ pt$pm10, xlab="미세먼지(PM10)", ylab="교통사고 발생건수")
cor(pt$pm10, pt$ta)

library(ggplot2)
ggplot(pt, aes(pm10, ta)) + 
  geom_point(colour="blue", size=3) + 
  theme_bw() + 
  xlab("미세먼지(PM10)") + ylab("교통사고 발생건수")

# 아버지와 아들 키 자료로부터 회귀계수 추정
hf$Gender <- factor(hf$Gender, levels=c("M", "F"))
hf.son <- subset(hf, Gender=="M")
hf.son <- hf.son[c("Father", "Height")]

mean.x <- mean(hf.son$Father)
mean.y <- mean(hf.son$Height)

sxy <- sum((hf.son$Father - mean.x)*(hf.son$Height - mean.y))
sxx <- sum((hf.son$Father - mean.x)^2)

( b1 <- sxy / sxx )
( b0 <- mean.y - b1 * mean.x )


# lm() 함수 이용 
out <- lm(Height ~ Father, data=hf.son)
summary(out)

# 그림 9-4
hf$Gender <- factor(hf$Gender, levels=c("M", "F"))
hf.son <- subset(hf, Gender=="M")
hf.son <- hf.son[c("Father", "Height")]

out <- lm(Height ~ Father, data=hf.son)

mean.y <- mean(hf.son$Height)

par(mar=c(4, 4, 1, 1))
plot(Height~Father, pch=21, data=hf.son, 
     xlim=c(75, 76), ylim=c(68, 75), 
     xlab="아버지의 키(인치)", ylab="아들의 키(인치)")
abline(h=mean.y, lwd=2)
abline(lm(Height~Father, data=hf.son), lty=3)

lines(c(75.5, 75.5), c(mean.y, out$fitted.values[hf.son$Father==75.5][1]))
lines(c(75.5, 75.5), 
      c(out$fitted.values[hf.son$Father==75.5][1], 
        min(hf.son$Height[hf.son$Father==75.5])), lty=3, col="red", lwd=2)

text(75.8, 70, expression(bar(y)))
arrows(75.8, 69.7, 75.8, 69.3, length=0.08)

text(75.8, 71, expression(hat(y)))
arrows(75.8, 71.3, 75.8, 72, length=0.08)

text(75.6, 70.5, expression(y-bar(y)))
arrows(75.55, mean.y, 75.55, min(hf.son$Height[hf.son$Father==75.5]), angle=90, code=3, length=0.05)


text(75.4, 72.5, expression(y-hat(y)), col="red")
arrows(75.45, 
       out$fitted.values[hf.son$Father==75.5][1], 75.45, 
       min(hf.son$Height[hf.son$Father==75.5]), 
       angle=90, code=3, length=0.05, col="red", lty=3)

text(75.4, 70, expression(hat(y)-bar(y)))
arrows(75.45, out$fitted.values[hf.son$Father==75.5][1], 
       75.45, mean.y, angle=90, code=3, length=0.05)

# 회귀분석
hf$Gender <- factor(hf$Gender, levels=c("M", "F"))
hf.son <- subset(hf, Gender=="M")
hf.son <- hf.son[c("Father", "Height")]

out <- lm(Height ~ Father, data=hf.son)
anova(out)

summary(out)

no <- par(no.readonly = TRUE)
par(mfrow=c(2,2))
plot(out)
par(no)


# 그림 9-5
no <- par(no.readonly = TRUE)
par(mar=c(2,2,2,1))
plot(Height~Father, data=hf.son, main="", 
     xlab="아버지의 키(인치)", ylab="아들의 키(인치)", 
     ylim=c(65, 75))
abline(out, lwd=1.5)
ci <- predict(out, interval="confidence")
prd <- predict(out, interval="predict")
lines(hf.son$Father, ci[,2], lty=3, lwd=1.5, col="red")
lines(hf.son$Father, ci[,3], lty=3, lwd=1.5, col="red")
par(no)

# 잔차 분석
hf$Gender <- factor(hf$Gender, levels=c("M", "F"))
hf.son <- subset(hf, Gender=="M")
hf.son <- hf.son[c("Father", "Height")]

out <- lm(Height ~ Father, data=hf.son)
out2 <- lm(dist ~ speed, data=cars)

# 그림 9-6
no <- par(no.readonly = TRUE)
par(mfrow=c(1, 2), mar=c(2, 2, 2, 3))
plot( hf.son$Father, residuals(out), xlab="residuals", ylab="")
abline(h=0, col="red", lty=2)
plot( cars$speed, residuals(out2), xlab="residuals", ylab="" )
abline(h=0, col="red", lty=2)
par( no )

# 그림 9-7
dev.off()
no <- par(no.readonly = TRUE)
par(mfrow=c(1, 2), mar=c(2, 2, 2, 3))
qqnorm(residuals(out), main="")
qqline(residuals(out), lty=2, col="red")
qqnorm(residuals(out2), main="")
qqline(residuals(out2), lty=2, col="red")
par( no )

# 정규성 검정
shapiro.test(residuals(out2))


# Polynomial Regression
women
View(women) # height(inch), weight(lbs)

# 신장에 따른 몸무게
plot(weight~height, data=women)
fit <- lm(weight~height, data=women)
abline(fit, col="red", lwd=2)

summary(fit)
cor.test(women$weight, women$height)

par(mfrow=c(2,2))
plot(fit) # 정규성(2-x), 독립성, 선형성(1-x), 등분산성(3-o)
par(mfrow=c(1,1))

fit2 <- lm(weight~height + I(height^2), data=women)
plot(weight~height, data=women)
lines(women$height, fitted(fit2), col="green", lwd=2)

summary(fit2)
par(mfrow=c(2,2))
plot(fit2) # 정규성(2-o), 독립성, 선형성(1-o), 등분산성(3-o)
par(mfrow=c(1,1))

fit3 <- lm(weight~height + I(height^2) + I(height^3), data=women)
plot(weight~height, data=women)
lines(women$height, fitted(fit3), col="orange", lwd=2)

summary(fit3)
par(mfrow=c(2,2))
plot(fit3) # 정규성(2-x), 독립성, 선형성(1-o), 등분산성(3-o)
par(mfrow=c(1,1))

# 다중회귀분석
state.x77
head(state.x77)
states <- as.data.frame(state.x77[,c("Murder","Population",
                                     "Illiteracy","Income","Frost")])

fit <- lm(Murder ~ Population+Illiteracy+Income+Frost, data=states)
summary(fit)
par(mfrow=c(2,2))
plot(fit)
par(mfrow=c(1,1))

fit1 <- lm(Murder ~ ., data=states)
summary(fit1)

fit2 <- lm(Murder ~ Population+Illiteracy, data=states)
summary(fit2)

# AIC
AIC(fit1, fit2) # 값이 적을수록 좋은 모델

# Backward stepwise regression, Forward stepwise regression
