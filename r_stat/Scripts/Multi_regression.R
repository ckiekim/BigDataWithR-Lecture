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

