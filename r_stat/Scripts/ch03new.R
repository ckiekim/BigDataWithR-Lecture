library(prob)
tosscoin(1)
rolldie(1)
urnsamples(1:3, size=2)   # 비복원 추출, 3C2
urnsamples(1:3, size=2, replace=T)  # 복원 추출
urnsamples(c( rep("R", 3), rep("B", 2)), size=2)
tosscoin(2, makespace=T)

# figure 3.6
x <- c(0, 1, 2)
px <- c(1/4, 2/4, 1/4)
barplot(px, names=x, xlab="앞면이 나오는 횟수(x)", ylab="확률(P[X=x])")

# 확률 변수의 평균(기댓값)과 분산
x <- c(0, 1, 2)
px <- c(1/4, 2/4, 1/4)
EX <- sum( x * px )
EX
VX <- sum(x^2 * px) - EX^2
VX

# 이항 분포
n <- 6
p <- 1/3
x <- 0:n

(dbinom(2, size=n, prob=p))
(dbinom(4, size=n, prob=p))
(px <- dbinom(x, size=n, prob=p))
plot(x, px, type="s", xlab="성공횟수(x)", ylab="확률(P[X=x])", main="B(6, 1/3)", lwd=2, col="red")
plot(x, px, type="h", xlab="성공횟수(x)", ylab="확률(P[X=x])", main="B(6, 1/3)", lwd=2, col="red")
barplot(px, names=x, xlab="성공의 횟수(x)",
        ylab="확률(P[X=x])",
        main="B(6, 1/3)")

(pbinom(2, size=n, prob=p))
(dbinom(0, size=n, prob=p)+dbinom(1, size=n, prob=p)+dbinom(2, size=n, prob=p))
(pbinom(4, size=n, prob=p))
(pbinom(4, size=n, prob=p) - pbinom(2, size=n, prob=p))

(qbinom(0.1, size=n, prob=p))
(qbinom(0.5, size=n, prob=p))

(rbinom(10, size=n, prob=p))

# 이항분포의 평균 및 분산
(ex <- sum( x * px ))
(ex2 <- sum( x^2 * px ))
(varx <- ex2 - ex^2)

# 야구 3할 타자
n <- 10
p <- 0.3
x <- 0:10
px <- dbinom(x, size = n, prob = p)
barplot(px, names=x, xlab="안타 횟수(x)",
        ylab="확률(P[X=x])",
        main="B(10, 0.3)")

# 정규 분포
#그림 3.12
x <- seq(-4, 4, by=0.01)
p <- dnorm(x, mean=0, sd=1)
#jpeg("./output/3.12.jpg", width=600, height=400, quality=100)
#par(mar=c(4,4,0,0))
plot(x, p, type="l",  xlab="x", ylab="P(X=x)", lwd=2, col="red")
#dev.off()

# 그림 3.13
set.seed(1)
x <- rnorm(10000, mean=170, sd=4)
hist(x, breaks=seq(150, 190, 1), right=F, freq=F)

options(digits=3)
mu <- 170
sigma <- 6
ll <- mu - 3*sigma
ul <- mu + 3*sigma

x <- seq(ll, ul, by=0.01)
nd <- dnorm(x, mean=mu, sd=sigma)
plot(x, nd, type="l", xlab="x", ylab="P(X=x)", lwd=2, col="red")

h182 <- 1 - pnorm(182, mean=mu, sd=sigma)
h182

pnorm(mu, mean=mu, sd=sigma)
pnorm(158, mean=mu, sd=sigma)
pnorm(180, mean=mu, sd=sigma) - pnorm(160, mean=mu, sd=sigma)

qnorm(0.25, mean=mu, sd=sigma)
qnorm(0.5, mean=mu, sd=sigma)
qnorm(0.75, mean=mu, sd=sigma)

options(digits=5)
set.seed(9)
smp <- rnorm(400, mean=mu, sd=sigma)
c(mean(smp), sd(smp))
hist(smp, prob=T, 
     main="N(170, 6^2)으로부터 추출한 표본의 분포(n=400)", 
     xlab="", ylab="", col="white", border="black")
lines(x, nd, lty=2, lwd=2, col="red")

# 90%, 95%
options(digits = 4)
mu <- 0
sigma <- 1

(p0.05 <- qnorm(0.05, mean=mu, sd=sigma))
(p0.025 <- qnorm(0.025, mean=mu, sd=sigma))

pnorm(1.645) - pnorm(-1.645)
pnorm(1.96) - pnorm(-1.96) 

