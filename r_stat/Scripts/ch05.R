# 분산이 다른 두 추정량의 분포
x <- seq(-3, 3, by=0.01)
y <- dnorm(x)
y.1 <- dnorm(x, sd=sqrt(1/3))
y.2 <- dnorm(x, sd=sqrt(7/18))
pnorm(0.1, sd=sqrt(1/3)) - pnorm(-0.1, sd=sqrt(1/3))  # 평균이 0이면 생략 가능
pnorm(0.1, sd=sqrt(7/18)) - pnorm(-0.1, sd=sqrt(7/18))
plot(x, y, type="l", ylim=c(0, 0.8), axes=F, 
     ylab="", lwd=3, col="orange")
lines(x, y.1, col="red", lwd=3)
lines(x, y.2, col="blue", lty=2, lwd=3)
axis(1)

# 예제 5-2, 유효성 모의 실험
options(digits=3)
set.seed(1)
mean.seq <- function (x) {  # (X1 + 2*X2 + 3*X3) / 6 의 평균
  n <- length(x)
  sum <- 0
  n2 <- 0
  for( i in 1:n) {
    newx <- i * x[i]
    sum <- sum + newx
    n2 <- n2 + i    
  }
  return( sum / n2 )
}

y1 <- rep(NA, 1000)
y2 <- rep(NA, 1000)
for(i in 1:1000) {
  smp <- rnorm(3)
  y1[i] <- mean(smp)
  y2[i] <- mean.seq(smp)
}

(n1 <- length(y1[(y1 > -0.1) & (y1 < 0.1)]))
(n2 <- length(y2[(y2 > -0.1) & (y2 < 0.1)]))
data.frame(mean=mean(y1), var=var(y1), n=n1)
data.frame(mean=mean(y2), var=var(y2), n=n2)

windows()
par(mfrow=c(1, 2))
hist(y1, probability = T, xlim=c(-2, 2), ylim=c(0, 0.65), 
     main="(x1+x2+x3)/3", xlab="", ylab="", 
     cex.main=2, col="orange", border="red")
hist(y2, probability = T, xlim=c(-2, 2), ylim=c(0, 0.65), 
     main="(1*x1+2*x2+3*x3)/6", xlab="", ylab="", 
     cex.main=2, col="orange", border="red")
par(mfrow=c(1, 1))

# 모비율에 대한 점추정
library(prob)
n <- 3
smps.all <- rolldie(n)
str( smps.all )
head( smps.all, n=3 )

is.even <- function(x) {
  return(!x%%2)
  # result <- ifelse(x%%2 == 0, T, F)
  # return (result)
}
var.p <- function(x) {
  return( sum((x-mean(x))^2 / length(x))  )
}
p.even <- function(x, s.size=3) {
  return( sum(is.even(x)) / s.size )
}

phat <- apply(smps.all, 1, p.even)

mean(phat)
( p.p <- 0.5 )
var.p(phat)
( p.p * (1 - p.p) / 3 )
sqrt(var.p(phat))

# 신뢰구간, 신뢰수준
par(mar=c(0,1,0,1))
x <- seq(-3, 3, by=0.01)
y <- dnorm(x)
plot(x, y, axes=F, type="l", ylim=c(-0.1, 0.5), xlab="", ylab="")
abline(h=0)
ll <- qnorm(0.025)
ul <- qnorm(0.975)
polygon(c(-3, x[x<ll], ll), c(0, y[x<ll], 0), density=20)
polygon(c(ul, x[x>ul], 3), c(0, y[x>ul], 0), density=20, angle=135)

text(0, 0.2, expression(1-alpha))
text(-2.5, 0.1, expression(plain(P)(Z<z) == over(alpha, 2)), cex=0.7)
text(2.5, 0.1, expression(plain(P)(Z>z) == over(alpha, 2)), cex=0.7)
text(-1.96, -0.02, expression(-z[over(alpha, 2)]), cex=0.8)
text(1.96, -0.02, expression(z[over(alpha, 2)]), cex=0.8)

# 모평균에 대한 95% 신뢰구간
set.seed(9)
n <- 10
x <- 1:100
y <- seq(-3, 3, by=0.01)

smps <- matrix(rnorm(n * length(x)), ncol=n)
head(smps)
xbar <- apply(smps, 1, mean)
se <- 1 / sqrt(10)
alpha <- 0.05
z <- qnorm(1 - alpha/2)
ll <- xbar - z * se
ul <- xbar + z * se

windows()
plot(y, type="n", xlab="표본추출", ylab="z", 
     xlim=c(1, 100), ylim=c(-1.5, 1.5), cex.lab=1.8)
abline(h=0, col="red", lwd=2, lty=2)
l.c <- rep(NA, length(x))
l.c <- ifelse(ll * ul > 0, "red", "black")
arrows(1:length(x), ll, 1:length(x), ul, code=3, 
       angle=90, length=0.02, col=l.c, lwd=1.5)

# 모평균에 대한 95% 신뢰구간(모분산을 모를 때)
ci.t <- function(x, alpha=0.05) {
  n <- length(smp)
  m <- mean(x)
  s <- sd(x)
  t <- qt(1-(alpha/2), df=n-1)
  ll <- m - t * (s / sqrt(n))
  ul <- m + t * (s / sqrt(n))
  ci <- c(1-alpha, ll, m, ul)
  names(ci) <- c("Confidence Level", "Lower limit", "Mean", "Upper limit")
  return( ci )
}

smp <- c(520, 498, 481, 512, 515, 542, 520, 518, 527, 526)
ci.t(smp)
ci.t(smp, 0.1)