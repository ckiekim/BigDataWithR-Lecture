# 표본 평균 x bar 의 분포
m10 <- rep(NA, 10000)
m40 <- rep(NA, 10000)
set.seed(9)
for( i in 1:10000) {
  m10[i] <- mean(rnorm(10))
  m40[i] <- mean(rnorm(40))
}

options(digits=4)
c(mean(m10), sd(m10))
c(mean(m40), sd(m40))

hist(m10, xlim=c(-1.5, 1.5), main="", 
     xlab="x", ylab="", col="cyan", border="blue")
hist(m40, xlim=c(-1.5, 1.5), main="", 
     xlab="x", ylab="", col="cyan", border="blue")

# 정규분포에서 추출한 표본평균 x bar의 분포
set.seed(9)
n <- 1000
r.1.mean = rep(NA, n)
r.2.mean = rep(NA, n)
for (i in 1:n ) {
  r.1.mean[i] = mean( rnorm(4, mean=3, sd=1) )
  r.2.mean[i] = mean( rnorm(4, mean=170, sd=6) )
}

options(digits=4)
c(mean(r.1.mean), sd(r.1.mean))
c(mean(r.2.mean), sd(r.2.mean))

hist(r.1.mean, prob=TRUE, 
     xlab="표본평균", ylab="밀도", 
     main="", col="orange", border="red")
x1 <- seq(min(r.1.mean), max(r.1.mean), length=1000)
y1 <- dnorm(x=x1, mean=3, sd=(1/sqrt(4)))
lines(x1, y1, lty=2, lwd=2, col="blue")

hist(r.2.mean, prob=TRUE, 
     xlab="표본평균", ylab="밀도", 
     main="", col="orange", border="red")
x2 <- seq(min(r.2.mean), max(r.2.mean), length=1000)
y2 <- dnorm( x=x2, mean=170, sd=(6/sqrt(4)) )
lines(x2, y2, lty=2, lwd=2, col="blue")

# 샘플 갯수가 16개 일때
for (i in 1:n ) {
  r.1.mean[i] = mean( rnorm(16, mean=3, sd=1) )
  r.2.mean[i] = mean( rnorm(16, mean=170, sd=6) )
}

c(mean(r.1.mean), sd(r.1.mean))
c(mean(r.2.mean), sd(r.2.mean))

hist(r.1.mean, prob=TRUE, 
     xlab="표본평균", ylab="밀도", 
     main="", col="orange", border="red")
x1 <- seq(min(r.1.mean), max(r.1.mean), length=1000)
y1 <- dnorm(x=x1, mean=3, sd=(1/sqrt(16)))
lines(x1, y1, lty=2, lwd=2, col="blue")

hist(r.2.mean, prob=TRUE, 
     xlab="표본평균", ylab="밀도", 
     main="", col="orange", border="red")
x2 <- seq(min(r.2.mean), max(r.2.mean), length=1000)
y2 <- dnorm( x=x2, mean=170, sd=(6/sqrt(16)) )
lines(x2, y2, lty=2, lwd=2, col="blue")

# 이항분포에서 추출한 표본평균 x bar의 분포
# 그림 4-6
t <- 10; p <- 0.1; x <- 0:10    # 세미콜론(;) 을 이용하여 한 줄에 여러 명령을 실행할 수 있습니다.
b.p <- dbinom(x, size=t, prob=p)
barplot(b.p, names=x, main="n=10, p=0.1인 이항분포")

# 예제 4-3
set.seed(9)
t <- 10
p <- 0.1
x <- 0:10
n <- 1000
b.2.mean <- rep(NA, n)
b.4.mean <- rep(NA, n)
b.16.mean <- rep(NA, n)
b.64.mean <- rep(NA, n)
b.256.mean <- rep(NA, n)

for(i in 1:n) {
  b.2.mean[i] <- mean( rbinom(2, size=t, prob=p) )
  b.4.mean[i] <- mean( rbinom(4, size=t, prob=p) )
  b.16.mean[i] <- mean( rbinom(16, size=t, prob=p) )
  b.64.mean[i] <- mean( rbinom(64, size=t, prob=p) )
  b.256.mean[i] <- mean( rbinom(256, size=t, prob=p) )
}

options(digits=4)
c(mean(b.2.mean), sd(b.2.mean))
c(mean(b.4.mean), sd(b.4.mean))
c(mean(b.16.mean), sd(b.16.mean))
c(mean(b.64.mean), sd(b.64.mean))
c(mean(b.256.mean), sd(b.256.mean))

hist(b.2.mean, prob=T, xlim=c(0, 4), 
     main="표본 크기 : 2", 
     ylab="", xlab="", col="orange", border="red")
x1 <- seq(min(b.2.mean), max(b.2.mean), length=1000)
y1 <- dnorm( x=x1, mean=1, sd=sqrt(0.9)/sqrt(2) )
lines(x1, y1, lty=2, lwd=2, col="blue")

hist(b.4.mean, prob=T, 
     xlim=c(0, 4), ylim=c(0, 1.2), 
     main="표본 크기 : 4", ylab="", xlab="", 
     col="orange", border="red")
x2 <- seq(min(b.4.mean), max(b.4.mean), length=1000)
y2 <- dnorm( x=x2, mean=1, sd=sqrt(0.9)/sqrt(4) )
lines(x2, y2, lty=2, lwd=2, col="blue")

hist(b.16.mean, prob=T, xlim=c(0, 4), 
     main="표본 크기 : 16", ylab="", xlab="", 
     col="orange", border="red")
x3 <- seq(min(b.16.mean), max(b.16.mean), length=1000)
y3 <- dnorm( x=x3, mean=1, sd=sqrt(0.9)/sqrt(16) )
lines(x3, y3, lty=2, lwd=2, col="blue")

hist(b.64.mean, prob=T, xlim=c(0, 2), 
     main="표본 크기 : 64", ylab="", xlab="", 
     col="orange", border="red")
x4 <- seq(min(b.64.mean), max(b.64.mean), length=1000)
y4 <- dnorm( x=x4, mean=1, sd=sqrt(0.9)/sqrt(64) )
lines(x4, y4, lty=2, lwd=2, col="blue")

hist(b.256.mean, prob=T, xlim=c(0.5, 1.5), 
     main="표본 크기 : 256", ylab="", xlab="", 
     col="orange", border="red")
x5 <- seq(min(b.32.mean), max(b.32.mean), length=1000)
y5 <- dnorm( x=x5, mean=1, sd=sqrt(0.9)/sqrt(256) )
lines(x5, y5, lty=2, lwd=2, col="blue")

# Chi-square 분포
df <- c(1, 3, 5, 10)
x <- seq(0, 20, by=0.01)
chi2.1 <- dchisq(x, df[1])
chi2.3 <- dchisq(x, df[2])
chi2.5 <- dchisq(x, df[3])
chi2.10 <- dchisq(x, df[4])

plot(x, type="n", xlim=c(0, 20), ylim=c(0, 0.3), 
     main="", xlab="x", ylab="", axes=F)
axis(1); axis(2)
lines(x, chi2.1, lwd=2, lty=1, col="black")
lines(x, chi2.3, lwd=2, lty=2, col="red")
lines(x, chi2.5, lwd=2, lty=3, col="blue")
lines(x, chi2.10, lwd=2, lty=4, col="green")
legend("topright", paste("df :", df), lty=1:4, 
       col=c("black","red", "blue", "green"), cex=0.7)

# t 분포
df <- c(1, 2, 8, 30)
x <- seq(-3, 3, by=0.01)
y <- dnorm(x)
t.1 <- dt(x, df=df[1])
t.2 <- dt(x, df=df[2])
t.8 <- dt(x, df=df[3])
t.30 <- dt(x, df=df[4])

par(mar=c(4,2,2,2))
plot(x, y, type="l", lty=1, axes=F, xlab="x", ylab="", col="red")
axis(1)
lines(x, t.1, lty=4, col="black")
lines(x, t.2, lty=3, col="magenta")
lines(x, t.8, lty=2, col="blue")
lines(x, t.30, lty=6, col="green")
legend("topright", paste("df :", df), lty=c(4, 3, 2, 6), 
       col=c("black", "magenta", "blue", "green"), cex=0.7)

# f 분포
df1 <- c(3, 10)
df2 <- c(5, 20)
x <- seq(0, 2, by=0.01)

f3.5 <- df(x, df1[1], df2[1])   # df(x, 3, 5)
f3.20 <- df(x, df1[1], df2[2])  # df(x, 3, 20)
f10.5 <- df(x, df1[2], df2[1])  # df(x, 10, 5)
f10.20 <- df(x, df1[2], df2[2]) # df(x, 10, 20)

plot(x, f3.5, type="l", ylim=c(0, 0.9), lwd=2, 
     axes=F, xlab="x", ylab="")
axis(1)
lines(x, f3.20, lty=2, lwd=2, col="blue")
lines(x, f10.5, lty=3, lwd=2, col="green")
lines(x, f10.20, lty=4, lwd=2, col="magenta")

legend("topright", paste("df :", c("3, 5", "3, 20", "10, 5", "10, 20")),
       col=c("black", "blue", "green", "magenta"), lty=1:4, cex=0.7)

# R function
# 모집단의 분산
options(digits=4)
var.p <- function(x) {
  n <- length(x)
  m <- mean(x)
  num <- sum( (x - m)^2 )
  var <- num / n
  return( var )
}

radius <- c(234, 234, 234, 233, 233, 233, 233, 231, 232, 231)
weight <- c(146.3, 146.4, 144.1, 146.7, 145.2, 144.1, 143.3, 147.3, 146.7, 147.3)

var.p(radius) # 모집단의 분산
var(radius)   # 표본집단의 분산
var.p(weight)
var(weight)

# NA 처리기능 추가
options(digits=4)
var.p2 <- function(x, na.rm=FALSE) {
  if(na.rm == TRUE){
    x <- x[!is.na(x)]
  }
  n <- length(x)
  m <- mean(x, na.rm=na.rm)
  num <- sum( (x - m)^2, na.rm=na.rm )
  denom <- n
  var <- num / denom
  return( var )
}

radius <- c(234, 234, 234, 233, 233, 233, NA, 231, 232, 231)
var.p2(radius)
var.p2(radius, na.rm=TRUE)  # 모집단의 분산
var(radius, na.rm = TRUE)   # 표본집단의 분산

