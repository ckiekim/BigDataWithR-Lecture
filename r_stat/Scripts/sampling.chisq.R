#1 카이제곱 (df=2)
df <- 2
m4 <- rep(NA, 1000)
m16 <- rep(NA, 1000)
m64 <- rep(NA, 1000)
m256 <- rep(NA, 1000)
set.seed(9)
for(i in 1:1000){
  m4[i] <- mean(rchisq(4, df))
  m16[i] <- mean(rchisq(16, df))
  m64[i] <- mean(rchisq(64, df))
  m256[i] <- mean(rchisq(256, df))
}

windows()
par(mfrow = c(2, 2))
sample_size <- 4
hist(m4, xlim=c(0, 8), prob=T, main="표본크기=4", 
     xlab="x", ylab="", col="cyan", border = "blue")
x1 <- seq(min(m4), max(m4),length=1000)
y1 <- dnorm(x=x1, mean=df, sd=sqrt(2*df/sample_size))  # 평균: df, 분산: 2*df
lines(x1, y1, lty=2, lwd=2, col="red" )

sample_size <- 16
hist(m16, xlim=c(0, 6), prob=T, main="표본크기=16", 
     xlab="x", ylab="", col="cyan", border = "blue")
x2 <- seq(min(m16), max(m16),length=1000)
y2 <- dnorm(x=x2, mean=df, sd=sqrt(2*df/sample_size))
lines(x2, y2, lty=2, lwd=2, col="red" )

sample_size <- 64
hist(m64, xlim=c(0.8, 3.2), prob=T, main="표본크기=64", 
     xlab="x", ylab="", col="cyan", border = "blue")
x3 <- seq(min(m64), max(m64),length=1000)
y3 <- dnorm(x=x3, mean=df, sd=sqrt(2*df/sample_size))
lines(x3, y3, lty=2, lwd=2, col="red" )

sample_size <- 256
hist(m256, xlim=c(1.3, 2.7), prob=T, main="표본크기=256", 
     xlab="x", ylab="", col="cyan", border = "blue")
x4 <- seq(min(m256), max(m256),length=1000)
y4 <- dnorm(x=x4, mean=df, sd=sqrt(2*df/sample_size))
lines(x4, y4, lty=2, lwd=2, col="red" )

#2 카이제곱 (df=10)
df <- 10
m4 <- rep(NA, 1000)
m16 <- rep(NA, 1000)
m64 <- rep(NA, 1000)
m256 <- rep(NA, 1000)
set.seed(9)
for(i in 1:1000){
  m4[i] <- mean(rchisq(4, df))
  m16[i] <- mean(rchisq(16, df))
  m64[i] <- mean(rchisq(64, df))
  m256[i] <- mean(rchisq(256, df))
}

windows()
par(mfrow = c(2,2))
sample_size <- 4
hist(m4, xlim=c(0, 20), prob=T, main="표본크기=4", 
     xlab="x", ylab="", col="cyan", border = "blue")
x1 <- seq(min(m4), max(m4),length=1000)
y1 <- dnorm(x=x1, mean=df, sd=sqrt(2*df/sample_size))  # 평균: df, 분산: 2*df
lines(x1, y1, lty=2, lwd=2, col="red" )

sample_size <- 16
hist(m16, xlim=c(5, 15), prob=T, main="표본크기=16", 
     xlab="x", ylab="", col="cyan", border = "blue")
x2 <- seq(min(m16), max(m16),length=1000)
y2 <- dnorm(x=x2, mean=df, sd=sqrt(2*df/sample_size))
lines(x2, y2, lty=2, lwd=2, col="red" )

sample_size <- 64
hist(m64, xlim=c(5, 15), prob=T, main="표본크기=64", 
     xlab="x", ylab="", col="cyan", border = "blue")
x3 <- seq(min(m64), max(m64),length=1000)
y3 <- dnorm(x=x3, mean=df, sd=sqrt(2*df/sample_size))
lines(x3, y3, lty=2, lwd=2, col="red" )

sample_size <- 256
hist(m256, xlim=c(8, 12), prob=T, main="표본크기=256", 
     xlab="x", ylab="", col="cyan", border = "blue")
x4 <- seq(min(m256), max(m256),length=1000)
y4 <- dnorm(x=x4, mean=df, sd=sqrt(2*df/sample_size))
lines(x4, y4, lty=2, lwd=2, col="red" )
par(mfrow = c(1, 1))