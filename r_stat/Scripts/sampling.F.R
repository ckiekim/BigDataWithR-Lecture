# F 분포 (df1 = 3, df2 = 5)
df1 <- 3; df2 <- 5
f4 <- rep(NA, 1000)
f32 <- rep(NA, 1000)
f128 <- rep(NA, 1000)
f512 <- rep(NA, 1000)
set.seed(9)
for(i in 1:1000) {
  f4[i] <- mean(rf(4, df1, df2))
  f32[i] <- mean(rf(32,  df1, df2))
  f128[i] <- mean(rf(128,  df1, df2))
  f512[i] <- mean(rf(512,  df1, df2))
}

#windows()
par(mfrow = c(2, 2))
meanf <- df2 / (df2 - 2)
varf <- (2 * df2^2 * (df1 + df2 - 2)) / (df1 * (df2 - 2)^2 * (df2 - 4))
sample_size <- 4
hist(f4, xlim=c(0, 8), prob=T, main="표본크기=4", 
     xlab="x", ylab="", col="cyan", border = "blue")
x1 <- seq(min(f4), max(f4), length=1000)
y1 <- dnorm(x=x1, mean=meanf, sd=sqrt(varf/sample_size))  
lines(x1, y1, lty=2, lwd=2, col="red" )

sample_size <- 32
hist(f32, xlim=c(0, 5), prob=T, main="표본크기=32", 
     xlab="x", ylab="", col="cyan", border = "blue")
x2 <- seq(min(f32), max(f32), length=1000)
y2 <- dnorm(x=x2, mean=meanf, sd=sqrt(varf/sample_size))
lines(x2, y2, lty=2, lwd=2, col="red" )

sample_size <- 128
hist(f128, xlim=c(0.8, 3), prob=T, main="표본크기=128", 
     xlab="x", ylab="", col="cyan", border = "blue")
x3 <- seq(min(f128), max(f128), length=1000)
y3 <- dnorm(x=x3, mean=meanf, sd=sqrt(varf/sample_size))
lines(x3, y3, lty=2, lwd=2, col="red" )

sample_size <- 512
hist(f512, xlim=c(1.2, 2.5), prob=T, main="표본크기=512", 
     xlab="x", ylab="", col="cyan", border = "blue")
x4 <- seq(min(f512), max(f512), length=1000)
y4 <- dnorm(x=x4, mean=meanf, sd=sqrt(varf/sample_size))
lines(x4, y4, lty=2, lwd=2, col="red" )
par(mfrow = c(1, 1))
