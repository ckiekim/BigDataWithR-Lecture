# 만 7세 남아 키
data <- read.csv("f:/Workspace/R/r_stat/Data/2010_6th.csv", header=T)
str(data)
tmp <- subset(data, data$나이==7 )
height.p <- tmp$X104.키
height.p
set.seed(9)
height <- height.p[sample(length(height.p), 15)]
height

mean(height)
mu0 <- 1220
sd(height)
sqrt( length(height) )
(mean(height) - mu0) / (sd(height) / sqrt(length(height)))

# 그림 6-4
par(mar=c(0.5,1,1,1))
x <- seq(-3, 3, by=0.001)
y <- dt(x, df=14)
plot(x, y, type="l", axes=F, ylim=c(-0.02, 0.38), 
     main="", xlab="t", ylab="")
abline(h=0)
alpha <- 0.05
ul <- qt(1-(alpha/2), df=14)
ll <- -ul
polygon(c(-3, x[x<ll], ll), c(0, y[x<ll], 0), col=2)
polygon(c(ul, x[x>ul], 3), c(0, y[x>ul], 0), col=2)
text(-2.5, 0.1, expression(plain(P)(T<t) == 0.025), cex=0.7)
text(2.5, 0.1, expression(plain(P)(T>t) == 0.025), cex=0.7)
text(ll, -0.02, expression(-t[0.025]==-2.14), cex=0.8)
text(ul, -0.02, expression(t[0.025]==2.14), cex=0.8)

#그림 6-5의 세 개의 그림을 각각 그립니다.
par(mar=c(0,1,1,1))
windows()
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))
#양쪽검정
x <- seq(-3, 3, by=0.001)
y <- dt(x, df=14)
plot(x, y, type="l", axes=F, ylim=c(-0.02, 0.38), 
     main="", xlab="", ylab="")
abline(h=0)
alpha <- 0.05
ul <- qt(1-(alpha/2), df=14)
ll <- -ul
polygon(c(-3, x[x<ll], ll), c(0, y[x<ll], 0), col=2)
polygon(c(ul, x[x>ul], 3), c(0, y[x>ul], 0), col=2)
text(-2.5, 0.1, expression(plain(P)(T<c[l]) == over(alpha, 2)))
text(2.5, 0.1, expression(plain(P)(T>c[u]) == over(alpha, 2)))
text(ll, -0.02, expression(c[l]==-t[alpha / 2]), cex=1.2)
text(ul, -0.02, expression(c[u]==t[alpha / 2]), cex=1.2)

# (왼쪽) 한쪽검정
x <- seq(-3, 3, by=0.001)
y <- dt(x, df=14)
plot(x, y, type="l", axes=F, ylim=c(-0.02, 0.38), 
     main="", xlab="", ylab="")
abline(h=0)
alpha <- 0.05
ll <- qt(alpha, df=14)
polygon(c(-3, x[x<ll], ll), c(0, y[x<ll], 0), col=2)
text(-2.5, 0.1, expression(plain(P)(T<c[l]) == alpha))
text(ll, -0.02, expression(c[l]==-t[alpha]), cex=1.2)

# (오른쪽) 한쪽검정
x <- seq(-3, 3, by=0.001)
y <- dt(x, df=14)
plot(x, y, type="l", axes=F, ylim=c(-0.02, 0.38), 
     main="", xlab="", ylab="")
abline(h=0)
alpha <- 0.05
ul <- qt(1-alpha, df=14)
polygon(c(ul, x[x>ul], 3), c(0, y[x>ul], 0), col=2)
text(2.5, 0.1, expression(plain(P)(T>c[u]) == alpha))
text(ul, -0.02, expression(c[u]==t[alpha]), cex=1.2)

par(mfrow=c(1, 1))

# 그림 6-6을 위한 R코드
xbar <- mean(height)
mu0 <- 1220
s <- sd(height)
n <- length(height)
t.t <- (xbar - mu0) / (s / sqrt(n-1))

x <- seq(-3, 3, by=0.001)
y <- dt(x, df=14)
plot(x, y, type="l", axes=F, ylim=c(-0.02, 0.38), 
     main="", xlab="t", ylab="")
abline(h=0)
alpha <- 0.05
ul <- qt(1-(alpha/2), df=14)
ll <- -ul
polygon(c(-3, x[x<ll], ll), c(0, y[x<ll], 0), col=2)
polygon(c(ul, x[x>ul], 3), c(0, y[x>ul], 0), col=2)

arrows(t.t, 0.05, t.t, 0, length=0.1)
text(t.t, 0.07, paste("t=", round(t.t, 3)))

text(ll, -0.02, expression(-t[0.025]==-2.14))
text(ul, -0.02, expression(t[0.025]==2.14))

# 그림 6-7, 검정통계량으로부터 구한 유의확률(p-value)
par(mar=c(0,1,1,1))
x <- seq(-3, 3, by=0.001)
y <- dt(x, df=14)
plot(x, y, type="l", axes=F, ylim=c(-0.02, 0.38), 
     main="", xlab="t", ylab="")
abline(h=0)
alpha <- 0.05
ul <- qt(1-(alpha/2), df=14)
ll <- -ul

polygon(c(-3, x[x<ll], ll), c(0, y[x<ll], 0), col=2)
polygon(c(ul, x[x>ul], 3), c(0, y[x>ul], 0), col=2)
text(ll, -0.02, expression(-t[0.025]==-2.14))
text(ul, -0.02, expression(t[0.025]==2.14))

t.t <- 0.727
p.value <- 1 - pt(t.t, df=14)
polygon(c(t.t, x[x>t.t], 3), c(0, y[x>t.t], 0), density=20, angle=45)
text(t.t, -0.02, paste("t=", round(t.t, 3)))

text(1.7, 0.2, paste("P(T>t)=",round(p.value, 3)))
arrows(1.7, 0.18, 1.5, dt(1.5, df=14), length=0.1)

#단일 모집단의 평균검정 (1-sample T)
data <- read.table("http://www.amstat.org/publications/jse/datasets/babyboom.dat.txt", header=F)
str( data )
names(data) <- c("time", "gender", "weight", "minutes")
tmp <- subset(data, gender==1)
(weight <- tmp[[3]])

barx <- mean(weight)
s <- sd(weight)
(n <- length(weight))
h0 <- 2800
( t.t <- (barx - h0) / (s / sqrt(n)) )

alpha <- 0.05
( c.u <- qt(1-alpha, df=n-1) )
( p.value <- 1 - pt(t.t, df=n-1) )

t.test(weight, mu=2800, alternative="greater")

# 도표 작성 : 그림 6-8
par(mar=c(0,1,1,1))
x <- seq(-3, 3, by=0.001)
y <- dt(x, df=n-1)
plot(x, y, type="l", axes=F, ylim=c(-0.02, 0.38), 
     main="", xlab="t", ylab="")
abline(h=0)

polygon(c(c.u, x[x>c.u], 3), c(0, y[x>c.u], 0), col=2)
text(c.u, -0.02, expression(t[0.05]==1.74))
text(1.8, 0.2, expression(alpha == 0.05), cex=0.8)
arrows(1.8, 0.18, 1.8, 0.09, length=0.05)

polygon(c(t.t, x[x>t.t], 3), c(0, y[x>t.t], 0), density=20, angle=45)
text(t.t, -0.02, paste("t=", round(t.t, 3)), pos=4)
text(2.65, 0.1, expression(plain(P)(T>2.233) == 0.0196), cex=0.8)
arrows(2.7, 0.08, 2.5, 0.03, length=0.05)

# 모비율 검정
tmp <- read.table("f:/Workspace/R/r_stat/Data/restitution.txt", header=T)
rel <- ifelse(tmp$rst < 0.4134 | tmp$rst > 0.4374, 1, 0)

n <- length(rel)
nos <- sum(rel)
sp <- nos / n
hp <- 0.1
(z <- (sp - hp) / sqrt( ( hp*(1-hp) )/n ) )

alpha <- 0.05
( c.u <- qnorm(1-alpha) )
( p.value <- 1 - pnorm(z) )

prop.test(nos, n, p=0.1, alternative="greater", correct=FALSE)

# 도표 출력 : 그림 6-9
par(mar=c(0,1,1,1))
x <- seq(-3, 3, by=0.001)
y <- dnorm(x)
plot(x, y, type="l", axes=F, ylim=c(-0.02, 0.4), 
     main="", xlab="z", ylab="")
abline(h=0)

polygon(c(c.u, x[x>c.u], 3), c(0, y[x>c.u], 0), col=2)
text(c.u, -0.02, expression(z[0.05]==1.645))

polygon(c(z, x[x>z], 3), c(0, y[x>z], 0), density=20, angle=45)
text(z, -0.02, paste("z=", round(z, 3)))
text(1.2, 0.3, paste("P(Z>z)=", round(p.value, 3)), cex=0.8)
