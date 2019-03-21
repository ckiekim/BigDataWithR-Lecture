# 집단 사이의 비율
mtcars
table(mtcars$cyl, mtcars$am)

mtcars$tm <- ifelse(mtcars$am==0, "automatic", "manual")
table(mtcars$cyl, mtcars$tm)

mtcars$cyl2 <- factor(mtcars$cyl, labels = c("4기통", "6기통", "8기통"))
result <- table(mtcars$cyl2, mtcars$tm)
mosaicplot(t(result), color=c("cyan", "deepskyblue", "blue"),
           xlab="변속기", ylab="실린더 갯수")
demo(colors())

addmargins(result)

chisq.test(result)   # 5 미만 비율이 전체 cell의 20% 이하일 때
fisher.test(result)

# 그림 8-1
x <- seq(0, 15, by=0.01)
dc <- dchisq(x, df=3)

alpha <- 0.05
tol <- qchisq(0.95, df=3)

par(mar=c(0,1,1,1))
plot(x, dc, type="l", axes=F, ylim=c(-0.03, 0.25), xlab="", ylab="")
abline(h=0)
tol.g <- round(tol, 2)
polygon(c(tol.g, x[x>tol.g], 15), c(0, dc[x>tol.g], 0), col="red")
text(0, -0.03, "0", cex=0.8)
text(tol, -0.03, expression(chi[0.05]^{2}==2.14), cex=0.8)

# 멘델의 법칙
x <- c(315, 101, 108, 32)
chisq.test(x, p=c(9, 3, 3, 1)/16)

# 동질성 검정
sns.c <- read.csv("f:/Workspace/R/r_stat/data/snsbyage.csv", 
                  header=T, stringsAsFactors=FALSE)
str( sns.c )

sns.c <- transform(sns.c, age.c = 
                     factor(age, levels=c(1, 2, 3), 
                            labels=c("20대", "30대", "40대")))

sns.c <- transform(sns.c, service.c = 
                     factor(service, levels=c("F", "T", "K", "C", "E"), 
                            ordered=TRUE))

c.tab <- table(sns.c$age.c, sns.c$service.c)
c.tab
(a.n <- margin.table(c.tab, margin=1))
(s.n <- margin.table(c.tab, margin=2))
(s.p <- s.n / margin.table(c.tab))
(expected <- a.n %*% t(s.p))

(o.e <- c.tab-expected)
(t.t <- sum(  (o.e)^2 / expected ))


qchisq(0.95, df=8)

1-pchisq(t.t, df=8)

result <- chisq.test(c.tab) 
result$expected
str(result)
result$p.value

addmargins(result$expected)

# 독립성 검정
data(UCBAdmissions)
UCBAdmissions
ucba.tab <- apply(UCBAdmissions, c(1, 2), sum)
ucba.tab
round(prop.table(ucba.tab, margin=2) * 100, 1)

# 독립성 검정
(a.n <- margin.table(ucba.tab, margin=1))
(g.n <- margin.table(ucba.tab, margin=2))

(a.p <- a.n / margin.table(ucba.tab))
(g.p <- g.n / margin.table(ucba.tab))

(expected <- margin.table(ucba.tab) * (a.p %*% t(g.p)))
addmargins( expected )


## chi-square statistic
o.e <- (ucba.tab - expected)^2 / expected
addmargins(o.e)

chisq.t <- sum(o.e)
chisq.t
qchisq(0.95, df=1)
1-pchisq(112.250, df=1)
1-pchisq(chisq.t, df=1)


chisq.test(ucba.tab)

## continuity correction
o.e2 <- (abs(ucba.tab - expected)-0.5)^2 / expected
sum(o.e2)

chisq.test(ucba.tab, correct=FALSE)


