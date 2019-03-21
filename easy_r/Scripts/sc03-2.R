c(T, T) & c(T, F)   # TRUE FALSE
c(T, T) && c(T, F)  # TRUE

# Factor
sex <- factor("m", c("m", "f"))
sex
nlevels(sex)
levels(sex)
levels(sex)[1]
levels(sex) <- c("male", "female")
sex
factor(c("m", "m", "f"), c("m", "f"))
factor(c("m", "m", "f"))
ordered("a", c("a", "b", "c"))
factor("a", c("a", "b", "c"), ordered = TRUE)

# Vector
(x <- c(1, 2, 3, 4, 5))
x <- c(1, 2, 3, 4, 5)
x
(x <- c("1", 2, "3"))
(x <- c(TRUE, 2L, FALSE))   # 1 2 0
(y <- c(1L, 2, 3.5))
(z <- c(1L, 2.5, "3.5"))
c(1, 2, 3, c(1, 2, 3))
x <- c(1, 3, 4)
names (x) <- c("kim", "seo", "park")
x
(x <- c(1, 3, 4))
names(x)
x[2]
x[-2]
x[1:2]
x[c(1,3)]
length(x)
NROW(x)
names (x) <- c("kim", "seo", "park")
x["seo"]

identical(c(1, 2, 3), c(1, 2, 3))
identical(c(1, 2, 3), c(1, 2, 100))
"a" %in% c("a", "b", "c")
"d" %in% c("a", "b", "c")
x <- c(1, 2, 3, 4, 5)
x+1
10-x
c(1, 2, 3) == c(1, 2, 100)
c(1, 2, 3) != c(1, 2, 100)
union (c("a", "b", "c"), c("a", "d"))
intersect (c("a", "b", "c"), c("a", "d"))
setdiff (c("a", "b", "c"), c("a", "d"))    # 차집합
setequal (c("a", "b", "c"), c("a", "d"))
setequal (c("a", "b", "c"), c("a", "b", "c", "c"))
seq(1, 10, 3)
x <- c(2, 4, 6, 8, 10)
1:NROW(x)
seq_along(x)

rep(1:2, times=5)
rep(1:2, each=5)
rep(1:2, each=5, times=2)

# List
(x <- list(name="foo", height=70))
(x <- list(name="foo", height=c(1, 3, 5)))
list (a=list(val=c(1, 2, 3)), b=list(val=c(1, 2, 3, 4)))
x[1]
x$height
x$name

# Matrix
matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow=3)
matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), ncol=3)
matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow=3, byrow=T)
matrix(1:9, nrow=3,
       dimnames=list(c("r1", "r2", "r3"), c("c1", "c2", "c3")))
(x <- matrix(1:9, ncol=3))
dimnames(x)<-list(c("r1", "r2", "r3"), c("c1", "c2", "c3"))
x
x <- matrix(1:9, ncol=3)
rownames(x) <- c("r1", "r2", "r3")
rownames(x)
x
x[2,2]
x[1,]
x[,3]
x[1:2,]
x[-3,]
x[c(1, 3), c(1, 3)]
(x <- matrix(1:9, nrow=3,
   dimnames=list(c("r1", "r2", "r3"), c("c1", "c2", "c3"))))
x["r1", ]
x <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow=3)
x + 3
10 - x
3 * x
x + x
x - x
x * x
x %*% x
(x <- matrix(1:6, nrow=3))
t(x)
x %*% t(x)
(x <- matrix(c(1, 2, 3, 4), ncol=2))
solve(x)
x %*% solve(x)
nrow(x)
NROW(x)
ncol(x)
(x <- matrix(1:6, nrow=3))
dim(x)
(x <- matrix(1:6, ncol=3))
dim(x)

# Array
array(1:12, dim=c(3, 4))
(x <- array(1:12, dim=c(2, 2, 3)))
x[,,3]
x[1,,]
x[1, 2, 3]
dim(x)

# Data Frame
(d <- data.frame(x=c(1, 2, 3, 4, 5), y=c(2, 4, 6, 8, 10)))
str(d)
(d <- data.frame(x=c(1, 2, 3, 4, 5),
                  y=c(2, 4, 6, 8, 10),
                  z=c('M', 'F', 'M', 'F', 'M')))
str(d)
d$x
d$z
d$x <- 6:10
d$w <- c("A", "B", "C", "D", "E")
d
str(d)

(x <- data.frame(1:3))
colnames(x) <- c('val')
x
rownames(x) <- c('a', 'b', 'c')
x

d <- data.frame(x=c(1, 2, 3, 4, 5), y=c(2, 4, 6, 8, 10))
d
d$x
d[1, ]
d[1, 2]
d[c(1, 3), 2]
d[-1, -2]
d[, c("x", "y")]
d[, c("x")]
d[, c("x"), drop=FALSE]

(d <- data.frame(a=1:3, b=4:6, c=7:9))
d[, names(d) %in% c("b", "c")]
d[, !names(d) %in% c("a")]
d <- data.frame(x=1:1000, y=1000:1, z=1000:1999)
head(d)
tail(d)
head(d, 10)
tail(d, 10)
View(d)

# 타입 판별
class(c(1, 2))
class(matrix(c(1, 2)))
class(data.frame(x=c(1, 2), y=c(3, 4)))
str(c(1, 2))
str(list(c(1,2)))
is.factor(factor(c("m", "f")))

x <- c("a", "b", "c")
str(x)
as.factor(x)
as.character(as.factor(x))
x <- matrix(1:9, ncol=3)
df <- as.data.frame(x)
df
data.frame(list(x=c(1, 2), y=c(3, 4)))
as.factor(c("m", "f"))
factor(c("m", "f"), levels=c("m", "f"))
