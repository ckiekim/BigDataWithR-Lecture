if (!TRUE) {
  print("TRUE")
  print("hello")
} else {
  print("FALSE")
  print("world")
}

x <- c(1, 2, 3, 4, 5)
ifelse(x %% 2 == 0, "even", "odd")
for (i in 1:10) {
  print(i)
}
i <- 1
while (i <= 10) {
  print(i)
  i <- i + 1
}

i <- 0
while (i < 10) {
  i <- i + 1
  if (i %% 2 != 0) {
    next
  }
  print(i)
}

i<-1
while (i<=10) {
  if (i %% 2 == 0) {
    print(i)    
  }
  i <- i+1
}

i<-1
repeat {
  if (i %% 2 == 0) {
    print(i)    
  }
  if (i >= 10) {
    break;
  }
  i <- i+1
}

# Operator
1:5 * 2 + 1
x <- c(1, 2, 3, 4, 5)
x + 1
x + x
x == x
x == c(1, 2, 3, 5, 5)
c(T, T, F) & c(T, F, F)
c(T, T, F) | c(T, F, F)
sum(x)
mean(x)
median(x)
x <- c(1, 2, 3, 4, 5)
ifelse(x %% 2 == 0, "even", "odd")

(d <- data.frame(x=c(1, 2, 3, 4, 5), y=c("a", "b", "c", "d", "e")))
d[c(TRUE, FALSE, T, F, T),]
d[d$x %% 2 == 0, ]
mean(df_midterm[df_midterm$class == 1,]$english)

# NA
NA + TRUE
NA + 1
sum(c(1, 2, 3, NA))   # NA
sum(c(1, 2, 3, NA), na.rm = TRUE)   # 6
(x <- data.frame(a=c(1, 2, 3), b=c("a", NA , "c"), c=c("a", "b", NA)))
na.fail(x)
na.omit(x)
na.exclude(x)
na.pass(x)

# Function
fibo <- function(n) {
  if (n == 1 || n == 2) {
    return(1)
  }
  return(fibo(n-1) + fibo(n-2))
}
fibo(5)

f <- function(x, y) {
  print(x)
  print(y)
}
f(2, 3)
f(y=2, x=3)

f <- function(...) {
  args <- list(...)
  for (a in args) {
    print(a)
  }
}
f("3")
f("3", "4")
f(5:10, 1:4, c("a", "b"))

f <- function(x, y) {
  print(x)
  print(y)
}
g <- function(z, ...) {
   print(z)
   f(...)
}
g(1, 2, c(3, 4, 5))

# Scope
n <- 1
f <- function() {
  print(n)
}
f()
n <- 2
f()

n <- 100
f <- function() {
  n <- 1
  print(n)
}
f()

ls()
rm(list = ls())
f <- function() {
  print(n)
}
f()
f <- function() {
  n <- 1
}
f()
n

f <- function(df2) {
  df2$a <- c(1, 2, 3)
}
df <- data.frame(a <- c(4, 5, 6))
f(df)
df