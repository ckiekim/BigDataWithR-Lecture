install.packages("prob")
library(prob)

tosscoin(1)
rolldie(1)
urnsamples(1:3, size = 2)
urnsamples(1:3, size = 2, replace = T)
urnsamples(c(rep("R", 3), rep("B", 2)), size = 2)
tosscoin(2, makespace = T)
rolldie(2, makespace = T)
(dbinom(2, size = 4, prob = 0.5))
for(i in 0:4) {
  print(dbinom(i, size = 4, prob = 0.5))
}