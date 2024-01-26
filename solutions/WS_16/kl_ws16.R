# probability
# 2)
# n = 10, p= 1/10
# X = # of trials to open front door
# Method A = without replacement
# P(X) ~  H(1,1,10)
dhyper(x = 1:10, m = 1, n = 9, 1:10)
phyper(q = 1, m = 1, n = 9, k = 10)
library(tidyverse)
tibble(
  n= 1:10,
  p_hyp= (choose(1,1)*choose(10-1, n-1))/choose(10,1)
)
dhyper(1,1,9,1:10)
# Method B = with replacement
dgeom(x = 1:10,prob = 1/ 10)


# 3)

mean <- 115; sd <- 10

# a) find the probability that a drive will get a ticket
p <- 1-pnorm(130, mean, sd)
# b) X counts the number of tickets per hours
# X ~ B(1000,p) 
n <-  1000
# distribution, expeceted value, variance
# expected:
exp <- n*p
exp
var <- n*p*(1-p)
var
# c) X ~ N(1000*p, 1000*p*(1-p))

# d) P(X >100)
1-pnorm(100, exp, sqrt(var))
# e)
qnorm(0.9, exp, sqrt(var))
