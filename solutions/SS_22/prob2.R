# Probability Task 3
# A large freight elevator can transport a maximum
# of 9800 kg. Suppose a load of cargo containing n boxes must be
# transported via the elevator. Experience has shown that
# the weight of boxes of this type of cargo follows a
# distribution with mean µ = 205 kg and standard
# deviation sd = 15 kg
# Based on this information,

# a) What is the probability that all n=49 boxes
#   can be safely loaded onto the freight elevator
#   and transported?
mean <- 205; sd <- 15
n <- 49
# we assume that the given distribution is
# approximately normally distributed by using
# the central limit theorem.
pnorm(9800+0.5, n*mean, sqrt(n*sd**2))
#0.009940 --> 0.99%

# b) What should be the maximum load capacity of the elevator,
# so that all n=49 boxes can be loaded onto the elevator
# and transpoted with a probability of at least 99%?
qnorm(0.99,n*mean, sqrt(n*sd**2))# 10289.27
# The elevator needs have a maximum load capacity
#  of 10290 

# c) Determine the maximum number of boxes which
# can be loaded and transported with a probability
# of at least 90%?
tibble(
  n = 40:80,
  p = pnorm(9800+0.5, n*mean,sqrt(n*sd**2))
) %>% filter(p >= 0.90) %>% filter(n==max(n))
# n = 47

#gegen rechnung zur überprüfung
n <- 47
qnorm(0.946, n*mean ,sqrt(n*sd**2)) #9800 (kg)
