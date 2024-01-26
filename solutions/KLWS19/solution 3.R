mu <- 115
sd <- 10

# X = tmp of driver

# a) prob. to get a ticket
# P(X > 130)
p <- 1-pnorm(130, mu, sd)

# b) Distr. exp, var
# 1000 cars
# X = # tickets per hour
# X = x1 + x2 + ... + x1000 ~ B(1000, p)
exp <- 1000*p
var <- 1000*p*(1-p)

# c) 
# X ~ N(1000*p, 1000*p*(1-p))

# d)
1-pnorm(100, exp, sqrt(var))

# e)
qnorm(0.9, exp, sqrt(var))
