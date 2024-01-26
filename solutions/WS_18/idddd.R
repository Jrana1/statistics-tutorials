# TASK 3

#expected val
exp_val <- 3.5*0.2+3*0.35+2*0.15+4*0.3
var <- (3.5**2*0.2+3**2*0.35+2**2*0.15+4**2*0.3) - exp_val**2

exp_val;var
library(tidyverse)
stefan <- tibble(
  x = c(3.5,3,2,4),
  p = c(0.2,0.35,0.15,0.3)
) 
expected <- sum(stefan$x*stefan$p) 
expected

expected2 <- sum(stefan$x**2*stefan$p) 
variance <- expected2-expected**2
expected2
variance

## b)
# X sales revenue
# X ~ N(200*exp_val, 200*sqrt(var))
pnorm(670, 200*exp_val, sqrt(200*var))
      
# c)

qnorm(0.9, mean = 200*exp_val, sd = sqrt(200*var))
qnorm(0.99, mean = 200*exp_val, sd = sqrt(200*var))
