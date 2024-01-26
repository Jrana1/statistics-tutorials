# Inferential 
# Task 4
var <- 25;
sd <- sqrt(var)
# normally distributed with unkown µ

# a) How large must n be chosen at least, so that a confidence interval for µ
# to the level 0.9 has a length not greater than 1.25
alpha <- 0.1
len <- 1.25
n <-  (2*qnorm(1-alpha/2)*sd/len)**2
n # = 173.15 ~ 174

# b) What is the confidence level, if the confidence interval for µ
#    has a length of 1.15 for n = 200;
n <- 200; len <- 1.15
conf.level <- 1-(2*(1-pnorm(len*sqrt(n)/(2*sd))))
conf.level # = 0.8961 ~ 0.90 approx. 90%

# c) What is the length of a confidence interval for µ, given n=150
#    and a confidence level of 0.8?
len <- 2*qnorm(1-alpha/2)*sd/sqrt(n)
len

# Task 5
# A small bakery sells cookies in packages of 500 g. The cookies
# are handmade and the packaging is either done by the baker
# himself or his wife. A sample of the weight of packages
# bought on 16 different day is :
# weight(wife)  | 512, 530, 498, 540, 512, 528, 505, 523
# weight(bakery)| 499, 500, 510, 495, 515, 503, 490, 511
wife <- c(512, 530, 498, 540, 512, 528, 505, 523);
bakery <- c(499, 500, 510, 495, 515, 503, 490, 511)
# a) You trust the bakery and assume that you will be sold 
# packages with a weight of at least 500g.
# Check your assumption with a suitable statistical 
# test at the 5% level. State the null hypothesis and 
# the alternative and determine the p-value of the test.
# Does the data confirm your conjecture?

# H0: µ >= 500; H1: µ < 500
alpha <- 0.05; mean <- 500
# We are conducting a T-Test because the sd is unknown (normal model)
sample <- c(512, 530, 498, 540, 512, 528, 505, 523,
            499, 500, 510, 495, 515, 503, 490, 511)
t.test(x = sample, mu = mean, alternative ="less",
       conf.level = 1-alpha)
# we are not rejecting h0 because pvalue much higher
# than alpha!

# b)The baker's wife is always very friendly in the store. 
# Therefore, you believe that she is more generous in selling 
# the cookies than her husband. Check your assumption with a 
# suitable statistical test. State it null hypothesis and the 
# alternative and determine the p-value of the test. Does the
# data confirm the conjecture at a 5 percent level?
# Assume that the variances of the weights are identical
# for the baker and his wife.
alpha <- 0.05;
# H0: µ1 <= µ2, H1: µ1 > µ2
# We are conducting a two sample t-test because we have two samples
# and we assume that the variances are identical
t.test(wife, bakery, alternative = "greater", paired=F, var.equal =T,
       conf.level=1-alpha)
# we reject h0 because p-value is less than alpha
# So we can assume that the wife is more generous

#(c) Is the assumption in b) that the variances are equal justified?
#  Check this with an appropriate statistical test at the 10% level.
#  State it null hypothesis and the alternative and determine the p-value 
#  of the test.
# H0: sd1**2 = sd2**2, H1: sd1**2 != sd2**2
var.test(wife, bakery, alternative ="two.sided",
         conf.level = 1-0.1)
# we are not rejecting h0 
# so we can assume that the variances are equal


1-(2*(1-pnorm((8.32-5.12)*sqrt(10)/(2*1.8))))


n <- 250; phat <- 0.7
l <- phat-qnorm(1-alpha/2)*sqrt(phat*(1-phat)/n)
u <- phat+qnorm(1-alpha/2)*sqrt(phat*(1-phat)/n)
l;u
