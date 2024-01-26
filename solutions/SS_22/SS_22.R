# Descriptive 
# a) import
library(readr)
galtonfamilies <- read_csv("D:/Datentransfer/Studium/3.Semester/Statistics/SS_22/GaltonFamilies.csv")
View(GaltonFamilies)
GaltonFamilies
#b)
# family: qualitative discrete nominal
# father (height of father): quantitative continuous ratio
# mother (height of mother): quantitative continuous ratio
# midpartenHeight : quantitative continuous ratio
# children (number of childrens in family): quantitative discrete absolute
# childNum: qualitative discrete ordinal
# gender: qualitative discrete nominal
# childHeight: quantitative continuous ratio

#c) height given in inches. change the values to cm( 1 inch= 2.54cm)
library(tidyverse)
galtonfamilies <- galtonfamilies %>% 
  mutate(father = father*2.54,
         mother = mother*2.54,
         midparentHeight =  midparentHeight*2.54,
         childHeight = childHeight*2.54
         )
galtonfamilies

#d)
heights.fm <- 
  galtonfamilies %>% 
  gather(key = 'type', value = 'height', father,mother) %>% 
  select(type,height)
heights.fm
#e)
measures <- heights.fm %>% 
  group_by(type) %>% 
  summarise(
    n = n(),
    min = min(height),
    max = max(height),
    mean = mean(height),
    median = median(height),
    q1 = quantile(height, 0.25, type=1),
    q2 = quantile(height, 0.5, type=1),
    q3 = quantile(height, 0.75, type=1)
  )
measures
#f)
boxplot(heights.fm$height~heights.fm$type)
#g)
plot(galtonfamilies$midparentHeight,galtonfamilies$childHeight)
reg <- lm(galtonfamilies$childHeight~galtonfamilies$midparentHeight)
reg
abline(reg,col='red')
# alpha

#i)
children <- read_csv("D:/Datentransfer/Studium/3.Semester/Statistics/SS_22/children.csv")
parents <- read_csv("D:/Datentransfer/Studium/3.Semester/Statistics/SS_22/parent.csv")
children
parents
#j)
children <- children %>% full_join(parents, by="...1") %>%  select(child, parent)
children %>% left_join(parents, by ="...1")

galtonfamilies %>% full_join(children, by="...1")


###########


# Probability
# Task 2


# Task 3
mean <- 205; sd <- 15;n <- 49

# a) central limit theorem we assume that the distribution is a normal
# distribution
pnorm(9800+0.5,n*mean, sqrt(n*sd**2))
# b)
qnorm(0.99, n*mean, sqrt(n*sd**2))
# n = 10230
# c)
library(tidyverse)
tibble(
  n  = 1:100,
  p  = pnorm(9800+0.5, n*mean, sqrt(n*sd**2))
) %>% filter(p>=0.90) %>% filter(n==max(n))


#######################


mean <- 1550; sd <- 200;
# a) #P(1500 < X < 1600)
pnorm(1600, mean ,sd)- pnorm(1500, mean ,sd)
#b)
qnorm(c(0.025, 0.975), mean , sd)
# c)
1- pnorm(11000, 7*mean,sqrt(7**2*sd**2))
# d) # P(A-B>0) = A has higher revenue than B
meanA <- 1450; sdA <- 250
1-pnorm(0, meanA-mean, sqrt(sdA**2+sd**2))



# with test statistic
test.stat <- ((sample.mu-mu0)/sd0)*sqrt(n)
# calculate pvalue
# p-value using standard normal distribution
# two-sided
p <- pnorm(-test.stat)+(1-pnorm(test.stat))
# right sided
p <- 1 -pnorm(test.stat)
# left sided
p <-  pnorm(test.stat)
# when sample or sample.mu given:
library(TeachingDemos)
z.test(x = sample.mu, mu = mu0, alternative = "two.sided",
       stdev = sd0, n = sample.size, conf.level = 1 - alpha)





# with test statistic
test.stat <- ((sample.mu-mu0)/sample.sd)*sqrt(n)
# rejection region two sided
l <- -qt(1 - alpha / 2, n-1)
u <-  qt(1 - alpha / 2, n-1)
test.stat < l | test.stat > u 
# reject null hypothesis if true
# rejection region right sided
u <- qt(1-alpha,  n-1)
test.stat > u 
# reject null hypothesis if true
# rejection region left sided
l <- -qt(1-alpha, n-1)
test.stat < l
# reject null hypothesis if true

# calculate pvalue
# p-value using t distribution
# if p value < alpha then reject null hypothesis
# two-sided
p <- pt(test.stat,n-1)+1-pt(test.stat,n-1)
# right sided
p <- 1-pt(test.stat, n-1)
# left sided
p <- pt(test.stat, n-1)
# when sample is given 
t.test(x = sample, mu = mu0, alternative ="two.sided",
       conf.level=1-alpha)




# test statistic
p.hat <-  x / n
test.stat <- (p.hat - p0) / sqrt(p0 * (1 - p0) / n)
# pvalue:
# two sided
p <- pnorm(-test.stat)+(1-pnorm(test.stat))
# right sided
p <- 1 - pnorm(test.stat)
# left sided
p <- pnorm(test.stat)
# with binomtest (exact)
library(TeachingDemos)
binom.test(x = x, n = size, p =p0, alternative = "two.sided",
           conf.level = 1-alpha)



#test
len <- 8.32-5.12
1-(2*(1-pnorm(2.81)))
1-(2*(1-pnorm(len*sqrt(10)/(2*1.8))))
#test

## Inferential
# Task 1
# a)
alpha <- 0.1
n <- ceiling((2*qnorm(1-alpha/2)*sqrt(25)/1.25)**2)
n # 174
# b)
len <- 1.15; n=200;
sd <- 5
1-(2*(1-pnorm(len*sqrt(n)/(2*sd))))
# confidence level = 89.61% 
# 90%
# c) 
# 
n <- 150; alpha <- 0.2; sd <- 5
len <- 2*qnorm(1-alpha/2)*sd/sqrt(n)
len
n <- ceiling((2*qnorm(1-alpha/2)*sqrt(25)/1.046382)**2)
n

# Task 2
#
alpha <- 0.05; mu0 <- 500;
sample.w <- c(512,530,498,540,521,528,505,523)
sample.b <- c(499,500,510,495,515,503,490,511)
sample.wb <- c(512,530,498,540,521,528,505,523,499,500,510,495,515,503,490,511)
# H0: µ >= 500, H1: µ < 500
#t test
t.test(x=sample.wb, mu=mu0, alternative="less",
       conf.level=1-alpha)
# pvalue: 0.997 > alpha --> we are not rejecting h0
# the bakery is gönning us
# H0: µ <= 500, H1: µ > 500 
t.test(x=sample.wb, mu=mu0, alternative="greater",
       conf.level=1-alpha)

# b)
# H0: µ1<=µ2 , H1: µ1 > µ2
t.test(sample.w, sample.b, alternative="greater",
       paired=FALSE, var.equal=T, conf.level=1-0.05)
# reject null hypothesis beacuse pvalue is  0.005755 < 0.05
# frau gönnt
# c) conducting a f.test
# H0: sd1**2 = sd2**2, H1: sd1**2 != sd2**2
var.test(sample.w, sample.b, alternative = "two.sided",
         conf.level = 1-0.1)
#p-value = 0.2282 > alpha so we are not rejecting h0
#