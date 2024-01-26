# tibble melanoma
melanoma

#type and scale of all variables
# time  quantitative discrete absolute
# status qualitative discrete nominal
# sex qualitative  discrete nominal
# age quantiative discrete absolute
# year quantiative discrete interval -> weil jahr, datum etc. hat kein nullpunkt
# thickness  quantiative continious ratio
# ulcer qualitative discrete nominal

#c)
melanoma
# change variables to
melanoma <-  melanoma %>% 
  mutate(sex = as.character(sex),
         status = as.character(status),
         ulcer = as.character(ulcer))
#live status
melanoma <- melanoma %>% 
  mutate(live.status = 
           case_when(
             status == 1 ~ "dead",
             status == 3 ~ "dead",
             status == 2 ~ "alive"
           ))
# other variant
melanoma2 <- melanoma %>% 
  mutate(live.status= if_else(status==2, "alive", "dead"))

#d) create contingency table for sex and live.status
cont <- melanoma %>% 
  select(sex, live.status)
cont
cont_tab <- table(cont)
cont_tab
addmargins(cont_tab)
chisq.test(cont_tab)
#e) Evaluate the relative risks to survive at least 3 years 
#   for the variable sex and interpret the values
new_cont <- melanoma %>% select(time, sex, live.status) %>% 
                filter(time > (365*3))
new_cont <-  new_cont %>% select(sex, live.status)
new_cont
new_cont_tab <- table(new_cont)
new_cont_tab
addmargins(new_cont_tab)
new_cont_tab/rowSums(new_cont_tab)
#f)
measures <- melanoma %>% group_by(sex)  %>% summarise(
    min = min(age),
    max = max(age),
    mean = mean(age),
    Q1 = quantile(age,0.25, type = 1),
    Q2 = quantile(age,0.50, type = 1),
    Q3 = quantile(age,0.75, type = 1),
    IQ = Q3-Q1
)
measures
#g) boxplot
boxplot(melanoma$age~melanoma$sex, xlab = "sex", ylab= "age", main="sidebyside",
        names = c("female", "male")
        )
# both boxplots are symmetric -> the median(Q2) is approximately in 
# middle of the box
# male max is higher than female
# male min is higher than female
# there is an extreme value in group of female(outlier) 
#  --> 1.5 times the boxlength away from first or third quartile.

#h) csv file add.data.melanoma.csv
library(readr)
add.data.melanoma <- read_csv("add.data.melanoma.csv")
View(add.data.melanoma)
#i)
# dataset is not tidy
# --> because one column(sex_age_year) stores multiple other variables
# can be fixed by using separate() ! to make it tidy
add.data.melanoma <- add.data.melanoma %>% 
  separate(sex_age_year,c("sex", "age", "year"), sep = "/")
add.data.melanoma

#2)
# a)

# with formula
(factorial(60)/(factorial(15)**4))*(15/60)**60 #0.002140
# with multinom function
dmultinom(c(15,15,15,15), size = 60, prob=c(0.25,0.25,0.25,0.25))
#0.002140

# more than 15 student
pbinom(15,60,1/4,lower.tail = F)
1- pbinom(16,60,1/4)

# P(X > 3)
1 - pbinom(3)
# P(x >= 3)
1- pbinom(2)

dhyper(x = 2,m = 4, n = 6,k = 5)

pnorm(m+0.5,n*p,sqrt(n*p*(1-p)))



      
phyper(q = 2,m = 6, n = 5,k = 5)




#########
# INFERENTIAL STATISTIK
#
n <- 200;m <- 12
# a) show that x= m/n is an unbiased estimator of prevalence
x <- m/n
x
#(c) Determine an upper 95% condence bound for prevalence from
#    the sample data.
phat <-  m/n
alpha <- 0.05
u <- phat+qnorm(1-alpha)*sqrt((phat*(1-phat)/n))
u
# exact
binom.test(x = 12, n = 200, p = phat, 
           alternative = "less", conf.level = 1-0.05)$conf.int

#(d) What is the minimum sample size needed for the upper 95% con-
#dence bound is 0.01 greater than the estimate. Assume that the
#prevalence is <= 0.1 and use a normal approximation of the confi-
#  dence bound.

ceiling((qnorm(1-alpha)/0.01)**2*0.1*(1-0.1))
#minimum sample size = 2435


  

# TASK 5
sample <- c(98.32,97.26,99.85,99.52,95.73,95.56,100.49,98.19,95.16,
            98.26,96.46,100.23,99.76,98.58,97.43)
mu0 <- 100
# a) H0: m0 == 100, H1: m0 != 100
# b) a appropiate test would be t -test because the sd is unknown
# and we are testing mu
# c)
alpha <- 0.05;
t.test(x = sample, mu = mu0, alternative="two.sided",
       conf.level = 1 -alpha)
# pvalue is = 0.0007251 which is much lower than alpha,
# so we are rejecting the null hypothesis
# d)
sample2 <- c(100.14,100.05,96.51,98.70,98.22,101.06,103.55,100.16,
             100.60,102.85,103.15,100.66,102.52,102.09,100.84)
# e)
# H0: mu1 >= mu2 , H1: mu1 < mu 2  
t.test(sample, sample2, alternative="less", paired = F, var.equal =T, 
       conf.level = 1-alpha)
# pvalue = 0.0002228 much lesser than alpha -> reject H0
# f)
var.test(sample, sample2, alternative = "two.sided",
         conf.level = 1 -0.1)
