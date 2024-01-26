library(tidyverse)
# a) import
library(readr)
melanoma <- read_csv("D:/Datentransfer/Studium/3.Semester/Statistics/KL21/melanoma.csv")
View(melanoma)

# b)
# type and sclaes of all variables:

# time quantitative, discrete, ratio scale
# status qualitative, discrete, nominal scale
# sex  qualitative, discrete ,nominal scale
# age  quantitative, discrete, absolute
# year quantitative, discrete, intervall
# thickness quantitative, continious, ratio scale
# ulcer qualitative, discrete, nominal

# c)
melanoma <- melanoma %>% 
  mutate( sex = if_else(sex == 1,'male', 'female')) %>% 
  mutate(status = case_when(
    status == 1 ~ 'melanoma' ,
    status == 2 ~ 'alive',
    status == 3 ~ 'other reason' 
  )) %>% 
  mutate(ulcer = if_else(ulcer==1,'present','absent')) %>% 
  mutate(live.status = if_else(status=='alive','alive','dead'))

melanoma

# d) cont table for sex and live.status
chitest <- chisq.test(melanoma$sex, melanoma$live.status)
cont.tab <- chitest$observed
#melanoma$sex alive dead
#   female    91   35
#   male      43   36
cont.tab

# e) Evaluate the relative risks to survive at least 3 years for the variable sex
#  and interpret the values
tabl <- melanoma %>% filter(time >=(365*3)) %>% group_by(sex)

cont.tab2 <- chisq.test(tabl$sex, tabl$live.status)$observed
#tabl$sex alive dead
#  female    91   19  91+19
#  male      42   15  42+15
#  y | x
cont.tab2
# relative risks:1.71
#relative risks for each group
x <- (91/(91+19))
y <- (42/(42+15))
# relative risk ratio
x/y

91
# other way:
rel.risk <- (cont.tab2[1,1]*cont.tab2[2,2]) / (cont.tab2[1,2]*cont.tab2[2,1])
rel.risk
# this means females have higher risk to die
# frauen überlebenschance ist 1.71 mal höher als männer
# f) summary
measures.age <- melanoma %>% group_by(sex) %>% 
  summarise(min = min(age),
            max = max(age),
            mean = mean(age),
            q1 = quantile(age, 0.25, type=1),
            q2 = quantile(age, 0.5, type=1),
            q3 = quantile(age, 0.75, type=1),
            iqr = IQR(age, type = 1))

measures.age
#g) side by side boxplot
boxplot(melanoma$age~melanoma$sex)
# extreme values in the group of females in the
# both boxplots seem to be symmetric --> the median is in the center of the box
# looking at the iqr of both groups we can say the group of males has a higher spread
# the maximum of males is higher then the females
# the minimum is lower in females

# h)
library(readr)
add_data_melanoma <- read_csv("D:/Datentransfer/Studium/3.Semester/Statistics/KL21/add.data.melanoma.csv")
View(add_data_melanoma)
add.data.melanoma <- add_data_melanoma

add.data.melanoma
#i)
# obviously not tidy there are multiple values in one column (sex_age_year)
# the values are not atomic


# Probability
# Task 2 XING
# a) exactly 15 student register for every of the four projects
dmultinom(c(15,15,15,15),60,c(0.25,0.25,0.25,0.25))
# b) more than 15 students sign up for project P1 --> binomial
1-pbinom(15,60,1/4)

# Task 3
# 11% of the registered do not show up
# total pcs = 10*22 == 220 seats
# total of 240 first year students
n <- 240
pnorm(220,n*0.89, sqrt(n*0.89*(1-0.89)))
#b)
qnorm(0.99, 240*0.89, sqrt(n*0.89*(1-0.89)))
#c)
library(tidyverse)
tibble(
  n = 220:240,
  p = pnorm(220, n*0.89, sqrt(n*0.89*(1-0.89)))
) %>% filter(p>=0.99) %>% filter(n==max(n))
