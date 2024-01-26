# a) import
library(readr)
raw_data <- read_csv("D:/Datentransfer/Studium/3.Semester/Statistics/SS_21/testing_covid.csv")
View(testing_covid)
 
raw_data
# b) scales and types
# country: qualitative, discrete, nominal
# country_code: qualitative, discrete, nominal
# year_week: quantiative, discrete, intervall
# level: qualitative, discrete, nominal
# region: qualitative, discrete nominal
# region_name: qualitative, discrete nominal
# new_cases: quantitative, discrete, absolute
# tests_done: quantitative, discrete, abosolute
# population: quantitative, discrete, absolute
# testing_rate: quantitative, continious, absolute
# positivity_rate: quantitative, continious, absolute
# testint_data_soiruce: qualitative, discrete, nominal

# c)
test_germany
test_germany <- raw_data %>% filter(country_code == 'DE') %>%select(year_week,tests_done)
plot(test_germany$tests_done, type='l')

raw_data %>% filter(country_code == 'IT') %>%  group_by(level)
# d)
raw_data %>% filter(year_week >= '2020-W50')
sum_of_tests <- raw_data %>% filter(country_code %in% c('DE','AT','FR','IT')) %>% 
  filter(year_week >='2020-W49' & year_week <= '2020-W53') %>% filter(level == 'national') %>%
  group_by(level) %>% 
  sum(tests_done)

sum_of_tests

sum_of_tests <- sum_of_tests %>% group_by(country_code) %>% summarise(sum_new_cases = sum(new_cases),
                                                                      sum_tests_done = sum(tests_done))

sum_of_tests

# pie plot
pie(sum_of_tests$sum_new_cases/sum_of_tests$sum_tests_done, labels=sum_of_tests$country_code)

# e)
measures_testingrate <- raw_data %>% filter(country_code %in% c('DE', 'FR'), level =='national') %>% 
  group_by(country_code) %>% summarise(
    min= min(testing_rate),
    max= max(testing_rate),
    q1 = quantile(testing_rate, 0.25, type = 1),
    q2 = quantile(testing_rate, 0.5, type = 1),
    q3 = quantile(testing_rate, 0.75, type = 1)
  )
measures_testingrate
# side by side boxplot
data <- raw_data %>% filter(country_code %in% c('DE', 'FR'), level =='national')
data
boxplot(data$testing_rate~data$country_code)


# 7)

#b)
pn <- raw_data %>% filter(level=='national') %>% spread(key= year_week, value = tests_done)
#c)
# it got worse!!!


######################################################################################
# Probability

# Task 1
# a)
# P(T) = 0.3, P(C) = 0.6, P(T AND C) = 0.2
# drinks neither tea nor coffee:
# - P(T U C) =  1 - P(T U C) = 1 - (P(T) + P(C) - P(T AND C))
1-(0.3+0.6-0.2)
# drinks only coffe
# P(C\T) = P(C)-P(C AND T)
0.6 - 0.2
# drinks only tea
# P(T\C) = P(T)-P(C AND T)
0.3 - 0.2

# b)
# P(C|T) = P(C AND T)/P(T)
0.2/0.3

# Task2
# X = number of persons traveling together among the selected persons
# Determine the density, expected value and variance of X
# 1 from hamburg, 1 from berlin, 3 frankfurt, 4 stuttgart
# density function: possible values for x = {0,2,3}
x <- (choose(3,1)+ choose(4,1) + choose(3,1)*choose(4,1)+ choose(3,1)*choose(4,1))
y <- choose(9,3)
    x;y
x0 <- x/y
# P(X = 3)
x3 <- (choose(3,2)*choose(4,1)+choose(4,2)*choose(3,1))/choose(9,3)
# P(X= 2) = 1-P(x=3) - P(x=0)
x2 <- 1-x3-x0
x0+x3+x2
# expected value  --> P(X=0),P(X=2) , P(X = 3), 
# expected value =  0*P(x=0) + 2*P(x=2) + 3*P(x=3)
exp <- 0 * x0+ 2*x2 + 3*x3 
exp
#variance
var <- (0**2*x0+(2**2*x2)+(3**2*x3))-exp**2
var




#### WS 2019
## Probability
##

## unfair die  --> P(X=0) =1
## fair die:
# P(x=0) = 5/6*5/6
# P(x=1) = 5/6*1/6+1/6*5/6
# P(x=2) = 1/6*1/6

qnorm(0.5)
tibble(
  n = 1:1000,
  p = pbinom(n,)
)
sd = 15

var = sd**2
var
1-pnorm(550, mean = 4*123, sd = sqrt(4**2*var))

qnorm(0.95, mean = 100*123, sd = sqrt(100**2*(sd**2)))
library(tidyverse)
tibble(
  n = 100:1000,
  p = pnorm(50000, n*123, sd =sqrt(n**2*(sd**2)))
) %>% filter(p<=0.99) %>% filter(n==max(n))
