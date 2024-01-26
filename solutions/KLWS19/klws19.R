library(readr)
class_data <- read_delim("D:/Datentransfer/Studium/3.Semester/Statistics/KLWS19/class.data.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(class_data)
#import with read_csv2
class_data <- read_csv2(file = "D:/Datentransfer/Studium/3.Semester/Statistics/KLWS19/class.data.csv")
class_data
#2)

# firstname qualitative discrete nominal
# lastname qualitative discrete nominal
# ins.no qualitative discrete nominal
# canteen.rating qualitative discrete ordinal
# gender qualitative discrete nominal
# age quantitative discrete absolute
# height quantitative continous ratio
# weight  quantitative continous ratio

#3)
class_data <- class_data %>% mutate(
  height = (height*2.54)/100,
  weight = weight*0.45
)

class_data

#4)
class_data <- class_data %>%
  mutate(bmi = weight/(height**2))

# 5)
class_data <- class_data %>% 
  mutate(bmi.category = case_when(
    bmi < 18.5 ~ "underweight",
    bmi < 24.5 & bmi >= 18.5 ~ 'normalweight',
    bmi >= 24.5 & bmi < 30  ~ 'overweight',
    bmi >=  30 ~ 'obesity'
  ))
class_data

# mode(modus) berechnen (zahl die am meisten vor kommt)
# mode function
mode = function(x) {
  return(names(sort(-table(x)))[1])
}
# other version
getmode <- function(v){
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}


# 6)
measures_gender <- class_data %>% group_by(gender) %>% 
  summarise(
    q1 = quantile(height, 0.25, type =1),
    q2 = quantile(height, 0.5, type = 1),
    q3 = quantile(height, 0.75, type = 1 ),
    mean = mean(height),
    min = min(height),
    max = max(height),
    mode = mode(height),
    mode_stefan = getmode(height)
  )
measures_gender
# 7) side  by side boxplot height ~ variable gender
boxplot(class_data$height~class_data$gender, horizontal=T)

# 8)
# mündlich besprochen

# 9) 
bound <- c(0, 18.5, 24.5, 30, max(class_data$bmi))
hist(class_data$bmi, breaks = bound)

# 13)
cont.tab <- chisq.test(x = class_data$gender, y = class_data$bmi.category)$observed
cont.tab


##########
# Inferential
# TASK 1
sample <- c(0.487, 0.522,0.513,0.512,0.514,0.500,0.527,
            0.419, 0.477, 0.432)
# normally distributed
samp.mu <- mean(sample)
m0 <- 0.5
alpha <- 0.05
t.test(sample,mu=m0,alternative="greater",
       conf.level = 1-alpha)$conf.int
# 0.4685552
# zufuß
n <- length(sample)
samp.mu-qt(1-alpha,n-1)*sd(sample)/sqrt(n)
# 0.4685552
#b)
library(TeachingDemos)
sqrt(sigma.test(sample,alternative ="two.sided", conf.level = 0.99)$conf.int)

# c) 
sd <- 0.05
alpha <- 0.05
n <- (2*qnorm(1-alpha/2)*sd/0.05)**2
n
# laenge
2*qnorm(1-alpha/2)*sd/sqrt(16)
# conf level
1-(2*(1-pnorm(0.05*sqrt(n)/2*sd)))


# Task 2
# a) H0: µ >= 100, H1: µ < 100
sample <- c(96.4, 97.64,98.48,97.67, 100.11,95.29,99.8,  98.8,
            100.53, 99.41,
            97.64,101.11,93.43,96.99,97.92)
sd0 <- 2
# b) Conduct a appropiate test
alpha <- 0.05
z.test(x = sample, mu = 100, alternative = "less",
       stdev=sd0, n = length(sample), conf.level = 1-alpha)
# p value = 0.0001014 < alpha --> Reject h0!
