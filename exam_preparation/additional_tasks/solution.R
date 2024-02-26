
getwd()
setwd('path')
# import data
df<-read_csv('corona.csv') %>% as_tibble()
df %>% slice(1:5)
# show columns
names(df)
# type, scale
## continent, location -> qualitative, nonimal
## year, month, day->  quantitative, interval
# (c) new cases and new deaths: october, germany
df %>% filter(month==10 & location=='Germany') %>% select(new_cases,new_deaths)
# Number of new cases and new deaths in every country per month

df %>% group_by(location,month) %>% summarise(new_cases=sum(new_cases),new_deaths=sum(new_deaths)) # n() = count

# Max, min, mean and median of new cases and new deaths per month

df %>% group_by(month) %>% summarise(
               mean_cases=mean(new_cases,na.rm = T),
               mean_death=mean(new_deaths,na.rm=T)
               
)

## 7 Day incidences in germany 2020
## in der Übung

#Linear regression new deaths = a + b * new cases in Germany
#(a) Parameter a, b
#(b) Scatterplot
#(c) Coefficient of correlation, coefficient of determination
#(d) Prediction for 20000 new cases
#(e) Interpretation of the coefficients
#(a)
data.ger<-df %>% filter(location=='Germany')
model<-lm(data.ger$new_deaths~data.ger$new_cases)

a<-model$coefficients[1]
b<-model$coefficients[2]
a;b

#(b) scatterplot
plot(data.ger$new_deaths,data.g$new_cases)

#(c) Coefficient of correlation, coefficient of determination

cor(data.ger$new_deaths,data.ger$new_cases)
# coefficient of determination
cor(data.ger$new_deaths,data.ger$new_cases)**2
# #(d) Prediction for 20000 new cases
new_death=a+b*2000
new_death
#(e) Interpretation of the coefficients
a;b

# new_death=a+b*new_cases
# if new_cases=0
# new_death=a -> a=17.53 noise
# new_death = 17.53 + 0.12 * new_cases 
# new_death = 17.53 + 0.012 * new_cases
# if new_cases increases x unit then new_death increases x times 0.012
# b=slope/steigung
# how to draw regression line ?
plot(data.ger$new_deaths,data.ger$new_cases)
abline(lm(data.ger$new_deaths~data.ger$new_cases))


# Import csv-file “corona march 2020.csv”; Why is the dataset not tidy?
#  Make the dat set tidy.

df2<-read_csv('corona_march_2020.csv')
df2 %>% head()

## columns
names(df2)
# not tidy beacuse the columns 1:31 are actually values
# make the dataset tidy

# mutate  
   df2 %>% gather(key='day',value='ratio',4:34) %>%  # still not tidy
      separate(col='ratio',into = c('new_cases','new_deaths'),sep = '/') 
# now tidy 

   
   ################################################################################
   # Probability
   ################################################################################
   # Task 1
   # 100 Students in the graduating classes can choose between friday, saturday and 
   # sunday for their graduating ceremony. If all students choose one day and the
   # preferences of the days are
   # friday: 0.4, saturday: 0.5, sunday: 0.1
   # are equally popular among the students, what is the probability that 
   # binomial, multinomial, und normal
   
   # (a) more than 40 students sign up for saturday
   # P(X>=40), X~B(n=100,0.5)
   1- pbinom(q = 40,size=100,prob = 0.5)
   
   # b) 30 students sign up for friday, 50 students sign up for saturday,
   #    20 students sign up for sunday
   # multinomial distribution
   
   (factorial(100)*(0.5)**50*(0.4)**30*(0.1)**20)/(factorial(50)*factorial(30)*factorial(20))
   # r function
   dmultinom(c(50,30,20),size = 100,prob = c(0.5,0.4,0.1))
   # c) that the 20th student is first student choosing sunday
   (0.5+0.4)**19*0.1
   # (d) wird in der Übung erklärt :)
   
   
