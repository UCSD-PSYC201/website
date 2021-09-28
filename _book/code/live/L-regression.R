library(tidyverse)
load(url('http://vulstats.ucsd.edu/data/cal1020.cleaned.Rdata'))

glimpse(cal1020)

# correlation, covariance, regression slope for..
# speed ~ age

cor.test(cal1020$age, cal1020$speed.mph)
cor.test(cal1020$speed.mph, cal1020$age)

cov(cal1020$speed.mph, cal1020$age)
cal1020 %>% select(speed.mph, age) %>% cov()

speed.age = summary(lm(data = cal1020, speed.mph ~ age))
speed.age$coefficients[2,1]

speed.age$coefficients['age','Estimate']

# confidence interval on mean speed of 60 year olds. and a single 60 year old.

model = lm(data = cal1020, speed.mph ~ age)

predict.lm(model, 
           data.frame(age=60), 
           interval = 'confidence', 
           level = 0.95)

predict.lm(model, 
           data.frame(age=c(10, 60)), 
           interval = 'prediction', 
           level = 0.95)


Betas = coefficients(model)
Betas['(Intercept)']+Betas['age']*60

cal1020 %>% ggplot(aes(x=age, y=speed.mph))+geom_point()+geom_smooth(method='lm')


