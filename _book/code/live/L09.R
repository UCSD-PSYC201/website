library(tidyverse)

load(url('http://vulstats.ucsd.edu/data/cal1020.cleaned.Rdata'))
glimpse(cal1020)



m = lm(data = cal1020, speed.mph ~ age)
summary(m)
predict.lm(m, newdata = data.frame(age=60), 
           interval = 'confidence')
predict.lm(m, newdata = data.frame(age=60), 
           interval = 'prediction')

cal1020 %>% 
  ggplot(aes(age, speed.mph))+
  geom_point()+
  geom_smooth(method='lm')


cal1020 %>% 
  filter(age==60) %>% 
  summarize(m= mean(speed.mph), sd(speed.mph))



predict.lm(model, newdata)


summary(lm(data = cal1020, speed.mph ~ sex))

males = cal1020 %>% filter(sex=='male') %>% pull(speed.mph)
females = cal1020 %>% filter(sex=='female') %>% pull(speed.mph)


test = t.test(males, mu = 6)

str(test)
test$stderr

test







