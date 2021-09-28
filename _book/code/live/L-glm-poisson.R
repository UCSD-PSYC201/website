library(tidyverse)
load(url('http://vulstats.ucsd.edu/data/crime.data.Rdata'))

glimpse(crime.data) 

# (how) do rates differ across types of crime?

crime.data %>% 
  filter(Crime == 'Assault') %>% 
  glm(data = ., Count ~ log(Population), family=poisson())

M <- glm(data = crime.data, Count ~ Crime, family=poisson()) 

crime.data %>% 
  group_by(Crime) %>% 
  summarize(mean(Count))

M1 <- glm(data = crime.data, Count ~ log(Population) + Crime, family=poisson()) 


prediction <- expand.grid(Population = 10^seq(3, 7, by=0.01),
            Crime = unique(as.character(crime.data$Crime))) 
prediction$mean.count = predict(M1, newdata = prediction, type='response')

prediction %>% 
  ggplot(aes(x=Population, y=mean.count, color=Crime))+
  geom_line()+
  scale_x_log10()+
  scale_y_log10()

prediction %>% 
  ggplot(aes(x=Population, y=100000*mean.count/Population, color=Crime))+
  geom_line()+
  scale_x_log10()+
  scale_y_log10()+
  ylab('Expected number of crimes per 100k people')

summary(M1)
anova(M1, test='LRT')

# (how) does crime scaling with population change for different crimes?

M2 <- glm(data = crime.data, Count ~ log(Population)*Crime, family=poisson()) 
summary(M2)

prediction <- expand.grid(Population = 10^seq(3, 7, by=0.01),
                          Crime = unique(as.character(crime.data$Crime))) 
prediction$mean.count = predict(M2, newdata = prediction, type='response')

prediction %>% 
  ggplot(aes(x=Population, y=mean.count, color=Crime))+
  geom_line()+
  scale_x_log10()+
  scale_y_log10()

prediction %>% 
  ggplot(aes(x=Population, y=100000*mean.count/Population, color=Crime))+
  geom_line()+
  scale_x_log10()+
  scale_y_log10()+
  ylab('Expected number of crimes per 100k people')

crime.data = crime.data %>% 
  mutate(Centered.Population = exp(log(Population)-mean(log(Population))))

M3 <- glm(data = crime.data, Count ~ log(Centered.Population)*Crime, family=poisson()) 
summary(M3)

summary(M2)

crime.data %>% summarize(exp(mean(log(Population))))

prediction %>% 
  ggplot(aes(x=Population, y=100000*mean.count/Population, color=Crime))+
  geom_line()+
  geom_vline(xintercept = 6333, color='gray')+
  scale_x_log10()+
  scale_y_log10()+
  ylab('Expected number of crimes per 100k people')

