library(tidyverse)
load(url('http://vulstats.ucsd.edu/data/crime.data.Rdata'))
murder.data = crime.data %>% filter(Crime == 'Murder')
M1 = glm(data = murder.data, Count ~ log10(Population), family = poisson())
summary(M1)


M1 = glm(data = murder.data, 
         Count ~ log(Population), 
         family = poisson())
summary(M1)

betas = coef(M1)

pop = 1e6
exp(log(pop) * betas[2] + betas[1])

pop^betas[2]*exp(betas[1])


predict(M1, 
        data.frame(Population = 1e6), 
        type='response')

M1 = glm(data = murder.data, 
         Count ~ log(Population), 
         family = poisson())

predictions <- data.frame(Population = 10^seq(3, 7, by=0.1)) %>% 
  mutate(log.mean = coef(M1)['(Intercept)'] + 
           coef(M1)['log(Population)']*log(Population)) %>% 
  mutate(mean = exp(log.mean))

predict(M1, 
        predictions)

predict(M1, 
        predictions, 
        type='response')

predictions %>% 
  ggplot(aes(x = Population, y=mean))+
  geom_line()+
  scale_y_log10() + 
  scale_x_log10() + 
  theme_minimal()

predictions %>% 
  mutate(per.capita = mean/Population) %>% 
  ggplot(aes(x=Population, y=per.capita))+
  geom_line()+
  scale_y_log10() + 
  scale_x_log10() + 
  theme_minimal()
