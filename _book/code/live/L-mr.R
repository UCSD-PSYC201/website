library(tidyverse)

data <- readr::read_tsv('http://vulstats.ucsd.edu/data/bodyfat.data2.txt')

data <- data %>% select(-density)

data <- data %>% filter(height > 36) 

glimpse(data)


data = data %>% 
  mutate(BMI = (weight/2.2)/(height*0.0254)^2,
         WeightToHeight = weight/height,
         H2A = height/abdomen)


m <- lm(data = data, bf.percent ~ BMI)
m2 <- lm(data = data, bf.percent ~ WeightToHeight)
m3 <- lm(data = data, bf.percent ~ H2A)

m.all = lm(data = data, 
           bf.percent ~ .)
m.0 = lm(data = data, bf.percent~1)

m.step = step(m.0, 
     direction = 'forward', 
     scope=formula(m.all))

anova(m.all)


predict.lm(m3, 
           newdata = data.frame(H2A = c(0.6, 0.75, 0.9)), 
           interval = 'prediction')



predict.lm(m, 
           newdata = data.frame(BMI = 25), 
           interval = 'confidence')

predict.lm(m2, 
           newdata = data.frame(WeightToHeight = 2), 
           interval = 'prediction')
lm()





data = data %>% 
  mutate(BMI = (weight/2.2)/(height*0.0254)^2,
         WHR = abdomen / hip,
         F2W = forearm / wrist,
         T2K = thigh/knee) 

data %>% 
  cor() %>% 
  as_tibble(rownames = 'X1') %>% 
  pivot_longer(-X1, names_to = 'X2', values_to = 'r') %>% 
  ggplot(aes(x=X1,y=X2,fill=r))+
  geom_tile()+
  scale_fill_gradient2()+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5))
