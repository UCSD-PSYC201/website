library(tidyverse)

dat <- read_csv('http://vulstats.ucsd.edu/data/duckworth-grit-scale-data/data-coded.csv')

glimpse(dat)

dat %>% count(gender)

dat %>%  count(race)

dat %>% 
  ggplot(aes(fill = race, x=1))+
  geom_bar(position=position_stack())

dat %>% 
  filter(age < 100) %>% 
  ggplot(aes(x=age)) +
  geom_density(fill='blue', bw=0.5)

dat %>% 
  filter(gender %in% c('male', 'female')) %>% 
  ggplot(aes(y=neuroticism, x=gender, fill=gender))+
  geom_violin()+
  stat_summary(fun.data = mean_se, geom="errorbar")

dat %>% 
  filter(gender %in% c('male', 'female')) %>% 
  group_by(gender) %>% 
  summarize(mean.neuroticism = mean(neuroticism),
            se.neuroticism = sd(neuroticism)/sqrt(n())) %>% 
  ggplot(aes(x=gender, fill=gender, y=mean.neuroticism))+
  geom_col()+
  geom_errorbar(aes(ymin = mean.neuroticism - se.neuroticism,
                    ymax = mean.neuroticism + se.neuroticism))
  
dat %>% 
  filter(gender %in% c('male', 'female')) %>% 
  ggplot(aes(x=gender, y=neuroticism, fill=gender))+
  # stat_summary(fun.y = mean, geom='col')+
  stat_summary(fun.data = mean_se, geom='pointrange')+
  coord_cartesian(ylim = c(15, 21))

dat %>% 
  filter(gender %in% c('male', 'female')) %>% 
  filter(age < 100) %>% 
  ggplot(aes(x=age, y=neuroticism, color=gender)) +
  geom_point()+
  geom_smooth(method='lm')+
  facet_grid(race~gender)

dat %>% select(age, gender, extroversion:vocabulary) %>% 
  gather(key = 'measure', 
         value = 'score',
         extroversion:vocabulary) %>%
  filter(gender %in% c('male', 'female')) %>% 
  filter(age < 100) %>% 
  group_by(measure) %>% 
  mutate(score = (score- mean(score))/sd(score)) %>% 
  ungroup() %>% 
  ggplot(aes(x=age, y=score, color=gender))+
  facet_grid(gender ~ measure) +
  scale_color_manual(values = c('male'='mediumorchid4', 
                                'female'='orange'))+
  geom_point(size=0.1, position=position_jitter(width=0, height=0.1))+
  geom_smooth(method='lm', color = 'black')+
  theme_classic()+
  theme(panel.border = element_rect(color='black', fill=NA))
  
  
# effect sizes  
dat %>% 
  filter(gender %in% c('male', 'female')) %>% 
  select(gender, extroversion:vocabulary) %>% 
  pivot_longer(extroversion:vocabulary, names_to = 'measure', values_to = 'score') %>% 
  group_by(gender, measure) %>% 
  summarize(m=mean(score), s=sd(score), n=n()) %>% 
  pivot_wider(names_from = gender, values_from = m:n) %>% 
  mutate(delta.m = (m_male - m_female),
         delta.s = sqrt(s_male^2 + s_female^2),
         d = delta.m/delta.s) 
