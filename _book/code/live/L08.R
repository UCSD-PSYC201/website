library(tidyverse)

spsp = read_csv(url('http://vulstats.ucsd.edu/data/spsp.demographics.cleaned.csv'))

glimpse(spsp)

# is male/female independent of grad/undergrad/regular?

spsp %>% 
  filter(gender %in% c('Male', 'Female'),
         stage %in% c('Regular Member', 'Grad', 'Undergrad')) %>% 
  select(gender, stage) %>% 
  table() %>% 
  chisq.test()

# 
tmp = spsp %>% 
  filter(gender %in% c('Male', 'Female'),
         stage %in% c('Regular Member', 'Grad', 'Undergrad')) %>% 
  select(gender, stage) %>% 
  mutate_all(as.factor)

chisq.test(tmp$gender, tmp$stage)

#  make a plot of that thing above.
spsp %>% 
  filter(gender %in% c('Male', 'Female'),
         stage %in% c('Regular Member', 'Grad', 'Undergrad')) %>% 
  select(gender, stage) %>% 
  table()

spsp %>% 
  filter(gender %in% c('Male', 'Female'),
         stage %in% c('Regular Member', 'Grad', 'Undergrad')) %>% 
  select(gender, stage) %>% 
  ggplot(aes(x=stage, fill=gender))+
  geom_bar(position='fill')+
  ylab('Proportion')

           