rm(list=ls())
library(tidyverse)

grit = read_csv('http://vulstats.ucsd.edu/data/duckworth-grit-scale-data/data-coded.csv')

glimpse(grit)

grit <- grit %>% 
  mutate(religion = recode(as.character(religion),
                               `1`='Agnostic',
                               `2`='Atheist',
                               `3`='Buddhist',
                               `4`='Christian (Catholic)',
                               `5`='Christian (Mormon)',
                               `6`='Christian (Protestant)',
                               `7`='Christian (Other)',
                               `8`='Hindu',
                               `9`='Jewish',
                               `10`='Muslim',
                               `11`='Sikh',
                               `12`='Other',
                               `0`='Other'))

grit <- grit %>% 
  filter(gender %in% c('male', 'female')) %>% 
  filter(religion != 'Other')

grit %>% count(religion)

# one sample t-test
lm(data = grit, conscientiousness ~ 1) %>% summary()
t.test(x = grit$conscientiousness)

# make null be something other than 0.  
# Here 6 (for randomly selecting 1-5 likert values that go into scale)
lm(data = grit, (conscientiousness-6) ~ 1) %>% summary()
t.test(x = grit$conscientiousness, mu = 6)

# two-sample t-test
lm(data = grit, conscientiousness ~ gender) %>% summary()
t.test(grit$conscientiousness ~ grit$gender, var.equal=T)
t.test(grit$conscientiousness[grit$gender == 'male'],
       grit$conscientiousness[grit$gender == 'female'], 
       var.equal=T)

# anova (one-way)
lm(data = grit, conscientiousness ~ age + religion) %>% summary()
lm(data = grit, conscientiousness ~ religion) %>% anova()

# anova (two-way)
lm(data = grit, conscientiousness ~ gender*religion) %>% summary()
lm(data = grit, conscientiousness ~ gender*religion) %>% anova()


x = c(618,606,735,627,679,622,712,772,728,550,594,681,578,689,672)
lm((x-700) ~ 1) %>% summary()

x1 = c(618,606,735,627,679,622,712,772,728,550,594,681,578,689,672)
x2 = c(571,569,613,693,714,521,530,736,677,626,722)
data.frame(y = c(x1, x2), 
           grp = c(rep('a', length(x1)), 
                   rep('b', length(x2)))) %>% 
  lm(data = ., y ~ grp) %>% 
  summary()

xa = c(611,600,587,718,583,653,700,695,592,585,650,617,617,648)
xb = c(586,589,571,705,550,632,674,664,578,563,619,607,591,622)
lm((xa - xb) ~ 1) %>% summary()

