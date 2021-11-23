rm(list=ls())
library(tidyverse)
load(url('http://vulstats.ucsd.edu/data/shaferskelton.rdata'))

glimpse(dat)

dat = dat %>% rename(mouse = `Mouse/Touchscreen`,
                     saccades = saccade_condition,
                     subject = Subject_Initials)

dat = dat %>% select(subject, Error_Dist, mouse, Task, saccades)

aov(data = dat, Error_Dist ~ Task * mouse * saccades + 
      Error(subject/mouse*saccades*Task)) %>% 
  summary()

aov(data = dat, Error_Dist ~ Task * mouse * saccades + 
      Error(subject/(mouse*saccades*Task))) %>% 
  summary()

dat %>% count(subject,mouse,saccades,Task)

mean_dat = dat %>% 
  group_by(subject,mouse,saccades,Task) %>% 
  summarize(error = mean(Error_Dist))

aov(data = mean_dat, error ~ Task * mouse * saccades + 
      Error(subject/(mouse*saccades*Task))) %>% 
  summary()


mean_dat %>% ungroup() %>% count(subject)

good_subjects = mean_dat %>%
  ungroup() %>% 
  count(subject) %>% 
  filter(n == 16) %>% 
  pull(subject)

mean_dat = mean_dat %>% filter(subject %in% good_subjects)

aov(data = mean_dat, error ~ Task * mouse * saccades + 
      Error(subject/(mouse*saccades*Task))) %>% 
  summary()

