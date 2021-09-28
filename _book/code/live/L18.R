library(tidyverse)

load(url('http://vulstats.ucsd.edu/data/shaferskelton.rdata'))

glimpse(dat)

dat %>% 
  rename(error = Error_Dist,
         m.t = `Mouse/Touchscreen`,
         s.c = saccade_condition,
         task = Task,
         sub = Subject_Initials) %>% 
         aov(data = ., error ~ m.t*s.c*task + Error(sub/(m.t*s.c*task))) %>% 
  summary()

dat <- dat %>% 
  rename(error = Error_Dist,
         m.t = `Mouse/Touchscreen`,
         s.c = saccade_condition,
         task = Task,
         sub = Subject_Initials) 

dat %>% 
  count(sub, m.t, s.c, task)

dat.means <- dat %>% 
  group_by(sub, m.t, s.c, task) %>% 
  summarise(error.mean = mean(error))


dat.means %>% 
  count(sub, m.t, s.c, task)

dat.means %>% 
  aov(data = ., error.mean ~ m.t*s.c*task + Error(sub/(m.t*s.c*task))) %>% 
  summary()

dat.means %>% ungroup() %>% 
  count(sub)

subjects.we.really.want <- dat.means %>% ungroup() %>% 
  count(sub) %>% 
  filter(n==16) %>% 
  pull(sub)

dat.means %>% 
  filter(sub %in% subjects.we.really.want) %>% 
  aov(data = ., error.mean ~ m.t*s.c*task + Error(sub/(m.t*s.c*task))) %>% 
  summary()

dat.means %>% 
  ggplot(aes(x=log10(error.mean)))+
  geom_histogram()

dat.means %>% 
  group_by(m.t, s.c, task) %>% 
  summarize(m.error = mean(error.mean),
            se.error = sd(error.mean)/sqrt(n())) %>% 
  ungroup() %>% 
  ggplot(aes(x=s.c, y=m.error, color=task, group=task))+
  facet_grid(~m.t)+
  geom_line()+
  geom_pointrange(aes(ymin = m.error - se.error, 
                      ymax = m.error + se.error))

dat.means %>% 
  mutate(y = log10(error.mean)) %>% 
  group_by(sub) %>% 
  mutate(delta.y = y - mean(y)) %>% 
  group_by(m.t, s.c, task) %>% 
  summarize(m.delta.y = mean(delta.y),
            se.delta.y = sd(delta.y)/sqrt(n())) %>% 
  
  ggplot(aes(x=s.c, y=m.delta.y, color=task, group=task))+
  facet_grid(~m.t)+
  geom_line()+
  geom_pointrange(aes(ymin = m.delta.y - se.delta.y, 
                      ymax = m.delta.y + se.delta.y))
  