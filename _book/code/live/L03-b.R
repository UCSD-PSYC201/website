# projects!
# hw groups and support!

library(tidyverse)
# http://vulstats.ucsd.edu/data/duckworth-grit-scale-data/data-coded.csv
dat <- read_csv('http://vulstats.ucsd.edu/data/duckworth-grit-scale-data/data-coded.csv')

# "What is your religion?" 
# 1= Agnostic,
# 2= Atheist,
# 3= Buddhist,
# 4= Christian (Catholic),
# 5= Christian (Mormon),
# 6= Christian (Protestant),
# 7= Christian (Other),
# 8=Hindu,
# 9=Jewish,
# 10=Muslim,
# 11=Sikh,
# 12=Other
# how to recode the variable? 
# make a new column!

# a loop?
# make a vector names, with associated numbers

# mutate + case_when

# recode
dat <- dat %>% 
  mutate(religion.name = 
           recode(as.character(religion),
                  `1` = 'Agnostic',
                  `2` = 'Atheist',
                  `3` = 'Buddhist',
                  `4` = 'Christian (Catholic)',
                  `5` = 'Christian (Mormon)',
                  `6` = 'Christian (Protestant)',
                  `7` = 'Christian (Other)',
                  `8` ='Hindu',
                  `9` ='Jewish',
                  `10` ='Muslim',
                  `11` ='Sikh',
                  `12` ='Other'))

dat %>% count(religion, religion.name)

# how does age vary with religion:
# generate different facets per religion, with histogram of age in each

dat %>% 
  filter(age < 100) %>% 
  ggplot(aes(x=age))+
  geom_histogram()+
  facet_wrap(vars(religion.name), scales = 'free_y')

# how does conscientiousness vary as a function of religion?

# generate summary data frame
religion_summary = dat %>% 
  group_by(religion.name) %>% 
  summarise(m_conscientiousness = mean(conscientiousness),
            sem_conscientiousness = sd(conscientiousness)/sqrt(n()))

# plot violins with summary dataframe pointrange
dat %>% 
  ggplot(aes(x=religion.name, y=conscientiousness))+
  geom_violin()+
  geom_pointrange(data = religion_summary,
             aes(y=m_conscientiousness,
                 ymin = m_conscientiousness - sem_conscientiousness,
                 ymax = m_conscientiousness + sem_conscientiousness))

# plot violins with summary pointrange using stat_summary
dat %>% 
  ggplot(aes(x=religion.name, y=conscientiousness))+
  geom_violin()+
  stat_summary(fun.data = mean_se)+
  coord_flip()

dat %>% 
  group_by(religion.name) %>% 
  summarise(m_conscientiousness = mean(conscientiousness),
            sem_conscientiousness = sd(conscientiousness)/sqrt(n())) %>% 
  ggplot(aes(x=religion.name, y=m_conscientiousness))+
  geom_col()+
  geom_errorbar(aes(ymin = m_conscientiousness - sem_conscientiousness,
                    ymax = m_conscientiousness + sem_conscientiousness))+
  xlab('Religion')+
  ylab('Conscientsiousness')+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=90, hjust = 1, vjust = 0.5),
        panel.grid = element_blank())

dat %>% 
  group_by(religion.name) %>% 
  summarise(m_conscientiousness = mean(conscientiousness),
            sem_conscientiousness = sd(conscientiousness)/sqrt(n())) %>% 
  ggplot(aes(x=religion.name, y=m_conscientiousness))+
  geom_pointrange(aes(ymin = m_conscientiousness - sem_conscientiousness,
                      ymax = m_conscientiousness + sem_conscientiousness))+
  xlab('Religion')+
  ylab('Conscientsiousness')+
  theme_minimal()+
  coord_flip()

  
  
  ##
2+2
  
  
  
  
  
  












# what is the relationship between ?? and ??
