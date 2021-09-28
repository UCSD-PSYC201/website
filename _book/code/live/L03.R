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
  geom_histogram(binwidth = 10)

dat %>% 
  filter(age < 100) %>% 
  ggplot(aes(x=age)) +
  geom_density(fill='blue')


dat %>% 
  ggplot(aes(y=neuroticism, x=gender))+
  geom_point(position = position_jitter(width = 0.25),
             size = 1, alpha = 0.25)

dat %>% 
  filter(gender %in% c('male', 'female')) %>% 
  ggplot(aes(y=neuroticism, x=gender))+
  geom_violin()+
  stat_summary(fun.data = mean_se)

dat %>% 
  filter(gender %in% c('male', 'female')) %>% 
  sample_n(50) %>% 
  ggplot(aes(y=neuroticism, x=gender, fill=gender, color=gender))+
  geom_violin(alpha = 0.25)+
  stat_summary(fun.data = mean_se, 
               geom="pointrange", size=1.5)+
  scale_fill_manual(values = c('female'='purple', 'male'='red'))+
  ggtitle('our wonderful plot')+
  theme_minimal()+
  theme()

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
  geom_smooth(method = 'lm')+
  facet_grid(race~gender)

dat %>% select(age, gender, extroversion:vocabulary) %>% 
  gather(key = 'measure', 
         value = 'score',
         extroversion:vocabulary) %>%
  filter(gender %in% c('male', 'female')) %>% 
  filter(age < 100) %>% 
  group_by(measure) %>% 
  mutate(score = (score - mean(score))/sd(score)) %>% 
  ungroup() %>% 
  ggplot(aes(x=age, y=score, color=gender))+
  facet_grid(gender ~ measure) +
  scale_color_manual(values = c('male'='mediumorchid4', 
                                'female'='orange'))+
  geom_point(size=0.1, position=position_jitter(width=0, height=0.1))+
  geom_smooth(method='lm', color = 'black')+
  theme_classic()+
  theme(panel.border = element_rect(color='black', fill=NA))
  
  
dat %>% count(religion)

# "What is your religion?" 
# 1=Agnostic,
# 2=Atheist,
# 3=Buddhist,
# 4=Christian (Catholic),
# 5=Christian (Mormon),
# 6=Christian (Protestant),
# 7=Christian (Other),
# 8=Hindu,
# 9=Jewish,
# 10=Muslim,
# 11=Sikh,
# 12=Other

dat <- dat %>% 
  mutate(religion.cat = recode(as.character(religion),
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
                               `12`='Other'))

# numerical ~ numerical + categorical
# cons ~ grit 
# grit ~ consc

test_fn = function(data){
  y = mean(data$grit)
  sy = sd(data$grit)/sqrt(length(data$grit))
  data.frame(y = y,
             ymax = y+sy,
             ymin = y-sy,
             x = mean(data$conscientiousness))
}


keep.religions <- dat %>% count(religion.cat) %>% 
  filter(n>100) %>% 
  pull(religion.cat)


summary_dat <- dat %>% 
  filter(religion.cat %in% keep.religions) %>% 
  group_by(religion.cat) %>% 
  summarize(mean_grit = mean(grit),
            mean_conscientiousness = mean(conscientiousness),
            s_grit = sd(grit)/sqrt(n()),
            s_conscientiousness = sd(conscientiousness)/sqrt(n()))

summary_dat %>% 
  ggplot(aes(x=religion.cat, 
             y = mean_grit,
             ymax = mean_grit + 2*s_grit,
             ymin = mean_grit - 2*s_grit,
             fill=religion.cat))+
  geom_col()+
  geom_point()+
  geom_errorbar(width = 0.01)

dat %>% 
  filter(religion.cat %in% keep.religions) %>% 
  ggplot(aes(x = conscientiousness,
                   y = grit,
                   color = religion.cat))+
  # geom_point(alpha=0.1)+
  geom_point(data  = summary_dat,
             aes(x = mean_conscientiousness,
                 y = mean_grit))+
  geom_errorbar(data = summary_dat,
                  aes(x = mean_conscientiousness,
                      y = mean_grit,
                      ymax = mean_grit + 2*s_grit,
                      ymin = mean_grit - 2*s_grit))+
  geom_errorbarh(data = summary_dat,
                 aes(x = mean_conscientiousness,
                     y = mean_grit,
                     xmax = mean_conscientiousness + 2*s_conscientiousness,
                     xmin = mean_conscientiousness - 2*s_conscientiousness))
           

summary_dat = dat %>%
  filter(religion.cat %in% keep.religions) %>% 
  filter(age < 100) %>% 
  group_by(religion.cat,
           age.bin = round(age/5)*5) %>% 
  summarize(m_c  = mean(conscientiousness),
            s_c = sd(conscientiousness),
            n = n())

summary_dat %>% 
  filter( n > 1) %>% 
  ggplot(aes(x=age.bin, 
             y=m_c, 
             ymax=m_c+s_c/sqrt(n),
             ymin=m_c-s_c/sqrt(n),
             color=religion.cat,
             group=religion.cat))+
  geom_pointrange()+
  geom_line()

dat %>% 
  filter(religion.cat %in% keep.religions) %>% 
  filter(age < 100) %>% 
  ggplot(aes(x = age,
             y = conscientiousness,
             color = religion.cat))+
  geom_smooth(method='lm')


summary_dat = dat %>%
  filter(age < 100) %>% 
  group_by(gender,
           age.bin = round(age/5)*5) %>% 
  summarize(m_n  = mean(neuroticism),
            s_n = sd(neuroticism),
            n = n())

summary_dat %>% 
  filter( n > 1) %>% 
  filter(gender %in% c('male', 'female')) %>% 
  ggplot(aes(x=age.bin, 
             y=m_n, 
             color=gender,
             fill= gender,
             group=gender))+
  geom_point(size=2)+
  geom_line(size=1.1)+
  geom_ribbon(aes(ymax=m_n+s_n/sqrt(n),
                  ymin=m_n-s_n/sqrt(n)),
              alpha=0.3, color=NA)+
  geom_text(data = data.frame(gender= c('male', 'female'),
                              age.bin = c(20, 20),
                              m_n = c(16, 20)),
            aes(label=gender))+
  xlab('Age')+ylab('Neuroticism')+
  scale_x_continuous(breaks = seq(0, 100, by=5))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        text = element_text(size = 14),
        title = element_text(size=16, face = 'bold'),
        legend.position = 'none')




