library(tidyverse)

ASL.Lex <- read_csv('http://vulstats.ucsd.edu/data/ASL.Lex.clean.csv')

dat <- ASL.Lex %>% 
  filter(LexicalClass %in% c('Noun', 'Verb')) %>% 
  select(Gloss, LexicalClass, Location)

entropy = function(p){-sum(log2(p)*p)}

dat %>% 
  count(LexicalClass, Location) %>% 
  complete(LexicalClass, Location, fill=list(n=0)) %>% 
  group_by(LexicalClass) %>% 
  mutate(p = n/sum(n)) %>% 
  ggplot(aes(x=Location, y=p, fill=LexicalClass))+
  geom_col(position=position_dodge())+
  theme_minimal()+
  coord_flip()

dat %>% 
  count(LexicalClass, Location) %>% 
  complete(LexicalClass, Location, fill=list(n=0)) %>% 
  group_by(LexicalClass) %>% 
  summarize(H = entropy((n+1)/(sum(n+1))), .groups='drop')

statistic = function(data){
  data %>% 
    count(LexicalClass, Location) %>% 
    complete(LexicalClass, Location, fill=list(n=0)) %>% 
    group_by(LexicalClass) %>% 
    summarize(H = entropy((n+1)/(sum(n+1))), .groups='drop') %>% 
    summarize(d = H[LexicalClass=='Noun'] - H[LexicalClass=='Verb']) %>% 
    pull(d)
}

shuffle = function(data){
  data %>% mutate(LexicalClass = sample(LexicalClass, n(), replace = F))
}

n_shuffles = 10000
null_stat = rep(NA, n_shuffles)
for(i in 1:n_shuffles){
  null_stat[i] = statistic(shuffle(dat))
}

(my_stat = statistic(dat))

tibble(statistic = null_stat) %>% 
  ggplot(aes(x=statistic, 
             fill=statistic > round(my_stat,1)))+
  geom_histogram(breaks = seq(-3, 3, by=0.1))+
  geom_vline(xintercept = my_stat, color='black')+
  theme_minimal()+
  theme(legend.position = 'none')


# calculate two-tailed p-value

(sum(null_stat >= my_stat) + 1) / (n_shuffles + 2)
