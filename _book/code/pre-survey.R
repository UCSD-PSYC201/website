library(tidyverse)
library(gridExtra)
rm(list=ls())

header.names = c('timestamp', 'email', 'laptop', 'os', 'computer', 'code', 'classes', 'terms', 'timezone', 'lectures', 'lecture_time', 'labs')
answer.remap = list(
  'laptop' = c('Yes'),
  'os' = c('Mac', 'Windows', 'Linux'),
  'computer' = c('browser', 'directory', 'terminal', 'editors', 'source'),
  'code' = c('HTML', 'Matlab', 'Python', 'Stata', 'R or S', 'JavaScript', 'C / C', 'LISP'),
  'classes' = c("from a social", "Calculus", "Linear", "from a math", "Machine", "Probability", "Differential", "Discrete"),
  'terms' = c('p-value', 'Confidence', 'power', 'error', 'Sampling', 'Cumulative', 'quantile', 'posterior', 'colinearity', 'general linear', 'generalized', 'mixed-effect', 'bayes', 'bootstrap', 'Markov chain Monte Carlo', 'gradient', 'forest'),
  'timezone' = c('Pacific'),
  'lectures' = c('Asynchronous'),
  'lecture_time' = c('Passive', 'Group', 'Not'),
  'labs' = c('Asynchronous')
)

tmp <- read_tsv('../../students/pre-survey.tsv')
students <- read_tsv('../../students/201a-list.txt', skip = 3)

names(tmp) <- header.names
tmp = tmp %>% filter(email != 'evul@ucsd.edu')


for(field in names(answer.remap)){
  i = 1;
  for(code in answer.remap[[field]]){
    tmp[[sprintf("%s.%s", field, code)]] = grepl(code, tmp[[field]], ignore.case = TRUE)
    i = i+1
  }
}

tmp <- tmp[,!(names(tmp) %in% names(answer.remap))]

n = nrow(tmp)

retmp <- tmp %>% 
  gather(key = question, value=response, -(1:2)) %>%
  separate(col=question, 
           into=c('type', 'item'), 
          sep = "\\.",
           remove = F) %>% 
  filter(type %in% c('terms', 'classes', 'code')) %>% 
  mutate(response = as.logical(response)) %>% 
  group_by(question) %>%
  mutate(baserate = mean(response)) %>%
  ungroup() %>%
  mutate(score = ifelse(response, -log(baserate), log(1-baserate))) %>%
  mutate(score = ifelse(response, 1, 0)) %>%
  group_by(email, type) %>% 
  summarise(typescore = sum(score)) %>%
  spread(key=type, value=typescore) %>%
  ungroup() %>%
  filter(email != 'evul@ucsd.edu')

pca <- retmp %>%
  select(terms, classes, code) %>%
  prcomp(center=T, scale=T) 
pca %>% summary()

loadings = data.frame(email=retmp$email, pca$x)

loadings <- left_join(students, loadings, by=c('Email'='email'))

loadings %>% ggplot(aes(x=PC1, y=PC2, color=Major))+geom_point()

groups = ceiling(nrow(loadings)/4)
group.table <- loadings %>% 
  mutate(group = as.vector(replicate(4, sample(1:groups, size = groups, replace=F)))[1:nrow(loadings)]) %>% 
  mutate(name = paste0(first, ' ', last)) %>%
  select(name, group) %>% 
  arrange(desc(runif(n()))) %>%
  group_by(group) %>%
  mutate(id = 1:n()) %>% 
  spread(key = id, value = name)

write_csv(group.table, path='groups.201a.csv')
  
g1 <- retmp %>% ggplot(aes(x=terms, y=classes))+
  geom_point()+
  geom_smooth(method='lm')+
  theme_bw()

g2 <- retmp %>% ggplot(aes(x=terms, y=code))+
  geom_point()+
  geom_smooth(method='lm')+
  theme_bw()

g3 <- retmp %>% ggplot(aes(x=terms, y=computer))+
  geom_point()+
  geom_smooth(method='lm')+
  theme_bw()

grid.arrange(g1,g2, ncol=2)


rename = c('from a social' = 'stats (social)',
           'from a math' = 'stats (math)',
           'C / C' = 'C++',
           'general linear' = 'general LM',
           'generalized' = 'generalized LM',
           'Markov chain Monte Carlo' = 'MCMC')
results <- tmp %>% 
  select(-timestamp, -email, laptop.Yes) %>% 
  summarize_all(mean) %>% 
  gather(question, mean) %>% 
  separate(col=question, 
           into=c('field', 'item'), 
           sep = "\\.",
           remove = F) %>% 
  mutate(item = recode(item, 'from a social' = 'stats (social)',
         'from a math' = 'stats (math)',
         'C / C' = 'C++',
         'general linear' = 'general LM',
         'generalized' = 'generalized LM',
         'Markov chain Monte Carlo' = 'MCMC')) %>% 
  mutate(field = factor(field, levels = names(answer.remap))) %>% 
  group_by(field) %>% 
  arrange(desc(mean), .by_group = TRUE) %>% 
  filter(question != 'laptop.Yes')
  
results %>% 
  mutate(question = factor(question, levels = results$question)) %>% 
  ggplot(aes(x=question, y=mean, fill=field))+
  facet_grid(.~field, scales="free_x", space="free_x")+
  # facet_wrap(~field, scales = "free_x", ncol=4)+
  geom_col()+
  theme_minimal()+
  ggtitle(paste0("N = ", n))+
  coord_cartesian(ylim=c(0,1), expand = c(0,0))+
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5),
        panel.border = element_rect(color = "#AAAAAA", fill = NA, size = 0.1),
        panel.grid.major.y = element_line(color = "#CCCCCC", size = 0.1),
        panel.grid.major.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_text(size=12),
        axis.title.y = element_text(size=14),
        strip.text = element_text(size=14, angle=0, vjust=-0.1),
        legend.position = 'none')

        