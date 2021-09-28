# install.packages('babynames')
# install.packages('stringr')

library(tidyverse)
library(babynames)
class <- read_tsv('../../students/201a-list.txt', skip = 0)

first <- class$Student %>% 
  stringr::str_split(", ") %>% 
  map_chr(function(x)return(x[2])) #%>% 
  stringr::str_split(" ") %>% 
  map_chr(function(x)return(x[1]))

cur.year = 2020
fx = log10
sd.age = 0.15
mean.age = 24
prior.age = function(age){pnorm(fx(age+0.5), fx(mean.age), sd.age)-pnorm(fx(age-0.5), fx(mean.age), sd.age)}
# prior.age = function(age){dnorm(1,1,1)}
# data.frame(age=1:120, prior=prior.age(1:120)) %>% 
#   ggplot(aes(x=age, y=prior))+
#   geom_line()+
#   theme_minimal()+
#   theme(text=element_text(size=16))

tmp <- babynames %>%
  filter(name %in% first) %>%
  mutate(age = cur.year-year) %>%
  select(name, age, sex, n) %>%
  filter(age < 100) %>%
  group_by(name) %>%
  mutate(p.female = sum(n[sex=='F'])/sum(n)) %>%
  filter(sex==ifelse(p.female > 0.5, 'F', 'M')) %>%
  filter(sum(n)>100) %>%
  ungroup() %>%
  complete(name, age, fill=list(n=0)) %>%
  group_by(name) %>%
  mutate(sex = (sex[!is.na(sex)])[1],
         p.female = (p.female[!is.na(sex)])[1],
         likelihood = n/sum(n),
         prior = prior.age(age),
         prior = prior/sum(prior),
         posterior = prior*likelihood,
         posterior = posterior/sum(posterior)) %>%
  arrange(name, age) %>%
  mutate(cum.posterior = cumsum(posterior)) %>%
  mutate(mean.age = sum(age*posterior)/sum(posterior)) %>%
  ungroup()

name.summary <- tmp %>% group_by(name) %>% 
  summarize(mean.age = sum(age*posterior)/sum(posterior),
            sd.age = sqrt(sum((age-mean.age)^2*posterior)/sum(posterior)),
            p.L = approx(y = age, x = cum.posterior, xout = 0.25)$y,
            p.H = approx(y = age, x = cum.posterior, xout = 0.75)$y) %>%
  arrange(mean.age) %>%
  mutate(f.name = factor(name, levels=name))

M = 30
offset = -0.5
g <- tmp %>% mutate(f.name = factor(name, levels=name.summary$name)) %>%
  mutate(groupNum = as.integer(f.name)) %>%
  arrange(desc(mean.age)) %>%
  mutate(order.name = factor(name, levels=rev(name.summary$name))) %>%
  ggplot(aes(x=age, y=groupNum, group=order.name)) + 
    geom_ribbon(aes(x=age, ymin=groupNum+offset, ymax=M*posterior+groupNum+offset, fill=p.female), alpha=0.9) +
    geom_line(aes(y=M*posterior+groupNum+offset)) +
  geom_errorbarh(data = name.summary, aes(y=as.numeric(f.name), group=f.name, x = mean.age, xmin=p.L, xmax=p.H), color='black', height = 0, size=1.5)+
  geom_errorbarh(data = name.summary, aes(y=as.numeric(f.name), group=f.name, x = mean.age, xmin=p.L, xmax=p.H), color='white', height = 0, size=0.5)+
  geom_point(data = name.summary, aes(y=as.numeric(f.name), group=f.name, x = mean.age), color='black', height = 0, size=3.5)+
  geom_point(data = name.summary, aes(y=as.numeric(f.name), group=f.name, x = mean.age), color='white', height = 0, size=2.5)+
  scale_y_continuous('student name', breaks=1:nrow(name.summary), labels = paste0(name.summary$name, " (", round(name.summary$mean.age,0), ")")) +
  scale_x_continuous('student age', breaks=seq(10,120,10)) +
  theme_minimal() +
  scale_fill_continuous(high="#992222", low="#222299")+
  theme(panel.grid = element_blank(),
        legend.position = 'none',
        axis.text = element_text(size = 14),
        axis.title = element_blank())
  
print(g)

# who got dropped?
paste(first[!(first %in% name.summary$name)], collapse=", ")

# what fraction of folks are missing?
babynames %>% 
  group_by(year, sex) %>% 
  summarise(missing = 1-sum(prop)) %>%
  ggplot(aes(x=year, y=missing, color=sex))+
  geom_line() +  
  scale_x_continuous('Birth year', breaks=seq(1880,2010,20)) +
  scale_y_continuous('Proportion missing', breaks=seq(0,0.1,0.01)) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = 'top',
        axis.text = element_text(size = 12),
        axis.title = element_text(size=14),
        axis.text.x = element_text(angle=90, hjust=-1, vjust=0.1))

# proportion female

babynames %>% 
  group_by(year, sex) %>%
  summarize(inferred.n = sum(n)/sum(prop)) %>%
  ungroup() %>% group_by(year) %>%
  summarize(p.female = sum(inferred.n[sex=='F'])/sum(inferred.n)) %>%
  ggplot(aes(x=year, y=p.female)) +
  geom_hline(yintercept = 0.5, color='gray') +
  geom_line() +  
  scale_x_continuous('Birth year', breaks=seq(1880,2020,20)) +
  scale_y_continuous('Proportion female', breaks=seq(0,1,0.1)) +
  coord_cartesian(xlim = c(1880, 2020), ylim=c(0.3,0.7))+
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = 'none',
        axis.text = element_text(size = 12),
        axis.title = element_text(size=14),
        axis.text.x = element_text(angle=90, hjust=-1, vjust=0.1))

# describe babynames
babynames %>% 
  mutate(decade = floor(year/10)*10) %>%
  group_by(name, sex, decade) %>%
  summarise(n = sum(n)) %>%
  ungroup() %>%
  group_by(decade, sex) %>%
  arrange(desc(n)) %>%
  top_n(5) %>%
  ungroup() %>%
  arrange(decade)
  
  
