library(tidyverse)

set.seed(1)

n = 9
population <- function(n){rnorm(n,65,3)}
dpop <- function(x){dnorm(x, 65, 3)}
pop.x = seq(65-3*4, 65+3*4, length.out = 1000)
statistic = function(x){(mean(x)-65)/(3/sqrt(n))}

sample <- tibble(x = population(n)+1)

null.samples = tibble(x = population(n), sample='1') %>%
  bind_rows(tibble(x=population(n), sample='2')) %>%
  bind_rows(tibble(x=population(n), sample='3')) %>%
  bind_rows(tibble(x=population(n), sample='4')) %>%
  bind_rows(tibble(x=population(n), sample='5')) %>%
  bind_rows(tibble(x=population(n), sample='6'))

sample %>% ggplot(aes(x=x))+
  geom_histogram(binwidth = 1, color='black')+
  theme_minimal()+
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(size=16))

mystat = statistic(sample$x)


null.samples %>% ggplot(aes(x=x))+
  facet_grid(sample~.)+
  geom_histogram(binwidth = 1, color='black')+
  theme_minimal()+
  theme(panel.grid = element_blank())

tibble(x=pop.x) %>%
  ggplot(aes(x=x, y=dpop(x)))+
  geom_area(fill='blue')+
  geom_vline(xintercept = 65, color='black', size=2)+
  theme_minimal()+
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(size=16))


null.samples %>% group_by(sample) %>% summarize(m=statistic(x))




tibble(statistic = replicate(10000, statistic(population(n)))) %>%
  ggplot(aes(x=statistic, fill=statistic<=mystat))+
  geom_histogram(bins = 50, color='black')+
  theme_minimal()+
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(size=16),
        legend.position = 'none')

tibble(statistic = replicate(10000, statistic(population(n)))) %>%
  mutate(sig = statistic >= mystat) %>%
  summarize(mean(sig))

summary = tibble(statistic = replicate(10000, statistic(population(n)))) %>%
  summarize(m = mean(statistic), s=sd(statistic))

tibble(statistic = replicate(10000, statistic(population(n)))) %>%
  ggplot(aes(x=statistic))+
  geom_histogram(bins = 50, color='black', fill='lightgray')+
  geom_vline(xintercept = summary$m, color='darkred', size=2)+
  geom_segment(x = summary$m, xend=summary$m+summary$s, y=300, yend=300, color='darkgreen', size=2)+
  theme_minimal()+
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(size=16))

