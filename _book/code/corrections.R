alpha = 0.05

library(tidyverse)

df <- data.frame(n.means = 2:100) %>%
  mutate(n.pairwise = n.means*(n.means-1)/2) %>%
  mutate(bonferoni.z.crit = qnorm(1-alpha/2/n.comparisons)) %>%
  mutate(tukey.z.crit = 1/sqrt(2)*qtukey(1-alpha, n.means, 100000)) %>%
  mutate(scheffe.z.crit = sqrt((n.means-1)*qf(1-alpha, n.means-1, 1000000)))

df %>%  ggplot(aes(x=n.means))+
  geom_line(aes(y=bonferoni.z.crit), color='red') + 
  geom_line(aes(y=tukey.z.crit), color='blue') + 
  geom_line(aes(y=scheffe.z.crit), color='darkgreen')+
  ylab('critical standardized statistic (e.g., z, t)')
