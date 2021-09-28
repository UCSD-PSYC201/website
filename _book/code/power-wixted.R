library(pwr)

n = round(pwr::pwr.norm.test(d=0.5, power = 0.7)$n)

d.thresh = qnorm(0.975, 0, 1/sqrt(n))
1-pnorm(d.thresh, 0.5, 1/sqrt(n))

library(tidyverse)

dat <- tibble(d1 = rnorm(1000, 0.5, 1/sqrt(n)), d2 = rnorm(1000, 0.5, 1/sqrt(n))) 
m.d1 = dat %>% filter(d1 > qnorm(0.975, 0, 1/sqrt(n))) %>% pull(d1) %>% mean()
m.d2 = dat %>% filter(d1 > qnorm(0.975, 0, 1/sqrt(n))) %>% pull(d2) %>% mean()

dat %>% 
  ggplot(aes(d1,d2))+
  geom_point(size=0.1, color='black')+
  annotate("rect", xmin = qnorm(0.975, 0, 1/sqrt(n)), xmax = -0.5,
            ymin = -0.5, ymax=1.5, fill=alpha('red', 0.2))+
  annotate("rect", xmin = qnorm(0.975, 0, 1/sqrt(n)), xmax = 1.5,
           ymin = -0.5, ymax=1.5, fill=alpha('green', 0.2))+
  geom_abline(slope=1, color='black')+
  geom_point(x = m.d1, y=m.d2, color='red')+
  theme_minimal()+
  xlab('observed effect in first experiment')+
  ylab('observed effect in replication')

delta = 0.001
d = seq(-1, 2, by=delta)
p = dnorm(d, 0.5, 1/sqrt(25))
p[d < d.thresh] = 0
p = p/sum(p)
plot(d,p)
sum(d*p)

tibble(d=seq(-0.5, 1.5, by=delta), 
       p=dnorm(d, 0.5, 1/sqrt(25))) %>% 
  mutate(sig = d>d.thresh) %>% 
  ggplot(aes(x=d, ymax=p, ymin=0, fill=sig))+
  geom_ribbon()+
  geom_vline(xintercept = 0.5, color='black')+
  geom_vline(xintercept = 0.6, color='blue')+
  geom_vline(xintercept = 1, color='red')
  


## observed vs true effect size, given constant sample size

n = 25
d.thresh = qnorm(0.975, 0, 1/sqrt(n))
data.frame(d = seq(-1,1,by=0.0001)) %>% 
  mutate(d.o = rnorm(n(), d, 1/sqrt(n))) %>% 
  filter(abs(d.o) > abs(d.thresh)) %>% 
  ggplot(aes(x=d, y=d.o))+
  geom_point(size=0.02)+
  stat_summary_bin(geom='line', fun.y='mean', color='blue')+
  xlab('true effect size')+
  ylab('observed effect size')


n = 25
d.thresh = qnorm(0.975, 0, 1/sqrt(n))
data.frame(d = seq(-1,1,by=0.0001)) %>% 
  mutate(d.o = rnorm(n(), d, 1/sqrt(n))) %>% 
  mutate(reported = ifelse(runif(n())<0.5, abs(d.o) > abs(d.thresh),
                           d.o > d.thresh)) %>% 
  filter(reported) %>% 
  ggplot(aes(x=d, y=d.o))+
  geom_point(size=0.02)+
  stat_summary_bin(geom='line', fun.y='mean', color='blue')+
  xlab('true effect size')+
  ylab('observed effect size')




n = 25
d.thresh = qnorm(0.975, 0, 1/sqrt(n))
data.frame(d = seq(-1,1,by=0.0001)) %>% 
  mutate(n.sample = rgeom(n(), 0.05)+2) %>% 
  mutate(d.o = rnorm(n(), d, 1/sqrt(n.sample))) %>% 
  filter(abs(d.o) > abs(qnorm(0.975, 0, 1/sqrt(n.sample)))) %>% 
  ggplot(aes(x=d, y=d.o))+
  geom_point(size=0.02)+
  stat_summary_bin(geom='line', fun.y='mean', color='blue')+
  xlab('true effect size')+
  ylab('observed effect size')




n = 25
d.thresh = qnorm(0.975, 0, 1/sqrt(n))
data.frame(d = seq(-1,1,by=0.00001)) %>% 
  mutate(n.sample = rgeom(n(), 0.05)+2) %>% 
  mutate(d.o = rnorm(n(), d, 1/sqrt(n.sample))) %>% 
  filter(abs(d.o) > abs(qnorm(0.975, 0, 1/sqrt(n.sample)))) %>% 
  mutate(d.o = abs(d.o)) %>% 
  ggplot(aes(x=d, y=d.o))+
  geom_point(size=0.02)+
  stat_summary_bin(geom='line', fun.y='mean', color='blue')+
  xlab('true effect size')+
  ylab('absolute observed effect size')


data.frame(d = seq(-1,1,by=0.00001)) %>% 
  mutate(n.sample = rgeom(n(), 0.05)+2) %>% 
  mutate(d.o = rnorm(n(), d, 1/sqrt(n.sample))) %>% 
  filter(abs(d.o) > abs(qnorm(0.975, 0, 1/sqrt(n.sample)))) %>% 
  mutate(d.o = abs(d.o)) %>% 
  ggplot(aes(x=d, y=n.sample))+
  geom_point(size=0.02)+
  stat_summary_bin(geom='line', fun.y='mean', color='blue')+
  xlab('true effect size')+
  ylab('sample size of what was significant')
