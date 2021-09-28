library(tidyverse)

# parameters to play with.
h0 = c(mean=65, sd=3)
h1 = c(mean=66.5, sd=3)

n.sim = 100000
n = 5
alpha = 0.05

# calculate effect size (formula only good for equal variance)
effect.size = (66.5-65)/3

# sampling functions.
h0.sample = function(n){rnorm(n, h0['mean'], h0['sd'])}
h1.sample = function(n){rnorm(n, h1['mean'], h1['sd'])}

# statistic for test.
statistic = function(x){mean(x)}
# z score relative to sampling distribution of mean under the null
# statistic = function(x){(mean(x)-h0['mean'])/(h0['sd']/sqrt(n))}

samples = bind_rows(
  tibble(from="H0", stat = replicate(n.sim, statistic(h0.sample(n)))),
  tibble(from="H1", stat = replicate(n.sim, statistic(h1.sample(n)))))

# check that things went as expected (applies only if stat is the mean):
samples %>%
  group_by(from) %>%
  summarize(mean.of.samp.means = mean(stat),
            se = sd(stat),
            sd.recovered = sd(stat)*sqrt(n),
            N.per.samp.mean = n)
# yep!

# plot sampling distributions.
samples %>% ggplot(aes(x=stat, fill=from))+
  geom_histogram(position='identity', alpha=0.5, bins=50)+
  xlab('statistic')+
  theme_minimal()+
  theme(panel.grid = element_blank())

# calculate cutoffs for alpha.
cutoffs = samples %>% 
  filter(from == "H0") %>%
  .$stat %>%
  quantile(p=c(alpha/2, 1-alpha/2))

# 4-color code
colors = c('H0-sig' = 'red',
           'H0-ns' = 'darkgray',
           'H1-sig' = 'darkgreen',
           'H1-ns' = 'darkblue')

# color coded graph
samples %>% 
  mutate(sig = ifelse(stat < cutoffs[1] | stat > cutoffs[2], 'sig', 'ns')) %>%
  mutate(class = paste(from, sig, sep='-')) %>%
  ggplot(aes(x=stat, fill=class))+
  geom_histogram(position='identity', alpha=0.5, bins=50)+
  geom_vline(xintercept = cutoffs[1])+
  geom_vline(xintercept = cutoffs[2])+
  scale_fill_manual(values=colors)+
  xlab('statistic')+
  theme_minimal()+
  theme(panel.grid = element_blank())

# calculate alpha, beta, power, p(correct rejection)
samples %>% 
  mutate(sig = ifelse(stat < cutoffs[1] | stat > cutoffs[2], 'sig', 'ns')) %>%
  count(from, sig) %>%
  group_by(from) %>%
  summarize(p.sig = n[sig=='sig']/sum(n),
            p.ns = n[sig=='ns']/sum(n))

