library(tidyverse)
library(rstan)
library(tidybayes)

# fake data.
n = 100
dat <- data.frame(x = rnorm(n)) %>%
  mutate(y = x*1+rnorm(n, 0, 0.5))

# standard lm
summary(lm(data = dat, y~x))

# stan data.
library(rstan)

stan.data = list(N = nrow(dat),
                 x = dat$x, 
                 y = dat$y)

stan.file = 'lm-ols.stan'

stan.fit <- stan(
  file = stan.file,
  data = stan.data
)

stan.fit


library(tidybayes)
samples <- stan.fit %>% 
  recover_types() %>%
  spread_draws(intercept, slope, sigma)
head(samples)

samples %>% 
  gather(key=parameter, 
         value = estimate, 
         intercept, slope, sigma) %>%
  ggplot(aes(x=.iteration, y=estimate, color = as.factor(.chain)))+
  facet_wrap(~parameter)+
  geom_line()

samples %>% 
  gather(key=parameter, 
         value = estimate, 
         intercept, slope, sigma) %>%
  filter(.iteration < 10) %>%
  ggplot(aes(x=.iteration, y=estimate, color = as.factor(.chain)))+
  facet_wrap(~parameter)+
  geom_line()


samples %>% 
  gather(key=parameter, 
         value = estimate, 
         intercept, slope, sigma) %>%
  ggplot(aes(x=estimate, fill=as.factor(.chain)))+
  facet_grid(.chain~parameter)+
  geom_histogram(bins=100)


samples %>% 
  gather(key=parameter, 
         value = estimate, 
         intercept, slope, sigma) %>%
  filter(parameter=='slope') %>%
  ggplot(aes(x=estimate))+
  facet_grid(~parameter)+
  geom_histogram(bins=100)

samples %>%
  gather(key=parameter, 
         value = estimate, 
         intercept, slope, sigma) %>%
  group_by(parameter) %>% 
  summarize(posterior.mean = mean(estimate), 
            posterior.sd = sd(estimate))

samples %>%
  summarize_at(vars(intercept, slope, sigma), list(mean=mean, sd=sd))

# adding priors / regularization