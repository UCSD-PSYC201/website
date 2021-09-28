require(tidyverse)
require(tidybayes)
require(rstan)

N = 100
df <- tibble(x = rnorm(N),
       y = x*0.2+rnorm(N))
lm(data = df, y~x) %>% summary()


# make a stan model of this
fit <- stan(file = 'lm.stan',
     data = list(N = N,
                 x = df$x,
                 y = df$y),
     chains = 4,
     warmup = 1000,
     iter = 2000,
     cores = 1)

summary(fit)$summary
# add bayesian regularization / priors

fit <- stan(file = 'lm-prior.stan',
            data = list(N = N,
                        priorSD = 1,
                        x = df$x,
                        y = df$y),
            chains = 4,
            warmup = 1000,
            iter = 2000,
            cores = 1)

summary(fit)$summary

# 

samples <- tidybayes::spread_draws(fit, intercept, slope, sigma)

samples %>% ggplot(aes(x=sigma))+
  geom_density(aes(fill = .chain, group=.chain), alpha=0.2)+
  theme_minimal()

bayestestR::hdi(samples$intercept, ci=0.95)
