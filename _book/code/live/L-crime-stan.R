rm(list=ls())
library(tidyverse)

load('../../data/crime.data.Rdata')

glimpse(crime.data)

murder.data <- crime.data %>% filter(Crime == 'Murder')

murder.data <- murder.data %>% 
  filter(State %in% 
           c('ALABAMA', 'CALIFORNIA', 'NEW YORK', 'LOUISIANA')) %>% 
  mutate(State = as.factor(as.character(State)))


library(rstan)

stan_data = list(
  N = nrow(murder.data),
  J = length(unique(murder.data$State)),
  state = as.integer(murder.data$State),
  logpop = log(murder.data$Population),
  y = murder.data$Count
)
# int<lower=1> N;
# int<lower=1> J; // number of states
# int<lower=0,upper=J> state[N];
# vector[N] logpop; // log population
# int<lower=0> y[N]; // number of murders

fit <- stan(file = 'murder.stan',
     data = stan_data,
     chains = 4,
     warmup = 1000,
     iter = 2000,
     cores = 1)

library(tidybayes)

summary(fit)$summary[,c('mean', 'sd', 'Rhat')]


fit0 <- stan(file = 'murder_null.stan',
            data = stan_data,
            chains = 4,
            warmup = 1000,
            iter = 2000,
            cores = 1)
b1 = bridgesampling::bridge_sampler(fit)
b0 = bridgesampling::bridge_sampler(fit0)

exp(b1$logml - b0$logml)

spread_draws(fit, intercept[state]) %>% 
  mutate(state = levels(murder.data$State)[state]) %>% 
  ggplot(aes(x=intercept, fill=.chain, group=.chain))+
    facet_wrap(.~state) +
    geom_density(alpha=0.2)


spread_draws(fit, intercept[state]) %>% 
  mutate(state = levels(murder.data$State)[state]) %>% 
  ggplot(aes(x=intercept))+
  facet_wrap(.~state) +
  geom_density(alpha=0.2)


# expected count = population^1.2* exp(-11.139778)
# 
# y = a + b + s*x



fit.ranef <- stan(file = 'murder-ranef.stan',
            data = stan_data,
            chains = 4,
            warmup = 1000,
            iter = 2000,
            cores = 1)

summary(fit.ranef)$summary[,c('mean', 'sd', 'Rhat')]

spread_draws(fit.ranef, mu) %>% 
  ggplot(aes(x=mu, fill=.chain, group=.chain))+
  geom_density(alpha=0.2)


