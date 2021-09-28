rm(list=ls())
library(tidyverse)
library(rstan)
library(tidybayes)

load(url('http://vulstats.ucsd.edu/data/crime.data.Rdata'))

stan.data = list(N = nrow(crime.data),
                 n_crimes = length(levels(crime.data$Crime)),
                 n_states = length(levels(crime.data$State)),
                 crime_count = crime.data$Count,
                 log_population = log(crime.data$Population),
                 crime_idx = as.integer(crime.data$Crime),
                 state_idx = as.integer(crime.data$State))

stan.file = 'glmer-crime-m4.stan'


# To avoid recompilation of unchanged Stan programs, we recommend calling
rstan_options(auto_write = TRUE)

stan.fit <- stan(
  file = stan.file,
  data = stan.data,
  cores = 1,
  chains = 1
)

