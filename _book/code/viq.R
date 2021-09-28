library(tidyverse)
library(rstan)
library(tidybayes)

REFIT = T
viq <- read_tsv('../data/VIQ/VIQT_data.csv')

answers = read_tsv('../data/VIQ/answer_key.tsv')

viq <- viq %>% mutate(subject = 1:n())

qa <- viq %>% select(subject, Q1:Q45) %>% 
  pivot_longer(Q1:Q45, names_to = "question", values_to="response") %>% 
  left_join(answers) %>% 
  mutate(correct = case_when(
    response == answer ~ 1,
    response <= 0 ~ 0.5,
    response != answer ~ 0)
  ) %>% 
  mutate(question = as.integer(stringr::str_sub(question, 2)))


# score -------------------------------------------------------------------

qa %>% 
  mutate(score = case_when(
    correct == 1 ~ 1,
    correct == 0.5 ~ 0,
    correct == 0 ~ 0
  )) %>% 
  group_by(subject) %>% 
  summarize(score = sum(score)) %>% 
  ggplot(aes(x=score))+
  geom_histogram(binwidth = 1)

tmp <- qa %>% filter(subject < 100)


stan_data = list(N=nrow(tmp),
                 J = length(unique(tmp$subject)),
                 K = length(unique(tmp$question)),
                 jj = as.integer(tmp$subject),
                 kk = as.integer(tmp$question),
                 y = ifelse(tmp$correct == 1, 1, 0))


fitfile = 'IRT-0pl-std.Rdata'
if(REFIT | !file.exists(fitfile)){
  fit <- stan(file = 'IRT-0pl-std.stan',
              data = stan_data,
              chains = 4,
              warmup = 1000,
              iter = 3000,
              cores = 2)
  loo_r = loo(fit, save_psis = T)
  bs = bridgesampling::bridge_sampler(fit)
  save(fit, bs, loo_r, file=fitfile)
} else {
  load(fitfile)
}

summary(fit)$summary[,c('mean', 'sd', 'Rhat')]

raws = tmp %>%   
  group_by(subject) %>% 
  mutate(score = case_when(
    correct == 1 ~ 1,
    correct == 0.5 ~ 0,
    correct == 0 ~ 0
  )) %>% 
  summarize(score_m = mean(score))
  
fit %>% spread_draws(alpha[subject]) %>% 
  group_by(subject) %>% 
  summarize(alpha_m = mean(alpha), alpha_s = sd(alpha)) %>% 
  left_join(raws) %>% 
  ggplot(aes(x=score_m, y=alpha_m)) +
  geom_pointrange(aes(ymin = alpha_m - alpha_s, 
                      ymax = alpha_m + alpha_s))


fitfile = 'IRT-1pl-std.Rdata'
if(REFIT | !file.exists(fitfile)){
  fit <- stan(file = 'IRT-1pl-std.stan',
              data = stan_data,
              chains = 4,
              warmup = 1000,
              iter = 3000,
              cores = 2)
  loo_r = loo(fit, save_psis = T)
  bs = bridgesampling::bridge_sampler(fit)
  save(fit, bs, loo_r, file=fitfile)
} else {
  load(fitfile)
}

summary(fit)$summary[,c('mean', 'sd', 'Rhat')]

rawq = tmp %>% 
  mutate(score = case_when(
    correct == 1 ~ 1,
    correct == 0.5 ~ 0,
    correct == 0 ~ 0
  )) %>% 
  group_by(question, description) %>% 
  summarize(score_m = mean(score), score_sem = sd(score, na.rm = T)/sqrt(n())) %>% 
  ungroup() %>% 
  mutate(description = fct_reorder(description, score_m))

fit %>% spread_draws(beta[question]) %>% 
  group_by(question) %>% 
  summarize(beta_m = mean(beta), beta_s = sd(beta)) %>% 
  left_join(rawq) %>% 
  ggplot(aes(x=score_m, y=beta_m)) +
  geom_point()


fit %>% spread_draws(alpha[subject]) %>% 
  group_by(subject) %>% 
  summarize(alpha_m = mean(alpha), alpha_s = sd(alpha)) %>% 
  left_join(raws) %>% 
  ggplot(aes(x=score_m, y=alpha_m)) +
  geom_pointrange(aes(ymin = alpha_m - alpha_s, 
                      ymax = alpha_m + alpha_s))




fitfile = 'IRT-2pl.Rdata'
if(REFIT | !file.exists(fitfile)){
  fit <- stan(file = 'IRT-2pl.stan',
              data = stan_data,
              chains = 4,
              warmup = 1000,
              iter = 3000,
              cores = 1)
  loo_r = loo(fit, save_psis = T)
  bs = bridgesampling::bridge_sampler(fit)
  save(fit, bs, loo_r, file=fitfile)
} else {
  load(fitfile)
}

summary(fit)$summary[,c('mean', 'sd', 'Rhat')]

fit %>% spread_draws(beta[question]) %>% 
  group_by(question) %>% 
  summarize(beta_m = mean(beta), beta_s = sd(beta)) %>% 
  left_join(rawq) %>% 
  ggplot(aes(x=score_m, y=beta_m)) +
  geom_point()

fit %>% spread_draws(beta[question], gamma[question]) %>% 
  group_by(question) %>% 
  summarize(beta_m = mean(beta), beta_s = sd(beta),
            gamma_m = mean(gamma), gamma_s = sd(gamma)) %>% 
  ggplot(aes(x=gamma_m, y=beta_m)) +
  geom_point()

fit %>% spread_draws(alpha[subject]) %>% 
  group_by(subject) %>% 
  summarize(alpha_m = mean(alpha), alpha_s = sd(alpha)) %>% 
  left_join(raws) %>% 
  ggplot(aes(x=score_m, y=alpha_m)) +
  geom_pointrange(aes(ymin = alpha_m - alpha_s, 
                      ymax = alpha_m + alpha_s))


fit %>% spread_draws(beta[question], gamma[question]) %>% 
  group_by(question) %>% 
  summarize(beta_m = mean(beta), beta_s = sd(beta),
            gamma_m = mean(gamma), gamma_s = sd(gamma))  %>% 
  left_join(rawq) %>% 
  mutate(description= fct_reorder(description, gamma_m)) %>% 
  ggplot(aes(x = description, y=gamma_m))+
  geom_pointrange(aes(ymin = gamma_m - gamma_s, 
                      ymax = gamma_m + gamma_s))+
  coord_flip()
  
# data {
#   int<lower=1> J;              // number of students
#   int<lower=1> K;              // number of questions
#   int<lower=1> N;              // number of observations
#   int<lower=1,upper=J> jj[N];  // student for observation n
#   int<lower=1,upper=K> kk[N];  // question for observation n
#   int<lower=0,upper=1> y[N];   // correctness for observation n
# }
