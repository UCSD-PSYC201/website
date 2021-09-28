library(tidyverse)
library(rstan)
library(tidybayes)

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


tmp <- qa %>% filter(subject <= 100)


tmp %>% glimpse()

tmp <- tmp %>% 
  mutate(correct = ifelse(correct == 0.25, 0, correct))

tmp <- tmp %>% 
  mutate(group = sample(c('train', 'test'), n(), replace=T))

train = tmp %>% filter(group == 'train')

train %>% 
  ggplot(aes(x=question, y=subject, fill=correct))+
  geom_tile()

subjectacc = train %>% 
  group_by(subject) %>% 
  summarize(score_s = mean(correct))

questionacc = train %>% 
  group_by(question) %>% 
  summarize(score_q = mean(correct))

test <- tmp %>% filter(group == 'test')

test %>% 
  left_join(subjectacc) %>% 
  left_join(questionacc) %>% 
  select(subject, question, correct, score_s, score_q) %>% 
  mutate(prediction = pmax(1e-8, pmin(1-1e-8, (score_s + score_q)/2))) %>% 
  mutate(logp = log(prediction)*correct + log(1-prediction)*(1-correct)) %>% 
  summarize(elogp = mean(logp))


test %>% 
  left_join(subjectacc) %>% 
  left_join(questionacc) %>% 
  select(subject, question, correct, score_s, score_q) %>% 
  mutate(prediction = pmax(1e-8, pmin(1-1e-8, (score_s + score_q)/2))) %>% 
  mutate(prediction = ifelse(prediction > 0.5, 1, 0)) %>% 
  summarize(accuracy = mean(prediction == correct))

# data {
#   int<lower=1> J;              // number of subjects
#   int<lower=1> K;              // number of questions
#   int<lower=1> N;              // number of observations
#   int<lower=1,upper=J> jj[N];  // subject for observation n
#   int<lower=1,upper=K> kk[N];  // question for observation n
#   int<lower=0,upper=1> y[N];   // correctness for observation n
# }

stan_data = list(J = length(unique(train$subject)), 
                 K = length(unique(train$question)), 
                 N = nrow(train), 
                 jj = train$subject, 
                 kk = train$question, 
                 y = train$correct)


fit <- stan(file = 'IRT-2pl-noll.stan',
     data = stan_data)

summary(fit)$summary[,c('mean', 'sd', 'Rhat')]

alphas = tidybayes::spread_draws(fit, alpha[subject]) %>% 
  group_by(subject) %>% 
  summarize(alpha = mean(alpha)) %>% 
  pull(alpha)

betas = tidybayes::spread_draws(fit, beta[question]) %>% 
  group_by(question) %>% 
  summarize(beta = mean(beta)) %>% 
  pull(beta)

gammas = tidybayes::spread_draws(fit, gamma[question]) %>% 
  group_by(question) %>% 
  summarize(gamma = mean(gamma)) %>% 
  pull(gamma)

mu_beta = tidybayes::spread_draws(fit, mu_beta) %>% 
  summarize(mu_beta = mean(mu_beta)) %>% pull(mu_beta)

logistic = function(x){1/(1+exp(-x))}

test %>% 
  select(subject, question, correct) %>% 
  mutate(logodds = gammas[question]*(alphas[subject] - (betas[question] + mu_beta))) %>% 
  mutate(prediction = logistic(logodds)) %>% 
  mutate(prediction = ifelse(prediction > 0.5, 1, 0)) %>% 
  summarize(accuracy = mean(prediction == correct))

  
test %>% 
  select(subject, question, correct) %>% 
  mutate(logodds = gammas[question]*(alphas[subject] - (betas[question] + mu_beta))) %>% 
  mutate(prediction = logistic(logodds)) %>% 
  mutate(logp = log(prediction)*correct + log(1-prediction)*(1-correct)) %>% 
  summarize(elogp = mean(logp))
