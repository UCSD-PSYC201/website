library(tidyverse)
library(rstan)
library(tidybayes)

# signaliness data
sig <- read_csv('http://vulstats.ucsd.edu/data/signaling_expt2.csv')

glimpse(sig)

# we want to estimate some "signaliness score" for each product.

sig %>% ggplot(aes(x=response))+geom_histogram()

# remove the attention check trials
sig <- sig %>% 
  filter(attention == 0)

m <- lm(data = sig, 
   response ~ product_A + product_B)

summary(m)
# note that we have two estimates for each product one per position, we want to a model to estimate one parameter per product, rather than kludge them together.

# lets reimplement in stan (take1.stan)

# find a list of all products that appear in A or B.
products = sort(unique(c(unique(sig$product_A), unique(sig$product_B))))

# create integer product vectors
sig <- sig %>% 
  mutate(pA = as.integer(factor(product_A, levels = products)),
         pB = as.integer(factor(product_B, levels = products)))

# data for stan models.
stan_data = list(N = nrow(sig),
                 K = length(products),
                 pA = sig$pA,
                 pB = sig$pB,
                 response = sig$response)

rstan_options(auto_write = T)

fit1 <- stan(file = 'take1.stan', 
     data = stan_data,
     chains = 4,
     warmup = 1000,
     iter = 2000,
     cores = 2)

# save file in case r session crashes... which it seems to often, unfortunately.
save(fit1, file='take1fit.Rdata')
load('take1fit.Rdata') # load after crash

# pull out samples, and plot correlation between A and B estimates.
fit1 %>% 
  spread_draws(productA[product], productB[product]) %>% 
  group_by(product) %>% 
  summarize(productA = mean(productA),
            productB = mean(productB)) %>% 
  ggplot(aes(x=productA, y=productB))+
  geom_point()


# now we set up a model that uses just one product label for both A and B occurrences of hte same product.
fit2 <- stan(file = 'take2.stan', 
            data = stan_data,
            chains = 4,
            warmup = 1000,
            iter = 2000,
            cores = 1)

save(fit2, file='take2fit.Rdata')
load('take2fit.Rdata') # load after crash

# plot a credible interval on the score of each product

# pull out samples, then calculate 95% confidence intervals using
# normal approximation (mean +/- 2 sd)
# quantile based, symmetrics
# highest density interval
product_scores <- fit2 %>% 
  spread_draws(product[pidx]) %>% 
  rename(score = product,
         product = pidx) %>% 
  group_by(product) %>% 
  summarize(score_mean = mean(score),
            score_lower_ms = mean(score) - 2*sd(score),
            score_upper_ms = mean(score) + 2*sd(score),
            score_lower_q = quantile(score, 0.025),
            score_upper_q = quantile(score, 0.975),
            score_lower_hdi = bayestestR::hdi(score, ci = 0.95)$CI_low,
            score_upper_hdi = bayestestR::hdi(score, ci = 0.95)$CI_high) 

# show one of the credible intervals (here hdi)

product_scores %>% 
  mutate(product_name = products[product]) %>% 
  mutate(product_name = reorder(product_name, -score_mean)) %>% 
  ggplot(aes(x = score_mean, y=product_name))+
  geom_pointrange(aes(xmin=score_lower_hdi, xmax=score_upper_hdi))
  
# now lets try to compare the values of two specific parameters:
# how do we ask whether thebicycle coefficient is different from the scarf coefficient?
product_scores %>% 
  mutate(product_name = products[product]) %>% 
  select(product_name, score_mean, score_lower_hdi, score_upper_hdi) %>% 
  filter(product_name %in% c('bicycle', 'scarf'))


# DO NOT DO THIS.
# THIS IS VERY WRONG
fit2 %>% 
  spread_draws(product[pidx]) %>% 
  rename(score = product,
         product = pidx) %>% 
  mutate(product_name = products[product]) %>% 
  filter(product_name %in% c('bicycle', 'scarf')) %>% 
  lm(data = ., score ~ product_name) %>% 
  summary()
# THE ABOVE IS VERY WRONG BECAUSE IT TREATS EACH OF OUR POSTERIOR SAMPLES
# AS AN INDEPENDENT DATA SAMPLE, WHICH IS WILDLY INCORRECT
# FIRST, IT IS INCORRECT BECAUSE IT TREATS SAMPLES OF THE COEFFICIENT
# AS THOUGH THEY ARE SAMPLES OF SOME DATA POPULATION
# SECOND IT IS INCORRECT BECAUSE IT TREATS EACH SAMPLE AS AN INDEPENDENT
# OBSERVATION, SO WE CAN MAKE "ESTIMATES" ARBITRARILY PRECISE BY
# TAKING MORE SAMPLES FROM OUR SIMULATION.  THAT IS VERY INCORRECT.

# instead, we want to use the distribution of sampled coefficient values 
# to make claims about teh difference in coefficients, by doing calculations
# on that distribution directly.

# pull out the difference between scarves and bicycles for each sample
# this takes into account any correlation in posterior samples.
scarf_bicycle_samples <- fit2 %>% 
  spread_draws(product[pidx]) %>% 
  rename(score = product,
         product = pidx) %>% 
  mutate(product_name = products[product]) %>% 
  filter(product_name %in% c('bicycle', 'scarf')) %>% 
  ungroup() %>% 
  select(product_name, score, .draw) %>% 
  arrange(.draw) %>% 
  pivot_wider(id_cols = .draw, 
              names_from = product_name, 
              values_from = score) %>% 
  mutate(difference = scarf-bicycle)

# here is a histogram of the probability distribution of the 
# difference between scarf and bicycle coeffiicents
scarf_bicycle_samples %>% 
  ggplot(aes(x=difference))+geom_histogram()

# we can put a confidence interval on that difference
scarf_bicycle_samples %>% 
  pull(difference) %>% 
  quantile(c(0.025, 0.975))
# it includes 0 so we should not say that they are different.

# probability that scarves > bicycle
scarf_bicycle_samples %>% 
  summarize(p = mean(difference > 0))
# probability is middling.  we would want it to be close to 0 or 1
# to indicate that scarves or bicycles have a reliably larger
# coefficient


# now we move on to take3.stan where we add a random effect of subject\

# accordingly we need to add a few variables to the data
stan_data = list(N = nrow(sig),
                 K = length(products),
                 M = length(unique(sig$subject)),
                 S = as.integer(factor(sig$subject)),
                 pA = sig$pA,
                 pB = sig$pB,
                 response = sig$response)



fit3 <- stan(file = 'take3.stan', 
             data = stan_data,
             chains = 4,
             warmup = 1000,
             iter = 2000,
             cores = 2)

save(fit3, file='take3fit.Rdata')
load('take3fit.Rdata') # load after crash


# from this we can inspect the confidence intervals again
# since we care about product effects, we might suspect that we could estimate those
# more precisely by taking into account subject variation

# using tidybayes hdi 95% interval
fit3 %>% 
  spread_draws(sigma_subject) %>% 
  mean_hdi(.width = 0.95)
# note that this is a pretty small sd, given that we are on a scale of 1-1000
# an sd of 22 in subject-specific biases is pretty small
# so we might suspect that this model does not yield an improvement
# we could endeavor to see if this is a better model using either
# bayes factors or leave-one-out posterior predictive probabilities
# or we could just ask: do our credible intervals on product coefficients
# (the stuff we really care about) change?




# this model we sketched in class but did not run, this one
# has a term in which different subjects use the scale differently,
# not just in their overal A-B bias, but also in how wide a range they use.
fit4 <- stan(file = 'take4.stan', 
             data = stan_data,
             chains = 4,
             warmup = 1000,
             iter = 2000,
             cores = 2)

save(fit4, file='take4fit.Rdata')
load('take4fit.Rdata') # load after crash

fit4 %>% 
  spread_draws(sigma_subject) %>% 
  mean_hdi(.width = 0.95)


fit4 %>% 
  spread_draws(sigma_scale) %>% 
  mean_hdi(.width = 0.95)

# a more concise, tidybayes approach to getting the product-specific intervals
product_scores <- fit4 %>% 
  spread_draws(product[pidx]) %>% 
  mean_hdi(.width = 0.95) %>% 
  rename(score = product,
         product = pidx) %>% 
  mutate(product_name = products[product])

product_scores %>% 
  mutate(product_name = reorder(product_name, -score)) %>% 
  ggplot(aes(x = score, y=product_name))+
  geom_pointrange(aes(xmin=.lower, xmax=.upper))
# note that here we converted the product parameters to be ~N(0,1) and offput the scaling to
# the response scale to the individual subject scale terms.  
# the *relative* comparisons between products still stand,
# but the absolute values of the coefficients have changed meanings.
# we could ask about the SNR of product coefficient variance under model 4 and model 2:

# variance across products compared to overall variance:
overall = fit4 %>% 
  spread_draws(product[pidx]) %>% 
  ungroup() %>% 
  summarize(v = var(product)) %>% pull(v)
var.within = fit4 %>% 
  spread_draws(product[pidx]) %>% 
  mean_hdi() %>% 
  pull(product) %>% var()
var.within / overall
# so a fairly large percent of overall variance in coefficients is reflected in variance across products.

# compare to fit2:
load('take2fit.Rdata') # load after crash
overall = fit2 %>% 
  spread_draws(product[pidx]) %>% 
  ungroup() %>% 
  summarize(v = var(product)) %>% pull(v)
var.within = fit2 %>% 
  spread_draws(product[pidx]) %>% 
  mean_hdi() %>% 
  pull(product) %>% var()
var.within / overall
# pretty much the same.  
# so the more elaborate model did not really improve the 
# precision of our product coefficient estimates.

  
