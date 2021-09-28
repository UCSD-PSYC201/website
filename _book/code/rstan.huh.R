
library(rstan)

stan.file = 'model.stan'

stan.code = '
data {
  int<lower=0> J;          // number of schools 
  real y[J];               // estimated treatment effects
}
parameters {
  real mu; 
  real<lower=0> sigma;
}
model {
  y ~ normal(mu, sigma);
}
'

fp <- file(stan.file)
writeLines(stan.code, fp)
close(fp)

data.for.stan <- list(
  J = 8,
  y = rnorm(8, 5, 3)
)

fit <- stan(
  file = stan.file,  # Stan program
  data = data.for.stan,    # named list of data
  chains = 4,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 2000,            # total number of iterations per chain
  cores = 2,              # number of cores (using 2 just for the vignette)
  refresh = 1000          # show progress every 'refresh' iterations
)
