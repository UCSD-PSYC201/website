
data {
  int<lower=1> N;
  int<lower=1> J; // number of states
  int<lower=0,upper=J> state[N];
  vector[N] logpop; // log population
  int<lower=0> y[N]; // number of murders
}

parameters {
  real mu;
  vector[J] intercept; //state specific intercept
  real slope; // slope on log(expected # of murders) / log(population)
  real<lower=0> intercept_sd;
}

model {
  intercept ~ normal(0, intercept_sd);
  y ~ poisson_log(logpop*slope + mu + intercept[state]);
}

