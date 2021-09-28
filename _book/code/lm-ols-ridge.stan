data {
  int<lower=0> N; // number of observations.
  vector[N] x;
  vector[N] y;
}

parameters {
  real intercept; 
  real slope; 
  real<lower=0> sigma; 
}

model {
  slope ~ normal(0, 0.1)
  y ~ normal(intercept + slope  * x, sigma);
}

