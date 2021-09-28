data {
  int<lower=0> N; // number of observations.
  vector[N] x;
  vector[N] y;
}

parameters {
  real<lower=1> nu;
  real intercept; 
  real slope; 
  real<lower=0> sigma; 
}

model {
//  slope ~ normal(0, 0.3);
  y ~ student_t(nu, intercept + slope  * x, sigma);
}

