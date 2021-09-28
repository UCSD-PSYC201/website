data {
  int<lower=1> N; // number of observations
  int<lower=1> K; // number of products
  int<lower=1> M; // number of subjects
  int<lower=1,upper=K> pA[N]; // A product
  int<lower=1,upper=K> pB[N]; // A product
  int<lower=1,upper=M> S[N]; // A product
  vector[N] response;
}

parameters {
  real mu;
  vector[M] intercept;
  vector[K] product;
  real<lower=0> sigma;
  real mu_scale;
  real<lower=0> sigma_scale;
  real<lower=0> sigma_subject;
  vector<lower=0>[M] scale;
  // changed to be vector to take advantage of vectorized operators
}

model {
  scale ~ lognormal(mu_scale, sigma_scale);
  product ~ normal(0, 1);
  intercept ~ normal(0, sigma_subject);
    response~ normal(mu + intercept[S] + 
      (product[pB] - product[pA]) .* scale[S], 
      sigma);
      // note use of matlab style elementwise .* multiplication.
}

