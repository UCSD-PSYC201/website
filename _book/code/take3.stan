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
  real<lower=0> sigma_product;
  real<lower=0> sigma_subject;
}

model {
  product ~ normal(0, sigma_product);
  intercept ~ normal(0, sigma_subject);
  response ~ normal(mu + intercept[S] - product[pA] + product[pB], sigma);
}

