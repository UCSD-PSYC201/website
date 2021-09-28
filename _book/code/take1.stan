data {
  int<lower=1> N; // number of observations
  int<lower=1> K; // number of products
  int<lower=1,upper=K> pA[N]; // A product
  int<lower=1,upper=K> pB[N]; // A product
  vector[N] response;
}

parameters {
  real intercept;
  vector[K] productA;
  vector[K] productB;
  real<lower=0> sigma;
  real<lower=0> sigma_product;
}

model {
  productA ~ normal(0, sigma_product);
  productB ~ normal(0, sigma_product);
  response ~ normal(intercept + productA[pA] + productB[pB], sigma);
}

