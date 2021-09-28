data {
  int<lower=1> J;              // number of subjects
  int<lower=1> K;              // number of questions
  int<lower=1> N;              // number of observations
  int<lower=1,upper=J> jj[N];  // subject for observation n
  int<lower=1,upper=K> kk[N];  // question for observation n
  int<lower=0,upper=1> y[N];   // correctness for observation n
}

parameters {
  real mu_beta;         // mean student ability
  real alpha[J];      // ability of student j - mean ability
  real beta[K];       // difficulty of question k
  real<lower=0> gamma[K];       
  real<lower=0> sigma_beta;
  real<lower=0> sigma_gamma;
}

model {
  alpha ~ std_normal();         
  beta ~ normal(0, sigma_beta);   
  gamma ~ lognormal(0, sigma_gamma);   
  mu_beta ~ normal(0, 5);
  for (n in 1:N){
    y[n] ~ bernoulli_logit(gamma[kk[n]] * (alpha[jj[n]] - (beta[kk[n]] + mu_beta)));
  }
}
// 
// generated quantities {
//   vector[N] log_lik;
//   for (n in 1:N) {
//     log_lik[n] = bernoulli_logit_lpmf(y[n] | gamma[kk[n]] * (alpha[jj[n]] - (beta[kk[n]] + mu_beta)));
//   }
// }


