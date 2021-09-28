data {
  int<lower=0> N; // number of observations.
  vector[N] log_population; // log(population)
  int<lower=0> crime_count[N]; // observed counts
  int<lower=0> n_crimes;
  int<lower=0> n_states;
  int<lower=1, upper=n_crimes> crime_idx[N]; 
  int<lower=1, upper=n_states> state_idx[N]; 
}

parameters {
  real intercept; 
  real slope; 
  vector[n_crimes] crime_intercept;
  real<lower=0> sigma_crime_intercept;
  vector[n_crimes] crime_slope;
  real<lower=0> sigma_crime_slope;
  vector[n_states] state_intercept;
  real<lower=0> sigma_state_intercept;
}

model {
  crime_intercept ~ normal(0, sigma_crime_intercept);
  crime_slope ~ normal(0, sigma_crime_slope);
  state_intercept ~ normal(0, sigma_state_intercept);
  for(i in 1:N) {
    crime_count[i] ~ poisson(exp(intercept 
                        + crime_intercept[crime_idx[i]] 
                        + state_intercept[state_idx[i]] 
                        + log_population * (slope + crime_slope[crime_idx[i]])));
  }
}

