


data {
  int<lower=1> N; // number of samples
  int<lower=1> K; // number of coefficients
  vector[N] x1; // first predictor values
  vector[N] x2; // second predictor values
  vector[N] y; // outcome vector
  
  int<lower=0> N_new; // number of new observations
  vector[N_new] x1_new; // first predictor values
  vector[N_new] x2_new; // first predictor values
}

parameters {
  vector[K] beta; // regression coefficients
  real<lower=0> sigma;          // observation noise
}

model {
  // priors
  beta ~ normal(0, 5); // weakly informative prior
  sigma ~ cauchy(0, 2.5); // weakly informative prior
  
  // Likelihood
  for (n in 1:N) {
    real poly_pred = beta[1]                   // Intercept
                   + beta[2] * x1[n]           // x1
                   + beta[3] * square(x1[n])   // x1^2
                   + beta[4] * x2[n]           // x2
                   + beta[5] * square(x2[n])   // x2^2
                   + beta[6] * x1[n] * x2[n];
                   
    
    y[n] ~ normal(poly_pred, sigma);
  }
}

generated quantities {
  vector[N_new] y_pred;    // Predictions for new data
  for (n in 1:N_new)
    y_pred[n] = normal_rng(beta[1] + beta[2] * x1_new[n] + beta[3] * square(x2_new[n]) +
                           beta[4] * x2_new[n] + beta[5] * square(x2_new[n]), sigma) +
                           beta[6] * x1[n] * x2[n];
}

