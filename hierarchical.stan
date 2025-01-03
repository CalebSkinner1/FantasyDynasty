


data {
  int<lower=1> N; // number of samples
  int<lower=1> K; // number of coefficients
  int<lower=1> J; // number of groups
  int<lower=1> group[N]; // group indicator for each observation
  vector[N] x1; // first predictor values
  vector[N] x2; // second predictor values
  vector[N] y; // outcome vector
}
parameters {
  matrix[J, K] beta; // group-level regression coefficients
  vector[K] mu_beta; // population-level regression coefficients
  vector<lower=0>[K] tau_beta; // hierarchical variance
  real<lower=0> sigma;          // observation noise
}

model {
  // priors
  mu_beta ~ normal(0, 5); // weakly informative prior
  tau_beta ~ cauchy(0, 2.5); // weakly informative prior
  sigma ~ cauchy(0, 2.5); // weakly informative prior
  
  // Hierarchical priors for group-level coefficients
  for (k in 1:K) {
    beta[, k] ~ normal(mu_beta[k], tau_beta[k]);
  }
  
  // Likelihood
  for (n in 1:N) {
    real poly_pred = beta[group[n], 1]                   // Intercept
                   + beta[group[n], 2] * x1[n]           // x1
                   + beta[group[n], 3] * square(x1[n])   // x1^2
                   + beta[group[n], 4] * x2[n]           // x2
                   + beta[group[n], 5] * square(x2[n]);   // x2^2
    
    y[n] ~ normal(poly_pred, sigma);
  }
}

