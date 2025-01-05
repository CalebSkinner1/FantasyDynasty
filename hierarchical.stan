


data {
  int<lower=1> N; // number of samples
  int<lower=1> K; // number of coefficients
  int<lower=1> J; // number of groups
  int<lower=1> group[N]; // group indicator for each observation
  matrix[N, K] X; // design matrix for predictors
  vector[N] y; // outcome vector
  
  int<lower=0> N_new; // number of new observations
  matrix[N_new, K] X_new; // design matrix for new data
}
parameters {
  matrix[J, K] beta; // group-level regression coefficients
  vector[K] mu_beta; // population-level regression coefficients
  vector<lower=0>[K] tau_beta; // hierarchical variance
  real<lower=0> sigma;          // observation noise
}

model {
  // priors
  mu_beta ~ normal(0, 10); // weakly informative prior
  tau_beta ~ student_t(3, 0, 2); // weakly informative prior
  sigma ~ student_t(3, 0, 30); // weakly informative prior
  
  // Hierarchical priors for group-level coefficients
  for(j in 1:J){
    for (k in 1:K) {
      beta[j, k] ~ normal(mu_beta[k], tau_beta[k]);
    }
  }

  // Likelihood
  for (n in 1:N) {
    y[n] ~ normal(dot_product(X[n], beta[group[n]]), sigma);
  }
}

generated quantities {
  vector[N_new] y_pred;    // predictions for new data
  for (n in 1:N_new)
    y_pred[n] = normal_rng(dot_product(X_new[n], beta[group[n]]), sigma);
}

