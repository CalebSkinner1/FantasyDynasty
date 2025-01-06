// polynomial regression model
// this model is obsolete - sorry :/


data {
  int<lower=1> N; // number of samples
  int<lower=1> K; // number of coefficients
  matrix[N, K] X; // design matrix for predictors
  vector[N] y; // outcome vector
  
  int<lower=0> N_new; // number of new observations
  matrix[N_new, K] X_new; // design matrix for new data
}

parameters {
  vector[K] beta; // regression coefficients
  real<lower=0> sigma;          // observation noise
}

model {
  // priors
  beta ~ normal(0, 5); // weakly informative prior
  sigma ~ student_t(3, 0, 1); // weakly informative prior
  
  // Likelihood
  y ~ normal(X * beta, sigma);
}

generated quantities {
  vector[N_new] y_pred;    // predictions for new data
  for (n in 1:N_new)
    y_pred[n] = normal_rng(dot_product(X_new[n], beta), sigma);
}

