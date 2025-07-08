# MCMC Samplers

# libraries
library("LaplacesDemon")
library("invgamma")
library("matlib")
library("tidyverse")
library("tictoc")
library("mcmcr")

# paper that explains
# https://arxiv.org/pdf/2110.10565


# simple linear regression gibbs sampler ----------------------------------

# # simulated
# n <- 50; p <- 3
# X <- MASS::mvrnorm(n, rep(0, p), 3^2*diag(nrow = p))
# b <- rep(1, p)
#
# Y <- X %*% b + rnorm(n, mean = 0, sd = 1)

reg_gibbs_sampler <- function(Y, X, new_X, iter = 5000, burn_in = 3000, thin = 2, g, nu_0 = 1, Sigma_0, beta_0){
  # initialize
  p <- ncol(X) #variables
  n <- nrow(X) # observations
  gram <- t(X) %*% X # gram matrix
  hat_mat <- X%*%inv(gram)%*%t(X)
  
  samples <- ceiling((iter - burn_in)/thin)
  
  # run ols
  ols <- lm(Y ~ X - 1)
  
  sigma_prop <- summary(ols)$sigma^2
  
  # specify hyperparameters
  if(missing(g)){
    g <- n*p
  }
  if(missing(Sigma_0)){
    Sigma_0 <- g*sigma_prop*inv(gram)
  }
  if(missing(beta_0)){
    beta_0 <- ols %>% coef()
  }
  
  if(missing(new_X)){
    new_X <- X
  }
  
  # conversion
  invSigma_0 <- inv(Sigma_0)
  
  # allocate space
  beta <- matrix(0, nrow = samples, ncol = p) # coef
  sigma <- numeric(samples) # sigma^2
  new_y <- matrix(0, nrow = samples, ncol = nrow(new_X))
  
  # sample from full conditionals
  for(i in 1:iter){
    # draw beta
    invQ <- inv(invSigma_0 + (sigma_prop)^(-1)*gram)
    l <- invSigma_0%*%beta_0 + (sigma_prop^(-1))*t(X)%*%Y
    beta_prop <- MASS::mvrnorm(1, invQ%*%l, invQ)
    
    # draw sigma^2
    sigma_prop <- rinvgamma(1, (nu_0 + n*p)/2, (nu_0*sigma_prop + t((Y-X%*%beta_prop))%*%(Y-X%*%beta_prop))/2)
    
    if(i > burn_in & (i - burn_in - 1) %% thin == 0){ #keep
      id <- (i - burn_in - 1) / thin + 1
      beta[id, ] <- beta_prop
      sigma[id] <- sigma_prop
      
      new_y[id,] <- new_X%*%beta_prop + rnorm(nrow(new_X), 0, sigma_prop^(.5))
    }
  }
  
  list("beta" = beta, "sigma" = sigma, "new_y" = new_y)
}

# tic()
# samples <- reg_gibbs_sampler(Y, X)
# toc()
# 
# samples$new_y
# 
# samples$beta %>% colMeans()


# linear regression Metropolis-Hastings sampler for heteroskedastity --------

# simulated data
# n <- 200; p <- 6
# X <- MASS::mvrnorm(n, rep(0, p), 3^2*diag(nrow = p))
# b <- rep(1, p)
# l <- rep(.1, p)
# log_var <- X%*%l + rnorm(n, mean = 0, sd = .1)
# 
# Y <- X %*% b + rnorm(n, mean = 0, sd = sqrt(exp(log_var)))

hetr_log_lik <- function(gamma, X, resid){
  sigma2 <- exp(X %*% gamma)
  -.5*sum(log(sigma2) + resid^2/sigma2)
}

hetr_log_accept <- function(gamma_new, gamma_old, X, resid){
  hetr_log_lik(gamma_new, X, resid) - hetr_log_lik(gamma_old, X, resid)
}

# this sampler uses X to model log(sigma^2) linearly. Since the full conditional of the coef lambda are not available analytically
# I use a metropolis hasting algorithm to propose new lambda values and accept or reject the proposed.
het_reg_mh_sampler <- function(Y, X, new_X, iter = 6000, burn_in = 1000, thin = 2, g,
                               beta_0, proposal_sd, gamma_prop, adapt_sd = 50, target_accept_rate = .45){
  # initialize
  p <- ncol(X) #variables
  n <- nrow(X) # observations
  gram <- t(X) %*% X # gram matrix
  hat_mat <- X%*%inv(gram)%*%t(X)
  
  samples <- ceiling((iter - burn_in)/thin)
  
  # run ols
  ols <- lm(Y ~ X - 1)
  
  sigma_prop <- rep(summary(ols)$sigma^2, n)
  
  # specify hyperparameters
  if(missing(g)){
    g <- n*p
  }

  if(missing(beta_0)){
    beta_0 <- ols %>% coef()
  }
  
  if(missing(gamma_prop)){
    gamma_prop <- inv(gram)%*%t(X)%*%log(sigma_prop)
  }
  
  if(missing(proposal_sd)){
    proposal_sd <- abs(gamma_prop) * 0.1 + 1e-3 # get appropriately sized jump
  }
  
  if(missing(new_X)){
    new_X <- X
  }
  
  # conversion
  invSigma_prop <- diag(1/sigma_prop)
  
  # allocate space
  beta <- matrix(0, nrow = samples, ncol = p) # beta coef
  sigma <- matrix(0, nrow = samples, ncol = n) # sigma_i^2
  gamma <- matrix(0, nrow = samples, ncol = p) # gamma coef
  acc_rate <- matrix(0, nrow = samples, ncol = p)
  new_y <- matrix(0, nrow = samples, ncol = nrow(new_X))
  accept_count <- numeric(p)
  acc <- numeric(p)
  
  # sample from full conditionals
  for(i in 1:iter){
    # draw beta
    invQ <- inv(t(X)%*%invSigma_prop%*%X + (1/g)*gram)
    l <- t(X)%*%invSigma_prop %*% Y + 1/g*gram%*%beta_0
    beta_prop <- MASS::mvrnorm(1, invQ%*%l, invQ)
    
    # draw gamma
    # random walk gamma proposal
    for(j in 1:p){
      gamma_new <- gamma_prop
      
      gamma_new[j] <- gamma_prop[j] + rnorm(1, 0, proposal_sd[j])
      
      accept_prob <- exp(min(0, hetr_log_accept(gamma_new, gamma_prop, X, Y - X %*% beta_prop)))
      if(runif(1) < accept_prob){
        gamma_prop <- gamma_new
        acc[j] <- TRUE
      }else{
        acc[j] <- FALSE
      }
      accept_count[j] <- accept_count[j] + acc[j]
    }

    
    # adjust proposal_sd every adapt_sd steps
    if(i %% adapt_sd == 0 & i <= burn_in){
      for(j in 1:p){
        if(accept_count[j]/adapt_sd > target_accept_rate + .05){
          proposal_sd[j] <- proposal_sd[j] * runif(1, 1, 1.2)
        }else if(accept_count[j]/adapt_sd < target_accept_rate - .05){
          proposal_sd[j] <- proposal_sd[j] / runif(1, 1, 1.2)
        }
      }
      accept_count <- numeric(p)
    }
    
    # compute sigma^2 (deterministically from gamma)
    sigma_prop <- exp(X%*%gamma_prop)
    invSigma_prop <- diag(as.numeric(1/sigma_prop))
    
    if(i > burn_in & (i - burn_in - 1) %% thin == 0){ #keep
      id <- (i - burn_in - 1) / thin + 1
      beta[id, ] <- beta_prop
      sigma[id, ] <- sigma_prop
      gamma[id, ] <- gamma_prop
      acc_rate[id,] <- acc
      
      # compute new Ys
      new_sigma_prop <- exp(new_X%*%gamma_prop)
      new_y[id,] <- new_X%*%beta_prop + rnorm(nrow(new_X), 0, new_sigma_prop^(.5))
    }
  }
  
  list("beta" = beta, "sigma" = sigma, "gamma" = gamma, "acc_rate" = acc_rate, "new_y" = new_y)
}

# tic()
# samples <- het_reg_mh_sampler(Y, X, thin = 2, iter = 6000, burn_in = 1000)
# toc()

# samples$acc_rate %>% mean()
# 
# samples$new_y
# 
# samples$beta %>% as.mcmc() %>% plot()
# 
# samples$gamma %>% #colMeans()
#   as.mcmc() %>% plot()

# hierarchical linear regression gibbs sampler-------------------------------------------------------------------------

# simulated
# n <- 200; p <- 8; J <- 4
# X <- MASS::mvrnorm(n, rep(0, p), 3^2*diag(nrow = p))
# group <- map(1:J, ~rep(.x, n/J)) %>% unlist()
# b <- MASS::mvrnorm(J, c(rep(1, p/2), rep(-1, p/2)), diag(nrow = p))
# X_g <- map(1:J, ~X[group == .x,]) # X matrix by group
# 
# Y <- map(1:J, ~{X_g[[.x]]%*%b[.x,] + rnorm(n/J, mean = 0, sd = 1)}) %>% unlist()

hlr_gibbs_sampler <- function(Y, X, group, iter = 2000, burn_in = 1000, thin = 5, g,
                              mu_0, Lambda_0, eta_0, S_0, nu_0 = 1, a_0 = 1, b_0){
  # initialize
  J <- length(unique(group)) #groups
  p <- ncol(X) #variables
  n <- nrow(X) # observations
  X_group <- map(1:J, ~X[group == .x,]) # X matrix by group
  Y_group <- map(1:J, ~Y[group == .x]) # Y vector by group
  group_indices <- split(seq_along(group), group)
  gram <- map(X_group, ~t(.x)%*%.x) # gram matrix
  
  samples <- ceiling((iter - burn_in)/thin)
  
  # run ols
  ols <- lm(Y ~ X - 1)
  
  sigma_0 <- summary(ols)$sigma^2
  
  # specify hyperparameters
  if(missing(g)){
    g <- J*p
  }
  if(missing(mu_0)){
    mu_0 <- ols %>% coef() # initialize with lm
  }
  if(missing(Lambda_0)){
    Lambda_0 <- g*sigma_0*inv(t(X)%*%X)
  }
  
  if(missing(eta_0)){
    eta_0 <- p + 2 #degrees of freedom for wishart
  }
  
  if(missing(S_0)){
    S_0 <- Lambda_0
  }
  if(missing(b_0)){
    b_0 <- 1/sigma_0
  }
  
  # conversions
  invLambda_0 <- inv(Lambda_0)
  
  # allocate space
  beta <- matrix(0, nrow = samples, ncol = p) # overall mean
  beta_j <- array(0, dim = c(samples, J, p)) # subgroup mean
  zeta <- numeric(samples) # variance of subgroup variance 
  invsigma_j <- matrix(0, nrow = samples, ncol = J) #subgroup variance
  invSigma <- array(0, dim = c(samples, p, p)) # variance of subgroup coef
  new_y <- matrix(0, nrow = samples, ncol = n)
  
  # initialize
  beta_prop <- MASS::mvrnorm(1, mu_0, Lambda_0) #overall coef
  invSigma_prop <- rwishart(eta_0, S_0) # variance of subgroup coef
  zeta_prop <- rgamma(1, shape = a_0, rate = b_0) # scale of variance
  beta_j_prop <- MASS::mvrnorm(J, beta_prop, invSigma_prop) # subgroup coef
  invsigma_j_prop <- rgamma(4, shape = nu_0/2, rate = nu_0*zeta_prop/2) # precision
  
  
  for(i in 1:iter){
    # draw beta_j and invsigma_j
    for(j in 1:J){
      invQ <- inv(invSigma_prop + invsigma_j_prop[j]*gram[[j]])
      l <- invSigma_prop%*%beta_prop + invsigma_j_prop[j]*(t(X_group[[j]])%*%Y_group[[j]])
      
      # draw beta_j
      beta_j_prop[j,] <- MASS::mvrnorm(1, invQ%*%l, invQ)
      
      # draw invsigma_j
      invsigma_j_prop[j] <- rgamma(1, shape = (nu_0 + n)/2, rate = (nu_0*zeta_prop +
                                   Reduce('+', map(1:length(Y_group[[j]]), ~{(Y_group[[j]][.x] - t(X_group[[j]][.x,])%*%beta_j_prop[j,])^2})))/2)
    }
    
    # draw beta
    invQ <- inv(invLambda_0 + J*invSigma_prop)
    l <- invLambda_0%*%mu_0 + invSigma_prop%*%colSums(beta_j_prop)
    beta_prop <- MASS::mvrnorm(1, invQ%*%l, invQ)
    
    # draw invSigma
    invSigma_prop <- rwishart(eta_0 + J, S_0 + Reduce('+', map(1:J, ~{(beta_j_prop[.x,] - beta_prop)%*%t(beta_j_prop[.x,] - beta_prop)})))
    
    # draw zeta
    zeta_prop <- rgamma(1, shape = a_0 + J*nu_0/2, scale = b_0 + nu_0/2*sum(invsigma_j_prop))
    
    if(i > burn_in & (i - burn_in - 1) %% thin == 0){ #keep
      id <- (i - burn_in - 1) / thin + 1
      beta_j[id, , ] <- beta_j_prop
      beta[id, ] <- beta_prop
      invSigma[id, ,] <- invSigma_prop
      invsigma_j[id, ] <- invsigma_j_prop
      zeta[id] <- zeta_prop
      
      y_prop <- map(1:J, ~{
        X_group[[.x]]%*%beta_j_prop[.x,] + rnorm(nrow(X_group[[.x]]), 0, invsigma_j_prop[.x]^(-.5))})
      
      for(j in seq_along(y_prop)){
        new_y[id, group_indices[[j]]] <- y_prop[[j]]}
    }
  }
  
  list("beta_j" = beta_j, "beta" = beta, "invSigma" = invSigma, "invsigma_j" = invsigma_j, "zeta" = zeta, "new_y" = new_y)
}

# tic()
# samples <- hlr_gibbs_sampler(Y_ktc, X_ktc, group_ktc, iter = 10000, burn_in = 3000, thin = 2)
# toc()
# 
# resid <- map_dbl(seq_len(length(Y_ktc)), ~(median(samples$new_y[.x,])-Y_ktc[.x]))
# 
# # x1^2, x2^2, x1_x2, x2_x3: 472
# sqrt(mean(resid^2))
# 
# resid %>%
#   as_tibble() %>%
#   ggplot() +
#   geom_density(aes(x = value))