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

# simulated
# n <- 50; p <- 3
# X <- MASS::mvrnorm(n, rep(0, p), 3^2*diag(nrow = p))
# b <- rep(1, p)
# #
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
    beta_prop <- ols %>% coef()
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
    l <- invSigma_0%*%beta_prop + (sigma_prop^(-1))*t(X)%*%Y
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


# hierarchical linear regression gibbs sampler-------------------------------------------------------------------------

# simulated
# n <- 200; p <- 8; J <- 4
# X <- MASS::mvrnorm(n, rep(0, p), 3^2*diag(nrow = p))
# group <- map(1:J, ~rep(.x, n/J)) %>% unlist()
# b <- MASS::mvrnorm(J, c(rep(1, p/2), rep(-1, p/2)), diag(nrow = p))
# X_g <- map(1:J, ~X[group == .x,]) # X matrix by group
# 
# Y <- map(1:J, ~{X_g[[.x]]%*%b[.x,] + rnorm(n/J, mean = 0, sd = 1)}) %>% unlist()

hlr_gibbs_sampler <- function(Y, X, group, iter = 2000, burn_in = 1000, thin = 5, g, mu_0, Lambda_0, eta_0, S_0, nu_0 = 1, a_0 = 1, b_0){
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