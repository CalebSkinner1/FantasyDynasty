# MCMC Samplers

# libraries
library("LaplacesDemon")
library("invgamma")
library("matlib")

# paper that explains
# https://arxiv.org/pdf/2110.10565

X <- hktc_data %>% prep_data_ktc(means, sds)
Y <- hktc_data$ktc_value %>% replace_na(0)
group <- hktc_data$position %>% factor() %>% as.numeric()

# gibbs sampler
ktc_sampler <- function(Y, X, group, niter = 1000, g, mu_0, Lambda_0, eta_0, S_0, nu_0 = 1, a_0 = 1, b_0){
  # initialize
  J <- length(unique(group)) #groups
  p <- ncol(X) #variables
  n <- nrow(X) # observations
  X_group <- map(1:J, ~X[group == .x,]) # X matrix by group
  Y_group <- map(1:J, ~Y[group == .x]) # Y vector by group
  gram <- map(X_group, ~t(.x)%*%.x) # gram matrix
  
  # run ols
  ols <- lm(Y ~ X - 1)
  
  sigma_0 <- summary(ols)$sigma^2
  
  # specify hyperparameters
  if(missing(g)){
    g <- n/J*p
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
  
  # allocate space
  beta <- matrix(0, nrow = niter, ncol = p) # overall mean
  beta_j <- array(0, dim = c(niter, J, p)) # subgroup mean
  zeta <- numeric(niter) # variance of subgroup variance 
  invsigma_j <- matrix(0, nrow = niter, ncol = J) #subgroup variance
  invSigma <- array(0, dim = c(niter, p, p)) # variance of subgroup coef
  
  # initialize
  beta[1,] <- MASS::mvrnorm(1, mu_0, Lambda_0) #overall coef
  invSigma[1, ,] <- rwishart(eta_0, S_0) # variance of subgroup coef
  zeta[1] <- rgamma(1, a_0, b_0) # scale of variance
  beta_j[1,c(1:4) , ] <- MASS::mvrnorm(1, t(beta[1,]), invSigma[1, ,]) # subgroup coef
  invsigma_j[1,] <- rgamma(1, nu_0/2, nu_0*zeta[1]/2)# precision
  
  
  for(i in 2:iter){
    # draw subgroup coefficients
    for(j in 1:J){
      beta_j[i, j,] <- MASS::mvrnorm(1, inv(invSigma[i, ,] + invsigma_j[i-1,j]*gram[[j]])%*%
                                 (invSigma[i-1, , ]%*%beta[i-1,] + invsigma_j[i-1,j]*t(X_group[[j]])%*%Y_group[[j]]),
                               inv(invSigma[i-1, , ] + invsigma_j[i-1,j]*gram[[j]]))
    
    }
    # draw overall coefficients
    beta[i, ] <- MASS::mvrnorm(1, )
    
}

tva_sampler <- function(X){
  
}