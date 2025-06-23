# MCMC Samplers

# https://arxiv.org/pdf/2110.10565

X <- hktc_data %>% prep_data_ktc(means, sds)
Y <- hktc_data$ktc_value %>% replace_na(0)
group <- hktc_data$position %>% factor() %>% as.numeric()

# gibbs sampler
ktc_sampler <- function(Y, X, group, niter = 1000, ){
  # initialize
  J <- length(unique(group)) #groups
  p <- ncol(X) #variables
  
  beta <- matrix(0, nrow = niter, ncol = p) # overall mean
  beta_j <- array(0, dim = c(niter, J, p)) # subgroup mean
  zeta <- numeric(niter) #hierarchical variance
  sigma_j <- matrix(0, nrow = niter, ncol = p) #subgroup variance
  
  beta[1,] <- lm(Y ~ X - 1) %>% coef()
  beta_j[1,c(1:4) , ] <- beta[,1] #HERE
}

tva_sampler <- function(X){
  
}