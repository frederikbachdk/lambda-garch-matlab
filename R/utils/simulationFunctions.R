ARCH1 <- function(T, x0, omega, alpha){
  
  # THIS FUNCTION SIMULATES A STANDARD ARCH(1) PROCESS
    
    # INPUT: 
      ## T: Number of observations
      ## x0: Initial value
      ## omega: Unconditional variance
      ## alpha: ARCH coefficient
  
  
  # INITIALIZE TIBBLE FOR OUTPUT
  ARCH1 <- tibble(
    index = seq(1,T),
    x  = rep(x0, T), 
    sigma_sq = rep(omega, T),
    z = rnorm(T, 0, 1)
  )
  
  alpha1 <- alpha[1]
  alpha2 <- alpha[2]
  
  # SIMULATE ARCH(1)
  for(i in 3:T){
    ARCH1$sigma_sq[i] <- omega + alpha1 * (ARCH1$x[i-1])^2 + alpha2 * (ARCH1$x[i-2])^2
    ARCH1$x[i] <- sqrt(ARCH1$sigma_sq[i]) * ARCH1$z[i]
  }
  
  return(ARCH1)
}

ARCH1_loglikelihood <- function(theta, y) {
  
  omega = theta[1]
  alpha1 = theta[2]
  alpha2 = theta[3]
  
  # ARCH(1) log-likelihood function   
  N <- length(y) # Number of observations
  logLik <- 0 # Initialize value of log-likelihood
  
  # Evaluate likelihood contributions at parameter values
  for (n in 3:N) {
    s <- omega + alpha1 * y[n-1]^2 + alpha2 * y[n-2]^2 # sigma_t^2
    logLik <- logLik - log(s) - y[n]^2 / s # Cumulative sum
  }
  
  # Remember the scaling
  return(0.5 * logLik)
}

negative_loglik <- function(x) -ARCH1_loglikelihood(theta=x, y = y)
