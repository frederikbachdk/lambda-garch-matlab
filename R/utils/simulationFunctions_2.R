ARCH <- function(T, x0, theta){
  
  # THIS FUNCTION SIMULATES A STANDARD ARCH(1) PROCESS
    
    # INPUT: 
      ## T: Number of observations
      ## x0: Initial value
      ## theta: Parameter vector (omega, alpha1)
    # OUTPUT:
      ## ARCH: Time series vector of length T
  p <- length(theta) - 1
  omega <- theta[1]
  alpha1 <- theta[2]
  alpha2 <- theta[3]
  alpha3 <- theta[4]
  alpha4 <- theta[5]
  alpha5 <- theta[6]
  
  # INITIALIZE TIBBLE FOR OUTPUT
  ARCH <- tibble(
    index = seq(1,T),
    x  = rep(x0, T), 
    sigma_sq = rep(omega, T),
    z = rnorm(T, 0, 1)
  )
  
  # SIMULATE ARCH(p)
  if(p == 0){
    for(i in 1:T){
      ARCH$sigma_sq[i] <- omega
      ARCH$x[i] <- sqrt(ARCH$sigma_sq[i]) * ARCH$z[i]
    }
  } else if(p == 1){
    for(i in 2:T){
      ARCH$sigma_sq[i] <- omega + 
        alpha1 * (ARCH$x[i-1])^2
      ARCH$x[i] <- sqrt(ARCH$sigma_sq[i]) * ARCH$z[i]
      }
    } else if(p == 2){
      for(i in 3:T){
        ARCH$sigma_sq[i] <- omega + 
          alpha1 * (ARCH$x[i-1])^2 + 
          alpha2 * (ARCH$x[i-2])^2 
        ARCH$x[i] <- sqrt(ARCH$sigma_sq[i]) * ARCH$z[i]
      }
    } else if(p == 3){
    for(i in 4:T){
      ARCH$sigma_sq[i] <- omega + 
        alpha1 * (ARCH$x[i-1])^2 + 
        alpha2 * (ARCH$x[i-2])^2 +
        alpha3 * (ARCH$x[i-3])^2
      ARCH$x[i] <- sqrt(ARCH$sigma_sq[i]) * ARCH$z[i]
    }
  } else if(p == 4){
    for(i in 5:T){
      ARCH$sigma_sq[i] <- omega + 
        alpha1 * (ARCH$x[i-1])^2 + 
        alpha2 * (ARCH$x[i-2])^2 +
        alpha3 * (ARCH$x[i-3])^2 +
        alpha4 * (ARCH$x[i-4])^2
      ARCH$x[i] <- sqrt(ARCH$sigma_sq[i]) * ARCH$z[i]
    }
  } else if(p == 5){
    for(i in 6:T){
      ARCH$sigma_sq[i] <- omega + 
        alpha1 * (ARCH$x[i-1])^2 + 
        alpha2 * (ARCH$x[i-2])^2 +
        alpha3 * (ARCH$x[i-3])^2 +
        alpha4 * (ARCH$x[i-4])^2 +
        alpha5 * (ARCH$x[i-5])^2
      ARCH$x[i] <- sqrt(ARCH$sigma_sq[i]) * ARCH$z[i]
    }
  }
  
  return(ARCH)
}

ARCH1_loglikelihood <- function(theta, y){
  
  p <- length(theta) - 1
  omega <- theta[1]
  alpha1 <- theta[2]
  
  N <- length(y) # Number of observations
  logLik <- 0    # Initialize value of log-likelihood
  
  # Evaluate likelihood contributions at parameter values
  for (n in 2:N) {
      s <- omega + 
        alpha1 * y[n-1]^2
      
      logLik <- logLik - log(s) - y[n]^2 / s
    }
  
  # Scaling
  return(0.5 * logLik)

}

ARCH1_negative_loglik <- function(x) -ARCH1_loglikelihood(theta=x, y = y)


ARCH0_loglikelihood <- function(theta, y){
  
  p <- length(theta) - 1
  omega <- theta[1]
  
  N <- length(y) # Number of observations
  logLik <- 0    # Initialize value of log-likelihood
  
  # Evaluate likelihood contributions at parameter values
  for (n in 1:N) {
    s <- omega
    logLik <- logLik - log(s) - y[n]^2 / s
  }
  
  # Scaling
  return(0.5 * logLik)
  
}

ARCH0_negative_loglik <- function(x) -ARCH0_loglikelihood(theta=x, y = y)

