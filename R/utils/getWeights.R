library(alabama)

equalWeights <- function(n) {
  # function to calculate an equal-weighted portfolio
  # input:
    # n (dbl): number of assets
  
  # output:
    # w (array): array of equal weights
  
  w <- rep(1/n, n) 
  
  return(w)
}


minimumVarWeights <- function(Omega) {
  # function to calculate a minimum variance portfolio
  # input:
    # Omega (matrix): Positive definite covariance matrix
  
  # output:
  # w (array): array of minimum variance weights
  
  iota <- rep(1, ncol(Omega))
  Omega_inv <- solve(Omega)
  
  w <- (Omega_inv %*% iota) / sum(Omega_inv %*% iota)
  
  return(w)
}

efficientWeights <- function(Omega, mu, mu_bar){
  # function to calculate the efficient portfolio given return target
  # input:
    # Omega  (matrix): Positive definite conditional covariance matrix
    # mu     (array): Conditionally expected return
    # mu_bar (array): Return target
  
  # output:
    # w (array): array of efficient weights

  iota <- rep(1,ncol(Omega))         # (N x 1) vector of 1's
  Omega_inv <- solve(Omega)          # Omega inverse
  w_mvp <- minimumVarWeights(Omega)  # minimum variance portfolio
  
  C <- as.numeric(t(iota) %*% Omega_inv %*% iota)
  D <- as.numeric(t(iota) %*% Omega_inv %*% mu)
  E <- as.numeric(t(mu)   %*% Omega_inv %*% mu)
  
  lambda <- as.numeric(2*(mu_bar-D/C)/(E-(D^2)/C))
  
  w_eff <- w_mvp + lambda/2 * (Omega_inv %*% mu - ((D/C) * Omega_inv %*% iota)) 
  w <- w_eff/sum(w_eff)  
  
  return(as.vector(w))
}


efficientWeights_costOpt <- function(Omega, mu, gamma = 2, w_init, beta = 0.05) {
  # function to calculate the efficient portfolio given return target
  # input:
    # Omega  (matrix): Positive definite conditional covariance matrix
    # mu     (array): Conditionally expected return
    # gamma  (dbl): Risk aversion coefficient
    # w_init (array): Weights before rebalancing
    # beta   (dbl): Transaction cost parameter (basis points)
  
  # output:
    # w (array): array of optimal weight allocation
  
  objective <- function(w) {
    -t(w) %*% mu + gamma / 2 * t(w) %*% Omega %*% w + beta / 2 * t(w - w_init) %*% (w - w_init)
  }
  
  w_optimal <- constrOptim.nl(
    par = w_init,
    fn = objective,
    heq = function(w) sum(w) - 1,
    control.outer = list(trace = FALSE)
  )
  
  return(w_optimal$par)
}


evaluate_performance <- function(w, w_prev, next_return, beta = 0.05){
  raw_return <- as.matrix(next_return) %*% w    # w_{t+1} * r_{t+1}
  turnover <- t(w - w_prev) %*% (w - w_prev)  # ||w_{t+1} - w_{t+}||
  net_return <- raw_return - beta * turnover  # r_{t+1} - b*turnover
  return(net_return)
}
