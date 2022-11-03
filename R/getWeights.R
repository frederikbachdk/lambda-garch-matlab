# for testing:
library(tidyverse)
condDynamics1 <- readRDS('data/conditionalDynamics1.rds')
Omega <- condDynamics1$condCovar[['2019-01-02']]

data <- readxl::read_excel('data/13102022_data.xlsx', sheet = 'DATA_CLEAN') %>%
  filter(Date >= as.Date('2018-12-01'),
         Date <= as.Date('2019-01-01')) %>%
  select(Africa:'EMBIG Div')

mu <- data %>% select(Africa:'Middle East') %>% colMeans() %>% as.matrix()
mu_bar <- mu + 0.5

rm(condDynamics1, data)


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
  # w (array): array of equal weights
  
  iota <- rep(1, ncol(Omega))
  Omega_inv <- solve(Omega)
  
  w <- (Omega_inv %*% iota) / sum(Omega_inv %*% iota)
  
  return(w)
}

tangentWeights <- function(Omega, mu, mu_bar){
  # function to calculate the efficient portfolio given return target
  # input:
    # Omega  (matrix): Positive definite conditional covariance matrix
    # mu     (array): Conditionally expected return
    # mu_bar (array): Return target
  
  # output:
    # w (array): array of equal weights

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



tangentNTCWeights <- function(Omega, mu, mu_bar, beta = 50) {
  # function to calculate the efficient portfolio given return target
  # input:
  # Omega  (matrix): Positive definite conditional covariance matrix
  # mu     (array): Conditionally expected return
  # mu_bar (array): Return target
  
  # output:
  # w (array): array of equal weights
    
    
  iota <- rep(1, ncol(Omega))
  Omega_inv <- solve(Omega)
  w_init <- (Omega_inv %*% iota) / sum(Omega_inv %*% iota)    # MVP
  
  #w_init <- rep(1 / ncol(Omega), ncol(Omega))                # Equal weights
  
  objective <- function(w) {
    -t(w) %*% mu + gamma / 2 * t(w) %*% Omega %*% w +
      (beta / 10000) / 2 * t(w - w_init) %*% (w - w_init)
  }
  
  w_optimal <- constrOptim.nl(
    par = w_init,
    fn = objective,
    heq = function(w) {
      sum(w) - 1
    },
    control.outer = list(trace = FALSE)
  )
  
  return(w_optimal$par)
}


evaluate_performance <- function(w, w_previous, next_return, beta = 50){
  raw_return <- as.matrix(next_return) %*% w          # w_{t+1} * r_{t+1}
  turnover <- t(w - w_previous) %*% (w - w_previous)  # ||w_{t+1} - w_{t+}||
  net_return <- raw_return - beta / 10000 * turnover  # r_{t+1} - b*turnover
  return(net_return)
}
