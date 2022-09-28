equalWeights <- function(n) {
  w <- rep(1/n, n)
  return(w)
}


minimumVarWeights <- function(Omega) {
  iota <- rep(1, ncol(Omega))
  Omega_inv <- solve(Omega)
  
  w <- (Omega_inv %*% iota) / sum(Omega_inv %*% iota)
  
  return(w)
}


tangentWeights <- function(Omega, mu, gamma = 4){
  iota <- rep(1, ncol(Omega))
  Omega_inv <- solve(Omega)
  # w_init <- (Omega_inv %*% iota) / sum(Omega_inv %*% iota)
  
  w_init <- rep(1 / ncol(Omega), ncol(Omega))
  
  w_tan <- w_init + 1 / gamma * (Omega_inv - w_init %*% t(iota) %*% Omega_inv) %*% mu
  
  return(as.vector(w_tan))
}


tangentNTCWeights <- function(Omega, mu, gamma = 4, beta = 50) {
  # iota <- rep(1, ncol(Omega))
  # Omega_inv <- solve(Omega)
  # w_mvp <- (Omega_inv %*% iota) / sum(Omega_inv %*% iota)
  
  w_init <- rep(1 / ncol(Omega), ncol(Omega))
  
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
  raw_return <- as.matrix(next_return) %*% w
  turnover <- t(w - w_previous) %*% (w - w_previous)
  net_return <- raw_return - beta / 10000 * turnover
  return(net_return)
}
