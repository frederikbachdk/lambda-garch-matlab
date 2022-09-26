equalWeights <- function(n) {
  w <- rep(1/n, n)
  return(w)
}

minimumVarWeights <- function(Omega, n) {
  iota <- rep(1,n)
  Omega_inv <- solve(Omega)
  
  w <- (Omega_inv %*% iota) / sum(Omega_inv %*% iota)
  
  return(w)
}