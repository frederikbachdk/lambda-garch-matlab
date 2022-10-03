import_packages <- function(){
  suppressMessages(library(tidyverse))
  suppressMessages(library(matlib))
  suppressMessages(library(tictoc))
  suppressMessages(library(pracma))
}

import_data <- function(estimation_start){
  data <- readxl::read_excel('data/07092022_embig_data.xlsx', sheet = 'Returns') %>%
    mutate(Date = as.Date(Date)) %>%
    select(Date, Africa, Asia, Europe, 'Middle East', 'Latin America') %>%
    filter(Date >= estimation_start)
    return(data)
}

import_theta <- function(){
  theta <- readxl::read_excel('thetahat.xlsx') %>% as.matrix()
  return(theta)
}

calculate_omegas <- function(data, theta, estimation_end){
  
  # subset data
  x_full <- data %>% select(-Date) %>% as.matrix() %>% t()
  x <- data %>% filter(Date <= estimation_end) %>%
    select(-Date) %>% as.matrix() %>% t()
  
  # set parameters
  N <- ncol(x_full)
  p <- n <- nrow(x_full)
  row_end <- which(data$Date == as.Date(estimation_end))
  
  # calculate the conditional eigenvalues in-sample
  loglik <- EigenARCH_loglikelihood_cont(x, theta, n)
  Lambda <- loglik$lambda %>% t()
  colnames(Lambda) <- c('lambda1','lambda2','lambda3','lambda4','lambda5')
  
  # Reparametrization of theta, save in individual matrices
  parameters <- EigenARCH_repar(p, n, theta)
  V <- parameters$eigenvectors
  W <- parameters$omega
  A <- parameters$alpha
  B <- parameters$beta
  
  # retrieve estimated lambdas
  condEigenvals <- data %>% select(Date) %>% 
    filter(Date <= estimation_end) %>% 
    cbind(Lambda %>% round(10))
  
  # estimate lambdas out of sample
  for(t in (row_end+1):(ncol(x_full))){
    
    # set date
    condEigenvals[t,1] <- data[t,1]
    
    # calculate conditional eigenvalues
    lambda_lag <- condEigenvals[t-1,] %>% select(-Date) %>% as.matrix() %>% t()
    lambda_t <- W + A %*% (t(V) %*% x_full[,t-1])^2 + B %*% lambda_lag
    lambda_t <- lambda_t %>% t()
    
    # save in tibble
    condEigenvals[t,2:(2+p-1)] <- lambda_t
  }
  
  # estimate Omegas out of sample
  condCovariances <- lapply(1:ncol(x_full), 
                            function(t) {
                              V %*% diag(condEigenvals[t,2:(2+p-1)]) %*% t(V)
                            }
  ) 
  
  names(condCovariances) <- data$Date
  
  res <- list(condCovariances, condEigenvals)
  names(res) <- c('Omega','Lambda')
  
  return(res)
}

calculate_weights <- function(data, Omegas){
  
  
  
}