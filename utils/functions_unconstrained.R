EigenARCH_optimizer_unconstrained <- function(param, tol = 1e-10, maxfeval = 3*10e4, maxiter = 10e5){
  
  # solution
  solution <- optim(par = theta0, 
                    fn = EigenARCH_loglikelihood_unconstrained,
                    method = 'Nelder-Mead',
                    lower = -Inf,
                    upper = Inf,
                    hessian = TRUE)
  
  return(solution)
}


EigenARCH_loglikelihood_unconstrained <- function(param){
  # log-likelihood function
  
  loglikelihood_fct <- EigenARCH_loglikelihood_cont_unconstrained(x = x, param, n = n)
  L <- mean(loglikelihood_fct$loglike)

  return(-L)
}

EigenARCH_loglikelihood_cont_unconstrained <- function(x, param, n){

  # Log likelihood contributions for the EigenARCH(1,1) moodel
    # Inputs: 
        # x: pxT matrix of asset returns
        # param: vector of initial parameters
        # n: the number of factors (n<=p)   
    # Outputs:
        # L: Log likelihood value
        # sigma2: Array of filtered covariance matrices
        # lambda: pxT matrix of time varying eigenvalues
        # persistence: scalar indicating persistence of the process.
  
  # Define constants
  N = ncol(x);
  p = nrow(x);
  
  # Fetch reparametrized parameter matrices
  parameter_list = EigenARCH_repar_unconstrained(p, n, param) 
  
  V = parameter_list$eigenvectors
  omega = parameter_list$omega %>% as.matrix()
  alpha = parameter_list$alpha %>% as.matrix()
  beta = parameter_list$beta %>% as.matrix()
  
  # Rotated returns  
  y <- t(V) %*% x
  
  #Log likelihood
  loglike         = matrix(0, nrow = 1, ncol = N)  # Vector to hold log-likelihood contributions
  sigma2          = list();                        # list to contain time-varying covariance matrices
  lambda          = matrix(0, nrow = p, ncol = N); # lambda (vector), contains time-varying eigenvalues
  lambda[,1]      = omega;
   
  for(i in 2:N){ 
    
    # conditional eigenvalues
    lambda[,i] = omega+alpha%*%y[,i-1]^2 + beta%*%lambda[,i-1]            
    
    # conditional covariance matrix
    sigma2[[i]] = V %*% diag(lambda[,i]) %*% t(V) # based on the spectral decomposition
    
    # log-likelihood contribution
    loglike[i] = -p/2*log(2*pi) - 1/2*sum(log(lambda[,i])) - 1/2*t(y[,i]) %*% diag(1/lambda[,i]) %*% y[,i];
  }
  
  #persistence = max(eigen(alpha+beta)$values); # persistence of stochastic process
  
  log_likelihood <- list(loglike = loglike, 
                         sigma2 = sigma2, 
                         lambda = lambda)
                         #persistence = persistence)
  
  return(log_likelihood)
}

EigenARCH_repar_unconstrained <- function(p, n, param){
  # Function to reparameterize the parameter-vector to the matrices
  
  count <- 1
  
  # Reparametrized eigenvectors
  phi <- param[count:(count + p*(p-1)/2-1)]
  phi <- exp(phi)/(1+exp(phi))*pi/2# reparametrized phi
  V   <- rotation(phi,p) # Rotation matrix
  count <- count+p*(p-1)/2
  
  # Reparametrized constant
  omega <- exp(param[count:(count+p-1)])
  count <- count+p
  
  # Reduced rank matrices
  a <- matrix(param[count:(count+p*n-1)], ncol=n, byrow = TRUE)^2
  count <- count + p*n
  
  if(p == n){
    g = diag(1,n)
  } else{
    g <- matrix(0,p,n)
    g[1:(p-n),] <- matrix(param[count:(count+(p-n)*n-1)], ncol=n, byrow = TRUE)^2 # FIRST ROW FREE
    g[(p-n+1):end,] <- diag(n,n)
    count <- count+(p-n)*n     
  }
  
  b <- matrix(param[count:(count+p*n-1)],ncol=n,nrow=p, byrow = TRUE)^2
  count <- count+p*n;
  
  alpha <- g %*% t(a); 
  beta  <- g %*% t(b); 
  
  repar <- list(eigenvectors = V, 
                omega = omega, 
                alpha = alpha, 
                beta = beta)
  
  return(repar)
}

